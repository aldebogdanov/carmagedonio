(ns carmageddon.shared.worldgen
  "Infinite world generation. Pure, seeded, and shared by client and server.

  Every feature derives from (world-seed, integer coordinates) and never from
  generation order, so chunks can be produced in any order, on any thread, on
  any machine, and still agree. That is what lets clients stream terrain with
  zero network traffic and lets the server reason about a place it has never
  simulated.

  The street network is a single jittered lattice at `street-spacing`, with the
  road hierarchy falling out of divisibility: every eighth lattice line is an
  arterial, every fourth a collector, the rest local streets. Arterials always
  exist, which is what guarantees the network is connected everywhere without
  any global reasoning; the smaller classes appear in proportion to how built-up
  the place is, so a city is a dense grid and open country is a few lanes
  512 m apart. Nodes are displaced anisotropically -- freely along a street,
  barely across an arterial -- so downtown stays rectilinear while lanes wander.

  Nothing here needs neighbours to agree about anything, because there is
  nothing to agree about: a lattice node is a function of its own two integers,
  and an edge is a function of the canonical seed of its two endpoints. Two
  chunks that overlap the same street compute the same street."
  (:require [carmageddon.shared.constants :as k]
            [carmageddon.shared.noise :as noise]
            [carmageddon.shared.prng :as prng]))

;; Portable maths. Clojure's Math/* interop and CLJS's js/Math.* are not the
;; same forms, and typed arrays are written differently on each platform.

(defn- sqrt  [x] #?(:clj (Math/sqrt x)  :cljs (js/Math.sqrt x)))
(defn- floor [x] #?(:clj (Math/floor x) :cljs (js/Math.floor x)))
(defn- hypot [x y] (sqrt (+ (* x x) (* y y))))
(defn- js-sin [x] #?(:clj (Math/sin x) :cljs (js/Math.sin x)))
(defn- js-cos [x] #?(:clj (Math/cos x) :cljs (js/Math.cos x)))

(defn- smoothstep [t] (* t t (- 3.0 (* 2.0 t))))

(defn- smootherstep-clamped [t]
  (let [t (max 0.0 (min 1.0 t))]
    (* t t (- 3.0 (* 2.0 t)))))

(defn- farray  [n] #?(:clj (float-array n)  :cljs (js/Float32Array. n)))
(defn- darray  [n] #?(:clj (double-array n) :cljs (js/Float64Array. n)))
(defn- iarray  [n] #?(:clj (int-array n)    :cljs (js/Int32Array. n)))
(defn- fput! [a i v] #?(:clj (aset ^floats a i (float v))   :cljs (aset a i v)))
(defn- dput! [a i v] #?(:clj (aset ^doubles a i (double v)) :cljs (aset a i v)))
(defn- dget  [a i]   #?(:clj (aget ^doubles a i)            :cljs (aget a i)))
(defn- iput! [a i v] #?(:clj (aset ^ints a i (int v))       :cljs (aset a i v)))
(defn- iget  [a i]   #?(:clj (aget ^ints a i)               :cljs (aget a i)))
(defn- dlen  [a]     #?(:clj (alength ^doubles a)           :cljs (.-length a)))

(defn- grid-floor
  "Lattice index containing `v`, rounding toward negative infinity so the
  lattice is uniform through the origin rather than mirrored about it."
  [v spacing]
  (long (floor (/ v spacing))))

;; --- terrain shape ----------------------------------------------------------

(def terrain-scale 520.0)   ; metres per major undulation
(def terrain-amp    34.0)   ; peak-to-trough metres

;; Four octaves at a low gain rather than five at 0.5. The extra octaves add
;; detail with a ~25 m wavelength and roughly a metre of amplitude, which is
;; invisible from a distance but launches a car at speed -- measured at 45% of
;; ticks airborne before this was tuned down.
(def ^:private terrain-octaves 4)
(def ^:private terrain-gain 0.42)

(defn base-height
  "Terrain before roads are cut into it."
  [seed x z]
  (* terrain-amp (- (noise/fbm2d seed (/ x terrain-scale) (/ z terrain-scale)
                                 terrain-octaves terrain-gain 2.0)
                    0.5)))

;; --- how built-up a place is ------------------------------------------------

(def ^:private city-threshold 0.56)
(def ^:private city-scale 6.0)      ; chunks per blob, so ~1.5 km of city

(defn urbanness
  "How built-up a *point* is, in [0,1]. Continuous in world space.

  This is the field the whole settlement pattern hangs off: it decides which
  streets exist, how much the lattice wanders, how wide the verges are and how
  the ground is tinted. Being continuous is the point -- a per-chunk decision
  would draw a seam along every chunk boundary where city meets country, in the
  road layout as well as in the colour."
  [seed x z]
  (let [d (* city-scale k/chunk-size)
        n (noise/fbm2d (+ seed 5501) (/ x d) (/ z d) 3)
        t (/ (- n (- city-threshold 0.09)) 0.18)]
    (smootherstep-clamped t)))

(defn biome
  "Coarse label for a whole chunk: is this a built-up place or open country?

  Sampled at the chunk centre from the same continuous field the streets use, so
  it can never disagree with what actually got generated."
  [seed cx cz]
  (if (> (urbanness seed
                    (* (+ cx 0.5) k/chunk-size)
                    (* (+ cz 0.5) k/chunk-size))
         0.5)
    :city
    :country))

;; --- street lattice ---------------------------------------------------------

(def street-spacing 64.0)     ; metres between lattice lines: one city block
(def ^:private arterial-every 8)   ; -> 512 m
(def ^:private collector-every 4)  ; -> 256 m

(defn- line-class
  "Class of the lattice line with index `i`.

  Divisibility gives the whole road hierarchy for free, and gives it globally:
  no chunk has to be told where the main roads are, it can see that line 24 is
  a multiple of eight."
  [i]
  (cond (zero? (mod i arterial-every))  :arterial
        (zero? (mod i collector-every)) :collector
        :else                           :local))

(def road-profile
  "Per-class carriageway half-width and verge width.

  The verges are far narrower than the 18 m the hub-and-spoke generator used.
  They have to be: with streets 64 m apart, an 18 m blend on each side of each
  road would leave no unroaded ground between them at all."
  {:arterial  {:half 7.0 :shoulder 8.0}
   :collector {:half 5.0 :shoulder 5.5}
   :local     {:half 3.8 :shoulder 4.0}})

(def signal-cycle 24.0)   ; seconds for a full traffic-light cycle
(def signal-green   9.5)
(def signal-amber   2.5)  ; green + amber is exactly half a cycle

(def road-max-influence
  "Furthest a road can affect the ground, over every class and every verge
  multiplier. Chunks gather streets within this distance of their bounds, so if
  a profile ever grows past it, roads start being clipped at chunk borders."
  24.0)

(defn- verge
  "Verge width for a class at a given urbanness. Country roads blend back into
  the landscape over a much wider band than a kerbed city street."
  [cls u]
  (* (:shoulder (road-profile cls)) (+ 1.0 (* 0.9 (- 1.0 u)))))

(def ^:private node-wobble 8.5)   ; metres, only in open country

(defn- line-offset
  "How far lattice line `i` sits from its nominal position.

  Displacement is per *line*, not per node, which is the difference between a
  city of straight streets on an irregular grid and a city of streets that lean.
  Blocks come out between about 40 and 90 m instead of a uniform 64, which is
  what an old street plan looks like; every street in it is still straight.

  Arterials move least, because a main road that steps sideways at every block
  is not a main road."
  [seed i axis]
  (let [amp (case (line-class i) :arterial 2.5 :collector 7.0 :local 13.0)
        r   (prng/make (prng/hash-coords (+ seed (if (zero? axis) 3301 7717)) i 0))]
    (prng/next-range! r (- amp) amp)))

(defn node
  "World position of lattice node (gx, gz).

  On top of the per-line offset a node may wobble, but only on an axis whose
  line is a local street, and only out in the country. Moving a node in X bends
  the vertical line through it, so letting an arterial node wobble would kink a
  road meant to run for kilometres -- and letting a city node wobble would leave
  every block a trapezoid with no square corner to put a building against."
  [seed gx gz]
  (let [bx (+ (* gx street-spacing) (line-offset seed gx 0))
        bz (+ (* gz street-spacing) (line-offset seed gz 1))
        u  (urbanness seed bx bz)
        rural (- 1.0 u)
        ax (if (= :local (line-class gx)) (* node-wobble rural) 0.0)
        az (if (= :local (line-class gz)) (* node-wobble rural) 0.0)
        r  (prng/chunk-rng seed gx gz (:roads k/salt))]
    [(+ bx (prng/next-range! r (- ax) ax))
     (+ bz (prng/next-range! r (- az) az))]))

(defn- edge-class
  "An edge running along X belongs to the horizontal line `gz`; one running
  along Z belongs to the vertical line `gx`."
  [gx gz along-x?]
  (line-class (if along-x? gz gx)))

(defn- edge-exists?
  "Does the street from (gx,gz) to its +X or +Z neighbour exist?

  Arterials always do -- that single rule is what makes the network connected
  everywhere, forever, with no global pass. Below that it is a weighted coin
  drawn from the canonical seed of the two endpoints, so both sides of any chunk
  border flip it the same way."
  [seed gx gz along-x?]
  (let [cls (edge-class gx gz along-x?)]
    (if (= :arterial cls)
      true
      (let [mx (* street-spacing (if along-x? (+ gx 0.5) gx))
            mz (* street-spacing (if along-x? gz (+ gz 0.5)))
            u  (urbanness seed mx mz)
            p  (case cls
                 :collector (+ 0.15 (* 0.75 u))
                 (max 0.0 (- (* 1.3 u) 0.45)))
            hx (if along-x? (inc gx) gx)
            hz (if along-x? gz (inc gz))
            r  (prng/make (prng/edge-seed seed gx gz hx hz))]
        (< (prng/next-double! r) p)))))

(def ^:private bend-samples 5)

(defn- bezier
  "Quadratic curve from a to b, bowed sideways. Country roads follow the land
  instead of ruling a line across it; city streets do not bend."
  [[ax az] [bx bz] bow n]
  (let [mx (* 0.5 (+ ax bx))
        mz (* 0.5 (+ az bz))
        dx (- bx ax) dz (- bz az)
        len (max 1e-6 (hypot dx dz))
        px (* (- dz) (/ bow len))
        pz (* dx (/ bow len))
        cx (+ mx px) cz (+ mz pz)]
    (mapv (fn [i]
            (let [t (/ (double i) (dec n))
                  u (- 1.0 t)]
              [(+ (* u u ax) (* 2 u t cx) (* t t bx))
               (+ (* u u az) (* 2 u t cz) (* t t bz))]))
          (range n))))

(def ^:private bend-threshold 0.4)   ; urbanness above which streets are straight

(defn- street
  "One street of the lattice, as a polyline plus its road profile.

  Endpoint heights come from the *unroaded* terrain at the two nodes and are
  interpolated along the polyline, which is what a real road does -- cut and
  fill to a steady grade rather than following every bump. Because both nodes
  are shared with the neighbouring streets, junctions cannot disagree about how
  high the ground is."
  [seed gx gz along-x?]
  (let [hx (if along-x? (inc gx) gx)
        hz (if along-x? gz (inc gz))
        a  (node seed gx gz)
        b  (node seed hx hz)
        cls (edge-class gx gz along-x?)
        u  (urbanness seed (* 0.5 (+ (nth a 0) (nth b 0)))
                           (* 0.5 (+ (nth a 1) (nth b 1))))
        bow (if (< u bend-threshold)
              (let [r (prng/make (prng/edge-seed (+ seed 7717) gx gz hx hz))]
                (* (- 1.0 (/ u bend-threshold))
                   (prng/next-range! r -15.0 15.0)))
              0.0)]
    {:points   (if (zero? bow) [a b] (bezier a b bow bend-samples))
     :half     (:half (road-profile cls))
     :shoulder (verge cls u)
     :class    cls
     :ya       (base-height seed (nth a 0) (nth a 1))
     :yb       (base-height seed (nth b 0) (nth b 1))}))

(defn streets-in-bounds
  "Every street that could reach the box [x0,x1] x [z0,z1].

  One lattice cell of slack on each side covers the largest possible node
  displacement, so no street that touches the box can be missed."
  [seed x0 z0 x1 z1]
  (let [gx0 (dec (grid-floor x0 street-spacing))
        gx1 (inc (grid-floor x1 street-spacing))
        gz0 (dec (grid-floor z0 street-spacing))
        gz1 (inc (grid-floor z1 street-spacing))
        out (transient [])]
    (doseq [gx (range gx0 (inc gx1))
            gz (range gz0 (inc gz1))]
      (when (edge-exists? seed gx gz true)
        (conj! out (street seed gx gz true)))
      (when (edge-exists? seed gx gz false)
        (conj! out (street seed gx gz false))))
    (persistent! out)))

;; --- road field: the streets near one chunk, indexed for lookup -------------

(def ^:private seg-stride 8)   ; x1 z1 y1 x2 z2 y2 half shoulder

(defn- segments-of
  "Flatten streets into [x1 z1 y1 x2 z2 y2 half shoulder ...].

  Flat and typed because `road-at` runs about a thousand times per chunk against
  this array; boxed vector access shows up plainly in a profile."
  [streets]
  (let [out (transient [])]
    (doseq [{:keys [points half shoulder ya yb]} streets]
      (let [n (count points)]
        (dotimes [i (dec n)]
          (let [[x1 z1] (nth points i)
                [x2 z2] (nth points (inc i))
                t1 (/ (double i) (dec n))
                t2 (/ (double (inc i)) (dec n))]
            (conj! out (double x1)) (conj! out (double z1))
            (conj! out (+ ya (* (- yb ya) t1)))
            (conj! out (double x2)) (conj! out (double z2))
            (conj! out (+ ya (* (- yb ya) t2)))
            (conj! out (double half)) (conj! out (double shoulder))))))
    (let [v (persistent! out)
          a (darray (count v))]
      (dotimes [i (count v)] (dput! a i (nth v i)))
      a)))

(defn- index-segments
  "Bucket segments into a uniform grid, in CSR form: `starts` gives each cell's
  slice of `items`.

  Each segment is inserted into every cell within `road-max-influence` of it, so
  a lookup only ever has to read the one cell containing the query point. That
  turns what used to be a linear scan over every segment in the neighbourhood --
  the hot loop of the whole generator -- into a scan of about five."
  [segs x0 z0 x1 z1]
  (let [cell (double road-max-influence)
        nx   (max 1 (long (inc (floor (/ (- x1 x0) cell)))))
        nz   (max 1 (long (inc (floor (/ (- z1 z0) cell)))))
        cells (* nx nz)
        n    (long (/ (dlen segs) seg-stride))
        counts (iarray (inc cells))
        ;; Two passes: count per cell, prefix-sum into starts, then fill.
        span (fn [i]
               (let [o  (* i seg-stride)
                     ax (dget segs o) az (dget segs (inc o))
                     bx (dget segs (+ o 3)) bz (dget segs (+ o 4))
                     pad (+ (dget segs (+ o 6)) (dget segs (+ o 7)))
                     lo-x (- (min ax bx) pad) hi-x (+ (max ax bx) pad)
                     lo-z (- (min az bz) pad) hi-z (+ (max az bz) pad)]
                 [(max 0 (min (dec nx) (long (floor (/ (- lo-x x0) cell)))))
                  (max 0 (min (dec nx) (long (floor (/ (- hi-x x0) cell)))))
                  (max 0 (min (dec nz) (long (floor (/ (- lo-z z0) cell)))))
                  (max 0 (min (dec nz) (long (floor (/ (- hi-z z0) cell)))))]))]
    (dotimes [i n]
      (let [[ix0 ix1 iz0 iz1] (span i)]
        (doseq [ix (range ix0 (inc ix1)), iz (range iz0 (inc iz1))]
          (let [c (inc (+ (* ix nz) iz))]
            (iput! counts c (inc (iget counts c)))))))
    (dotimes [c cells]
      (iput! counts (inc c) (+ (iget counts (inc c)) (iget counts c))))
    (let [starts (iarray (inc cells))
          cursor (iarray cells)
          items  (iarray (iget counts cells))]
      (dotimes [c (inc cells)] (iput! starts c (iget counts c)))
      (dotimes [i n]
        (let [[ix0 ix1 iz0 iz1] (span i)]
          (doseq [ix (range ix0 (inc ix1)), iz (range iz0 (inc iz1))]
            (let [c (+ (* ix nz) iz)
                  w (+ (iget starts c) (iget cursor c))]
              (iput! items w i)
              (iput! cursor c (inc (iget cursor c)))))))
      {:starts starts :items items :nx nx :nz nz :cell cell :x0 x0 :z0 z0})))

(defn road-field
  "Every street near chunk (cx, cz), flattened and indexed.

  The box is the chunk grown by `road-max-influence`, which is exactly the set
  of streets that can affect the ground anywhere inside it -- and, crucially,
  exactly the set the *neighbouring* chunk gathers for any point on their shared
  border. That is why the terrain has no step at a chunk seam."
  [seed cx cz]
  (let [pad (+ road-max-influence 2.0)
        x0  (- (* cx k/chunk-size) pad)
        z0  (- (* cz k/chunk-size) pad)
        x1  (+ (* (inc cx) k/chunk-size) pad)
        z1  (+ (* (inc cz) k/chunk-size) pad)
        segs (segments-of (streets-in-bounds seed x0 z0 x1 z1))]
    (assoc (index-segments segs x0 z0 x1 z1) :segs segs)))

(defn- road-at
  "How road-like a point is, in [0,1], and the road surface height there.

  Roads are combined rather than picked between: `roadness` is the strongest
  single influence, but the height is a weighted blend of every road nearby.
  Taking the height of whichever road happens to be strongest puts a step in the
  ground wherever the winner changes, which at a junction is right where a car
  is about to drive."
  [{:keys [segs starts items nx nz cell x0 z0]} x z]
  (let [ix (max 0 (min (dec nx) (long (floor (/ (- x x0) cell)))))
        iz (max 0 (min (dec nz) (long (floor (/ (- z z0) cell)))))
        c  (+ (* ix nz) iz)
        e  (iget starts (inc c))]
    (loop [i (iget starts c), best 0.0, wsum 0.0, wy 0.0]
      (if (>= i e)
        (if (pos? wsum) [best (/ wy wsum)] [0.0 0.0])
        (let [o  (* seg-stride (iget items i))
              x1 (dget segs o)       z1 (dget segs (+ o 1)) y1 (dget segs (+ o 2))
              x2 (dget segs (+ o 3)) z2 (dget segs (+ o 4)) y2 (dget segs (+ o 5))
              half (dget segs (+ o 6)) shoulder (dget segs (+ o 7))
              dx (- x2 x1) dz (- z2 z1)
              len2 (+ (* dx dx) (* dz dz))
              t (if (< len2 1e-9)
                  0.0
                  (max 0.0 (min 1.0 (/ (+ (* (- x x1) dx) (* (- z z1) dz)) len2))))
              px (+ x1 (* t dx)) pz (+ z1 (* t dz))
              d  (hypot (- x px) (- z pz))
              r  (cond (<= d half) 1.0
                       (>= d (+ half shoulder)) 0.0
                       :else (- 1.0 (smoothstep (/ (- d half) shoulder))))]
          (if (pos? r)
            (recur (inc i) (max best r) (+ wsum r)
                   (+ wy (* r (+ y1 (* t (- y2 y1))))))
            (recur (inc i) best wsum wy)))))))

(defn surface
  "Final height at a point plus how road-like it is, in [0,1].

  `field` comes from `road-field`; it is passed in rather than rebuilt because a
  chunk evaluates this about a thousand times."
  [seed field x z]
  (let [[road ry] (road-at field x z)]
    (cond
      (>= road 1.0) [ry 1.0]
      (<= road 0.0) [(base-height seed x z) 0.0]
      :else (let [b (base-height seed x z)]
              [(+ ry (* (- b ry) (- 1.0 road))) road]))))

(defn chunk-of [x z]
  [(grid-floor x k/chunk-size) (grid-floor z k/chunk-size)])

(defn height-at
  "Convenience for one-off queries (spawning, placing objects). Rebuilds the
  chunk's road field, so do not call it in a loop."
  [seed x z]
  (let [[cx cz] (chunk-of x z)]
    (first (surface seed (road-field seed cx cz) x z))))

(defn chunk-lines
  "The streets this chunk owns, for placing things along.

  Ownership is by the midpoint of the street, so every street belongs to exactly
  one chunk however the lattice is displaced, and two neighbours can never
  furnish the same road twice."
  [seed cx cz]
  (let [x0 (* cx k/chunk-size) z0 (* cz k/chunk-size)
        x1 (+ x0 k/chunk-size) z1 (+ z0 k/chunk-size)]
    (filterv (fn [{:keys [points]}]
               (let [[ax az] (first points)
                     [bx bz] (peek points)
                     mx (* 0.5 (+ ax bx))
                     mz (* 0.5 (+ az bz))]
                 (and (<= x0 mx) (< mx x1) (<= z0 mz) (< mz z1))))
             (streets-in-bounds seed x0 z0 x1 z1))))

;; --- props ------------------------------------------------------------------

(def prop-kinds
  "Smashable roadside clutter. Shared so the server can reason about what a
  chunk contains without rendering it."
  [{:name :crate  :half [0.60 0.60 0.60] :density 40.0 :colour 0xc9a86a}
   {:name :barrel :half [0.45 0.75 0.45] :density 55.0 :colour 0x8a6a3a}
   {:name :sign   :half [0.12 1.10 0.80] :density 26.0 :colour 0xa8a49c}])

(def props-per-chunk 14)
(def prop-stride 6)          ; x y z yaw kind scale

(defn chunk-props
  "Deterministic prop placement for one chunk, as a flat array of
  [x y z yaw kind scale ...].

  Flat and typed because this crosses the Worker boundary as a transferable; a
  vector of maps would have to be serialised for every chunk.

  Props are placed *along* the streets this chunk owns and pushed sideways clear
  of the carriageway, rather than scattered over the chunk and rejected when
  they miss. Scattering wasted three quarters of every batch, because the
  roadside band is about 12 m wide out of 256."
  [seed cx cz field]
  (let [lines (chunk-lines seed cx cz)]
    (if (empty? lines)
      (farray 0)
      (let [r   (prng/chunk-rng seed cx cz (:props k/salt))
            out (transient [])]
        (dotimes [_ props-per-chunk]
          (let [line  (nth lines (prng/next-int! r (count lines)))
                pts   (:points line)
                n     (count pts)
                i     (prng/next-int! r (dec n))
                t     (prng/next-double! r)
                side  (if (prng/next-bool! r) 1.0 -1.0)
                off   (+ (:half line) (prng/next-range! r 0.8 6.5))
                kind  (prng/next-int! r (count prop-kinds))
                yaw   (prng/next-range! r 0.0 6.2831853)
                scale (prng/next-range! r 0.8 1.35)
                [ax az] (nth pts i)
                [bx bz] (nth pts (inc i))
                px (+ ax (* t (- bx ax)))
                pz (+ az (* t (- bz az)))
                dx (- bx ax) dz (- bz az)
                len (max 1e-6 (hypot dx dz))
                ;; perpendicular to the road at this point
                ux (* (- dz) (/ side len))
                uz (* dx (/ side len))
                ;; Perpendicular to *this* street, but streets meet at junctions,
                ;; so stepping clear of one often lands on another. Try the other
                ;; side before trying further out: with streets 64 m apart,
                ;; pushing outward walks into the next carriageway, which is how
                ;; the old escalating ladder ended up putting one prop in six on
                ;; a road. The ladder is fixed, not drawn, so it costs no extra
                ;; randomness and stays identical on every machine.
                [x z] (loop [ms [[1.0 1.0] [-1.0 1.0] [1.0 1.55] [-1.0 1.55]
                                 [1.0 2.1] [-1.0 2.1]]]
                        (let [[sgn mul] (first ms)
                              cx' (+ px (* ux off mul sgn))
                              cz' (+ pz (* uz off mul sgn))]
                          (if (or (empty? (rest ms))
                                  (<= (second (surface seed field cx' cz')) 0.85))
                            [cx' cz']
                            (recur (rest ms)))))
                [y _] (surface seed field x z)]
            (conj! out x) (conj! out y) (conj! out z)
            (conj! out yaw) (conj! out (double kind)) (conj! out scale)))
        (let [v (persistent! out)
              a (farray (count v))]
          (dotimes [i (count v)] (fput! a i (nth v i)))
          a)))))

;; --- blocks, lots and zoning ------------------------------------------------

(defn industrialness
  "A second field, independent of `urbanness`, that says where the works are.

  Industry is not simply 'less city': it clusters, and it clusters at the edge
  of a town rather than in the middle of it. Giving it its own field is what
  stops factories being scattered through the housing."
  [seed x z]
  (let [d (* 3.5 k/chunk-size)]
    (noise/fbm2d (+ seed 8123) (/ x d) (/ z d) 3)))

(def ^:private lot-setback 2.4)    ; pavement between kerb and plot boundary
(def ^:private open-setback 0.8)   ; where a block side has no street at all
(def ^:private lot-depth 16.0)     ; how far back a street-fronting plot runs
(def ^:private min-lot 7.0)

;; Which side of a cell a plot fronts, and which way that makes it face. Yaw is
;; measured so local +Z points *away* from the street, into the block -- so a
;; building's front is its local -Z face and always looks at the road.
(def ^:private side-yaw
  {:north 0.0
   :south #?(:clj Math/PI :cljs js/Math.PI)
   :west  #?(:clj (/ Math/PI 2) :cljs (/ js/Math.PI 2))
   :east  #?(:clj (- (/ Math/PI 2)) :cljs (- (/ js/Math.PI 2)))})

(defn- cell-side
  "The street running along one side of lattice cell (gx, gz), or nil."
  [seed gx gz side]
  (let [[ex ez along-x? line] (case side
                                :north [gx gz true (line-class gz)]
                                :south [gx (inc gz) true (line-class (inc gz))]
                                :west  [gx gz false (line-class gx)]
                                :east  [(inc gx) gz false (line-class (inc gx))])]
    (when (edge-exists? seed ex ez along-x?)
      {:class line :half (:half (road-profile line))})))

(defn cell-interior
  "The buildable rectangle inside lattice cell (gx, gz).

  Each side is pulled back from whichever of the cell's own corner nodes lies
  further in, so the plot boundary clears the street however the lattice has
  been displaced. A side with no street on it is barely inset at all: that is
  where two cells have merged into one larger block, and the ground should run
  straight through."
  [seed gx gz]
  (let [[ax az] (node seed gx gz)
        [bx bz] (node seed (inc gx) gz)
        [cx' cz'] (node seed (inc gx) (inc gz))
        [dx dz] (node seed gx (inc gz))
        pull (fn [side] (if-let [{:keys [half]} (cell-side seed gx gz side)]
                          (+ half lot-setback)
                          open-setback))
        x0 (+ (max ax dx) (pull :west))
        x1 (- (min bx cx') (pull :east))
        z0 (+ (max az bz) (pull :north))
        z1 (- (min dz cz') (pull :south))]
    (when (and (> (- x1 x0) (* 2 min-lot)) (> (- z1 z0) (* 2 min-lot)))
      {:x0 x0 :x1 x1 :z0 z0 :z1 z1
       :sides (into {} (for [sd [:north :south :west :east]
                             :let [v (cell-side seed gx gz sd)]
                             :when v]
                         [sd v]))})))

(defn- strip-lots
  "Cut a run of frontage into plots.

  Interior boundaries are drawn once and shared by the plots on either side, so
  neighbours meet exactly rather than overlapping or leaving a sliver."
  [r a0 a1 frontage]
  (let [len (- a1 a0)
        n   (max 1 (long (+ 0.5 (/ len frontage))))
        step (/ len n)
        bounds (vec (concat [a0]
                            (for [i (range 1 n)]
                              (+ a0 (* i step) (prng/next-range! r -1.4 1.4)))
                            [a1]))]
    (mapv (fn [i] [(nth bounds i) (nth bounds (inc i))]) (range n))))

(def building-zones
  "What can stand on a plot. `cover` is the share of the plot the footprint
  takes and `height` its range in metres. The index into this vector is what
  travels in the buildings array, so appending is safe and reordering is not."
  [{:name :house     :cover 0.50 :height [4.5 7.0]}
   {:name :townhouse :cover 0.80 :height [7.0 11.0]}
   {:name :apartment :cover 0.74 :height [13.0 27.0]}
   {:name :shop      :cover 0.90 :height [5.0 9.5]}
   {:name :office    :cover 0.84 :height [22.0 58.0]}
   {:name :factory   :cover 0.82 :height [9.0 16.0]}
   {:name :warehouse :cover 0.88 :height [7.0 11.0]}
   {:name :civic     :cover 0.62 :height [10.0 19.0]}
   {:name :barn      :cover 0.45 :height [5.0 8.5]}])

(def zone-index (zipmap (map :name building-zones) (range)))

(defn- pick-zone
  "What gets built on a plot.

  Frontage matters as much as density: the same block has shops on the corner
  and on the main road and housing down the side street, which is most of what
  makes a city read as a city rather than as one repeated building. `:open`
  leaves the plot empty -- a yard, a car park, a gap."
  [r u ind front-class corner?]
  (let [main? (contains? #{:arterial :collector} front-class)
        p (prng/next-double! r)]
    (cond
      ;; Works sit at the edge of a town, not in the middle of one, so this
      ;; wants a band of density rather than a floor: without the upper bound a
      ;; quarter of downtown came out as warehousing.
      (and (> ind 0.68) (< 0.22 u 0.74))
      (if (< p 0.55) :factory :warehouse)

      (> u 0.80)
      (if main?
        (cond (< p 0.40) :office (< p 0.72) :shop (< p 0.92) :apartment
              (< p 0.97) :civic :else :open)
        (cond (< p 0.22) :shop (< p 0.80) :apartment (< p 0.88) :office
              (< p 0.94) :civic :else :open))

      (> u 0.58)
      (if (or main? corner?)
        (cond (< p 0.48) :shop (< p 0.74) :apartment (< p 0.92) :townhouse
              (< p 0.97) :civic :else :open)
        (cond (< p 0.12) :shop (< p 0.46) :apartment (< p 0.88) :townhouse
              (< p 0.95) :civic :else :open))

      (> u 0.34)
      (if main?
        (cond (< p 0.26) :shop (< p 0.60) :townhouse (< p 0.90) :house :else :open)
        (cond (< p 0.06) :shop (< p 0.36) :townhouse (< p 0.88) :house :else :open))

      (> u 0.15)
      (cond (< p 0.52) :house
            (< p 0.68) :barn
            :else :open)

      :else (if (< p 0.16) :barn :open))))

(defn- frontage-for [u ind]
  (cond (> ind 0.68) 28.0
        (> u 0.72)   13.0
        (> u 0.45)   11.5
        :else        20.0))

(defn cell-lots
  "Plots cut from one lattice cell, as a ring around its perimeter.

  A block's plots front the streets around it and back onto each other, which is
  how a block actually works; whatever is left in the middle is a yard and gets
  nothing. Cutting a ring is also far more robust than trying to tile the
  interior, because it degrades gracefully when the block is a strange shape."
  [seed gx gz]
  (if-let [{:keys [x0 x1 z0 z1 sides]} (cell-interior seed gx gz)]
    (let [r    (prng/chunk-rng seed gx gz (:blocks k/salt))
          u    (urbanness seed (* 0.5 (+ x0 x1)) (* 0.5 (+ z0 z1)))
          ind  (industrialness seed (* 0.5 (+ x0 x1)) (* 0.5 (+ z0 z1)))
          fr   (frontage-for u ind)
          dz   (min lot-depth (* 0.42 (- z1 z0)))
          dx   (min lot-depth (* 0.42 (- x1 x0)))
          ;; North and south take the full width; east and west take what is
          ;; left between them, so the four corners are not claimed twice.
          iz0  (+ z0 (if (:north sides) dz 0.0))
          iz1  (- z1 (if (:south sides) dz 0.0))
          strip (fn [side a0 a1 b0 b1 horizontal?]
                  (when-let [{:keys [class]} (get sides side)]
                    (when (> (- a1 a0) min-lot)
                      (let [runs (strip-lots r a0 a1 fr)
                            last-i (dec (count runs))]
                        (map-indexed
                         (fn [i [s e]]
                           (let [[lx0 lx1 lz0 lz1] (if horizontal? [s e b0 b1] [b0 b1 s e])]
                             {:x (* 0.5 (+ lx0 lx1)) :z (* 0.5 (+ lz0 lz1))
                              :hx (* 0.5 (- lx1 lx0)) :hz (* 0.5 (- lz1 lz0))
                              :yaw (side-yaw side)
                              :front class
                              :corner? (or (zero? i) (= i last-i))}))
                         runs)))))]
      (into []
            (comp cat
                  (filter (fn [{:keys [hx hz]}] (and (> hx 2.0) (> hz 2.0))))
                  (map (fn [{:keys [x z corner? front] :as lot}]
                         (assoc lot :zone (pick-zone r
                                                     (urbanness seed x z)
                                                     (industrialness seed x z)
                                                     front corner?)))))
            [(strip :north x0 x1 z0 (+ z0 dz) true)
             (strip :south x0 x1 (- z1 dz) z1 true)
             (strip :west iz0 iz1 x0 (+ x0 dx) false)
             (strip :east iz0 iz1 (- x1 dx) x1 false)]))
    []))

(defn chunk-lots
  "The plots this chunk owns -- those whose centre lands inside it."
  [seed cx cz]
  (let [x0 (* cx k/chunk-size) z0 (* cz k/chunk-size)
        x1 (+ x0 k/chunk-size) z1 (+ z0 k/chunk-size)
        gx0 (dec (grid-floor x0 street-spacing))
        gx1 (inc (grid-floor x1 street-spacing))
        gz0 (dec (grid-floor z0 street-spacing))
        gz1 (inc (grid-floor z1 street-spacing))]
    (vec (for [gx (range gx0 (inc gx1))
               gz (range gz0 (inc gz1))
               lot (cell-lots seed gx gz)
               :when (and (<= x0 (:x lot)) (< (:x lot) x1)
                          (<= z0 (:z lot)) (< (:z lot) z1))]
           lot))))

;; --- buildings --------------------------------------------------------------

(def building-stride 8)      ; x y z hx hz height zone yaw

(defn chunk-buildings
  "One building per built plot, as [x y z hx hz height zone yaw ...].

  Buildings are no longer thrown at the chunk and rejected when they land on a
  road. They stand on plots cut from the block, square to the street, pushed up
  against their frontage with the yard behind -- which is what turns a field of
  boxes into a street.

  Ground height is the *lowest* of the four corners, so a building on a slope
  cuts into the hill instead of floating over the downhill side."
  [seed cx cz field]
  (let [r   (prng/chunk-rng seed cx cz (+ 17 (:blocks k/salt)))
        out (transient [])]
    (doseq [{:keys [x z hx hz yaw zone]} (chunk-lots seed cx cz)
            :when (not= :open zone)]
      (let [{:keys [cover height]} (nth building-zones (zone-index zone))
            u  (urbanness seed x z)
            [h0 h1] height
            ;; Density drives height within the zone's range: the same kind of
            ;; block is taller downtown than on the edge of town.
            hgt (* (prng/next-range! r h0 h1) (+ 0.72 (* 0.38 u)))
            bhx (max 2.0 (* hx cover))
            bhz (max 2.0 (* hz cover))
            ;; Sit against the frontage rather than in the middle of the plot;
            ;; the leftover depth becomes the yard behind.
            back (* 0.55 (- hz bhz))
            [sx sz] [(js-sin yaw) (js-cos yaw)]
            bx (- x (* sx back))
            bz (- z (* sz back))
            corners [[bx bz]
                     [(- bx bhx) (- bz bhz)] [(+ bx bhx) (- bz bhz)]
                     [(- bx bhx) (+ bz bhz)] [(+ bx bhx) (+ bz bhz)]]
            y (reduce min (map (fn [[px pz]] (first (surface seed field px pz))) corners))]
        (conj! out bx) (conj! out y) (conj! out bz)
        (conj! out bhx) (conj! out bhz) (conj! out hgt)
        (conj! out (double (zone-index zone))) (conj! out yaw)))
    (let [v (persistent! out)
          a (farray (count v))]
      (dotimes [i (count v)] (fput! a i (nth v i)))
      a)))

;; --- pedestrians ------------------------------------------------------------

(def ped-stride 5)           ; x y z heading speed

(defn peds-per-chunk
  "Cities are busy, countryside is not."
  [seed cx cz]
  (if (= :city (biome seed cx cz)) 18 5))

(defn chunk-peds
  "Deterministic pedestrian spawns, as [x y z heading speed ...].

  Placed close to the carriageway -- they are meant to be in the way. Same
  draw-before-you-reject discipline as props so the stream advances identically
  regardless of what the terrain turns out to be."
  [seed cx cz field]
  (let [lines (chunk-lines seed cx cz)]
    (if (empty? lines)
      (farray 0)
      (let [r   (prng/chunk-rng seed cx cz (:peds k/salt))
            n   (peds-per-chunk seed cx cz)
            out (transient [])]
        (dotimes [_ n]
          (let [line  (nth lines (prng/next-int! r (count lines)))
                pts   (:points line)
                cnt   (count pts)
                i     (prng/next-int! r (dec cnt))
                t     (prng/next-double! r)
                side  (if (prng/next-bool! r) 1.0 -1.0)
                off   (prng/next-range! r 1.0 (+ (:half line) 3.0))
                head  (prng/next-range! r 0.0 6.2831853)
                speed (prng/next-range! r 0.7 1.9)
                [ax az] (nth pts i)
                [bx bz] (nth pts (inc i))
                px (+ ax (* t (- bx ax)))
                pz (+ az (* t (- bz az)))
                dx (- bx ax) dz (- bz az)
                len (max 1e-6 (hypot dx dz))
                x (+ px (* side (* (- dz) (/ off len))))
                z (+ pz (* side (* dx (/ off len))))
                [y _] (surface seed field x z)]
            (conj! out x) (conj! out y) (conj! out z)
            (conj! out head) (conj! out speed)))
        (let [v (persistent! out)
              a (farray (count v))]
          (dotimes [i (count v)] (fput! a i (nth v i)))
          a)))))


;; --- junctions and street furniture ----------------------------------------

(def ^:private class-rank {:local 0 :collector 1 :arterial 2})

(defn- node-arms
  "The streets meeting at lattice node (gx, gz), each as a unit direction away
  from the node and the class of that street.

  A node has four possible arms and every one of them is a lattice edge whose
  existence both neighbours already agree on, so the degree of a junction is
  computable from the node's own coordinates. That is the whole reason the
  furniture can be placed without any global pass over the network."
  [seed gx gz]
  (let [[nx nz] (node seed gx gz)]
    (into []
          (keep (fn [[dgx dgz along-x? ogx ogz]]
                  (when (edge-exists? seed ogx ogz along-x?)
                    (let [[ox oz] (node seed (+ gx dgx) (+ gz dgz))
                          dx (- ox nx) dz (- oz nz)
                          len (max 1e-6 (hypot dx dz))]
                      {:dir [(/ dx len) (/ dz len)]
                       :class (edge-class ogx ogz along-x?)}))))
          [[-1 0 true (dec gx) gz]
           [1  0 true gx gz]
           [0 -1 false gx (dec gz)]
           [0  1 false gx gz]])))

(defn junction
  "What sort of junction sits at lattice node (gx, gz), or nil if it is not one.

  Two arms is a bend or a continuation, not a junction, and gets nothing. Three
  or more is controlled: with signals where a main road meets a busy grid, and
  otherwise by priority, which means signs on the minor approaches only. A
  residential crossroads where every arm is the same class gets nothing at all,
  which is both cheaper and what such a junction actually looks like."
  [seed gx gz]
  (let [arms (node-arms seed gx gz)
        n    (count arms)]
    (when (>= n 3)
      (let [[x z] (node seed gx gz)
            u    (urbanness seed x z)
            ranks (map (comp class-rank :class) arms)
            top   (apply max ranks)
            minors (filter #(< (class-rank (:class %)) top) arms)
            ;; Signals are decided per *axis*, not per arm. Every node along an
            ;; arterial has two arterial arms -- the road running through it --
            ;; so asking whether any arm is arterial puts a set of lights every
            ;; 64 m. What matters is what the road being crossed by is: an
            ;; arterial meeting a collector is a signalled junction, an arterial
            ;; meeting a side street is a give-way.
            axis-rank (fn [along-x?]
                        (let [r (for [{:keys [dir class]} arms
                                      :let [[dx dz] dir]
                                      :when (= along-x? (> (abs dx) (abs dz)))]
                                  (class-rank class))]
                          (if (seq r) (apply max r) -1)))
            ax (axis-rank true)
            az (axis-rank false)
            major (max ax az)
            cross (min ax az)
            signals? (and (> u 0.45)
                          (or (and (= major 2) (>= cross 1))
                              (and (= n 4) (>= cross 1) (> u 0.7))))]
        {:pos [x z] :arms arms :degree n :top top :minors (vec minors)
         :half (apply max (map #(:half (road-profile (:class %))) arms))
         ;; Cycles are offset per junction so a city does not blink in unison.
         :offset (prng/next-range! (prng/chunk-rng seed gx gz 91) 0.0 signal-cycle)
         :kind (cond signals?      :signals
                     (seq minors)  :priority
                     :else         :uncontrolled)}))))

(defn chunk-junctions
  "The junctions this chunk owns -- those whose node lands inside it."
  [seed cx cz]
  (let [x0 (* cx k/chunk-size) z0 (* cz k/chunk-size)
        x1 (+ x0 k/chunk-size) z1 (+ z0 k/chunk-size)
        gx0 (dec (grid-floor x0 street-spacing))
        gx1 (inc (grid-floor x1 street-spacing))
        gz0 (dec (grid-floor z0 street-spacing))
        gz1 (inc (grid-floor z1 street-spacing))]
    (vec (for [gx (range gx0 (inc gx1))
               gz (range gz0 (inc gz1))
               :let [j (junction seed gx gz)]
               :when j
               :let [[jx jz] (:pos j)]
               :when (and (<= x0 jx) (< jx x1) (<= z0 jz) (< jz z1))]
           j))))

;; Furniture is emitted as *parts*, not as objects: a traffic light is a pole
;; instance plus a head instance. The client then needs one instanced draw per
;; part rather than one mesh per lamp post, and a signal head can take a colour
;; of its own without every pole in the city changing with it.
(def furniture-stride 8)     ; x y z yaw part size phase offset
(def furniture-parts [:pole :lamp-head :signal-head :sign-face :marking])
(def sign-types [:stop :give-way])

(def ^:private lamp-height 6.4)
(def ^:private mast-height 5.2)
(def ^:private sign-height 2.2)
(def ^:private signal-head-y 4.3)
(def ^:private lamp-spacing 26.0)
(def ^:private lamp-urbanness 0.32)
(def ^:private crossing-stripes 5)
(def ^:private centreline-spacing 9.0)

(defn signal-state
  "Colour a signal group shows at world time `t`.

  Pure, and a function of world time rather than of anything a client owns, so
  every machine in a session sees the same lights without a byte crossing the
  network -- and the server can say what a light was showing at a moment it
  never simulated. Green plus amber is exactly half the cycle, so opposite
  groups can never both be moving."
  [t offset phase]
  (let [u (mod (+ t offset (* phase 0.5 signal-cycle)) signal-cycle)]
    (cond (< u signal-green) :green
          (< u (+ signal-green signal-amber)) :amber
          :else :red)))

(defn- emit! [out x y z yaw part size phase offset]
  (conj! out x) (conj! out y) (conj! out z) (conj! out yaw)
  (conj! out (double part)) (conj! out size)
  (conj! out (double phase)) (conj! out offset))

(def ^:private part-index (zipmap furniture-parts (range)))

(defn chunk-furniture
  "Street furniture for one chunk, as a flat array of
  [x y z yaw part size phase offset ...].

  Everything here is derived from the street graph rather than scattered: masts
  stand on the corners of junctions that have signals, signs face the approaches
  that have to give way, lamps march along streets at a fixed spacing. That is
  what makes a road read as a road rather than as a strip of dark ground -- and
  it costs almost nothing, because the graph already knows the topology."
  [seed cx cz field]
  (let [out (transient [])
        ground (fn [x z] (first (surface seed field x z)))]
    ;; Junction furniture.
    (doseq [{:keys [pos arms kind half offset] :as j} (chunk-junctions seed cx cz)]
      (let [[jx jz] pos
            setback (+ half 3.2)]
        (case kind
          :signals
          (doseq [{:keys [dir class]} arms]
            (let [[dx dz] dir
                  ah (:half (road-profile class))
                  ;; Corner of the junction: back down the approach, then out to
                  ;; the far side of that approach's carriageway.
                  rx (- dz) rz dx
                  px (+ jx (* dx setback) (* rx (+ ah 1.9)))
                  pz (+ jz (* dz setback) (* rz (+ ah 1.9)))
                  y  (ground px pz)
                  ;; Facing back down the approach, at the driver.
                  yaw (#?(:clj Math/atan2 :cljs js/Math.atan2) (- dx) (- dz))
                  ;; Opposite arms share a group, so the two axes alternate.
                  phase (if (> (abs dx) (abs dz)) 0 1)]
              (emit! out px y pz yaw (part-index :pole) mast-height phase offset)
              (emit! out (- px (* rx 0.5)) (+ y signal-head-y) (- pz (* rz 0.5))
                     yaw (part-index :signal-head) 1.0 phase offset)
              ;; A crossing across this approach, just outside the junction.
              (dotimes [i crossing-stripes]
                (let [t (- (/ (double i) (dec crossing-stripes)) 0.5)
                      sx (+ jx (* dx (+ half 1.6)) (* rx t 2.0 ah 0.86))
                      sz (+ jz (* dz (+ half 1.6)) (* rz t 2.0 ah 0.86))]
                  (emit! out sx (+ 0.02 (ground sx sz)) sz
                         (#?(:clj Math/atan2 :cljs js/Math.atan2) dx dz)
                         (part-index :marking) 1.0 0 0.0)))))

          :priority
          (doseq [{:keys [dir class]} (:minors j)]
            (let [[dx dz] dir
                  ah (:half (road-profile class))
                  rx (- dz) rz dx
                  px (+ jx (* dx (+ half 2.4)) (* rx (+ ah 1.1)))
                  pz (+ jz (* dz (+ half 2.4)) (* rz (+ ah 1.1)))
                  y  (ground px pz)
                  yaw (#?(:clj Math/atan2 :cljs js/Math.atan2) (- dx) (- dz))
                  ;; Give way where the junction still has a through route,
                  ;; stop where it does not.
                  st (if (= 4 (:degree j)) 1 0)]
              (emit! out px y pz yaw (part-index :pole) sign-height 0 0.0)
              (emit! out px (+ y sign-height 0.1) pz yaw
                     (part-index :sign-face) 1.0 st 0.0)))

          nil)))
    ;; Lamp posts and centre lines along the streets this chunk owns.
    (doseq [{:keys [points half class]} (chunk-lines seed cx cz)]
      (let [[ax az] (first points)
            [bx bz] (peek points)
            dx (- bx ax) dz (- bz az)
            len (hypot dx dz)
            u   (urbanness seed (* 0.5 (+ ax bx)) (* 0.5 (+ az bz)))]
        ;; A painted centre line is the cheapest thing in the whole generator
        ;; and does more for reading a road as a road than anything else here.
        ;; Local streets get none, which is also true of most real ones.
        (when (and (not= :local class) (> len 1.0))
          (let [ux (/ dx len) uz (/ dz len)
                n  (long (floor (/ len centreline-spacing)))
                yaw (#?(:clj Math/atan2 :cljs js/Math.atan2) ux uz)]
            (dotimes [i n]
              (let [t (* (+ i 0.5) centreline-spacing)
                    px (+ ax (* ux t))
                    pz (+ az (* uz t))]
                (emit! out px (+ 0.02 (ground px pz)) pz yaw
                       (part-index :marking) 1.0 0 0.0)))))
        (when (and (> u lamp-urbanness) (> len 1.0))
          (let [ux (/ dx len) uz (/ dz len)
                rx (- uz) rz ux
                n  (max 1 (long (floor (/ len lamp-spacing))))
                off (+ half 1.9)
                lift (if (= class :arterial) 1.0 0.85)]
            (dotimes [i n]
              ;; Alternate sides, and skip the very ends so lamps do not pile up
              ;; on top of the junction furniture at either node.
              (let [t    (/ (+ i 0.5) (double n))
                    side (if (even? i) 1.0 -1.0)
                    px (+ ax (* ux len t) (* rx off side))
                    pz (+ az (* uz len t) (* rz off side))
                    y  (ground px pz)
                    h  (* lamp-height lift)
                    yaw (#?(:clj Math/atan2 :cljs js/Math.atan2) (* rx (- side)) (* rz (- side)))]
                (emit! out px y pz yaw (part-index :pole) h 0 0.0)
                ;; The luminaire hangs out over the carriageway.
                (emit! out (- px (* rx side 0.9)) (+ y h -0.15) (- pz (* rz side 0.9))
                       yaw (part-index :lamp-head) 1.0 0 0.0)))))))
    (let [v (persistent! out)
          a (farray (count v))]
      (dotimes [i (count v)] (fput! a i (nth v i)))
      a)))

;; --- chunk assembly ---------------------------------------------------------

;; Vertex colour multiplies the tiled ground texture, which is green. Simply
;; darkening it for roads gives dark grass, not asphalt -- so the road colour has
;; to actively cancel the texture's hue (green down, red and blue up) to land on
;; neutral grey.
(def ^:private road-colour-r 0.42)
(def ^:private road-colour-g 0.27)
(def ^:private road-colour-b 0.52)

(defn chunk-data
  "Everything needed to build one chunk's mesh and collider.

  `heights` is indexed `x * verts + z` -- the x index selects the row and z
  varies fastest. That is what Rapier's heightfield expects, established by
  raycasting a real collider and comparing against this function rather than by
  reading parry's source; the alternative orderings were wrong by up to 7 m,
  which still looks like plausible terrain but leaves the car floating.

  The mesh builder uses the same indexing, so the two cannot disagree about
  which way round the world is.

  Pure and allocation-contained, so this runs unchanged inside a Web Worker."
  [seed cx cz]
  (let [n     k/chunk-verts
        cells (dec n)
        step  (/ k/chunk-size cells)
        x0    (* cx k/chunk-size)
        z0    (* cz k/chunk-size)
        field (road-field seed cx cz)
        props (chunk-props seed cx cz field)
        buildings (chunk-buildings seed cx cz field)
        peds  (chunk-peds seed cx cz field)
        furniture (chunk-furniture seed cx cz field)
        heights (farray (* n n))
        colors  (farray (* n n 3))]
    (dotimes [j n]                       ; j indexes z
      (dotimes [i n]                     ; i indexes x
        (let [x (+ x0 (* i step))
              z (+ z0 (* j step))
              [y road] (surface seed field x z)
              idx (+ (* i n) j)
              dirt (noise/fbm2d (+ seed 977) (/ x 26.0) (/ z 26.0) 3)
              d    (* 0.55 (max 0.0 (- dirt 0.45)))
              gr   (+ 0.80 (* 0.30 dirt))
              tr   (* gr (+ 1.0 (* 0.55 d)))
              tg   (* gr (- 1.0 (* 0.05 d)))
              tb   (* gr (- 1.0 (* 0.40 d)))
              ;; Cities stand on concrete, not lawn. Same hue-cancelling trick
              ;; as the roads. Pale, not grey: the road colour is dark asphalt,
              ;; and if urban ground lands on the same value the streets vanish
              ;; into the pavement and a city reads as one flat slab.
              u    (urbanness seed x z)
              ;; Pavement has to sit clearly *above* the asphalt in value. With
              ;; streets 64 m apart most urban ground is near a road, and at the
              ;; old spacing the two tints were close enough that a city read as
              ;; one flat grey slab with buildings standing on it.
              tr   (+ tr (* u (- (* gr 1.34) tr)))
              tg   (+ tg (* u (- (* gr 0.98) tg)))
              tb   (+ tb (* u (- (* gr 1.46) tb)))
              o    (* idx 3)]
          (fput! heights idx y)
          (fput! colors (+ o 0) (+ tr (* road (- road-colour-r tr))))
          (fput! colors (+ o 1) (+ tg (* road (- road-colour-g tg))))
          (fput! colors (+ o 2) (+ tb (* road (- road-colour-b tb)))))))
    {:cx cx :cz cz :verts n :size k/chunk-size
     :origin [x0 z0]
     :heights heights
     :colors colors
     :props props
     :buildings buildings
     :peds peds
     :furniture furniture
     :biome (biome seed cx cz)}))

(defn spawn-point
  "Somewhere on the street network near the origin, and which way that street
  runs: {:pos [x y z] :dir [dx dz]}.

  The direction matters as much as the point. Opponents have to line up along
  the carriageway, because since buildings started standing on real plots a ring
  of cars around the spawn puts most of them inside one.

  Takes a street rather than a lattice node: a node can be a dead end in sparse
  country, whereas the middle of a street that exists is by definition on a
  road. Chunks are searched outward because a wilderness chunk may own none."
  [seed]
  (let [rings (for [d (range 0 8), cx (range (- d) (inc d)), cz (range (- d) (inc d))
                    :when (= d (max (abs cx) (abs cz)))]
                [cx cz])
        [cx cz] (or (first (filter (fn [[cx cz]] (seq (chunk-lines seed cx cz))) rings))
                    [0 0])
        pts  (:points (first (chunk-lines seed cx cz)))
        i0   (max 0 (dec (quot (count pts) 2)))
        i1   (min (dec (count pts)) (inc i0))
        [ax az] (nth pts i0)
        [bx bz] (nth pts i1)
        x  (* 0.5 (+ ax bx))
        z  (* 0.5 (+ az bz))
        len (max 1e-6 (hypot (- bx ax) (- bz az)))]
    {:pos [x (+ 1.2 (height-at seed x z)) z]
     :dir [(/ (- bx ax) len) (/ (- bz az) len)]}))
