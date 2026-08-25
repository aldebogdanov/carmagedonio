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
  "Per-class carriageway half-width, verge width and lattice jitter.

  `jitter` is a fraction of `street-spacing` and is deliberately tiny for
  arterials: a node's displacement bends the line it sits on, so letting an
  arterial node wander would kink a road that is supposed to run for kilometres.

  The verges are far narrower than the 18 m the hub-and-spoke generator used.
  They have to be: with streets 64 m apart, an 18 m blend on each side of each
  road would leave no unroaded ground between them at all."
  {:arterial  {:half 7.0 :shoulder 8.0 :jitter 0.03}
   :collector {:half 5.0 :shoulder 5.5 :jitter 0.10}
   :local     {:half 3.8 :shoulder 4.0 :jitter 0.20}})

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

(defn node
  "World position of lattice node (gx, gz).

  The displacement is anisotropic. Moving a node in X bends the vertical line it
  sits on, so the X amplitude is governed by that line's class, and likewise for
  Z. A node where two arterials cross barely moves at all; a purely local node
  moves freely. Open country wobbles more than downtown does."
  [seed gx gz]
  (let [bx (* gx street-spacing)
        bz (* gz street-spacing)
        u  (urbanness seed bx bz)
        wob (+ 0.35 (* 0.65 (- 1.0 u)))
        ax (* street-spacing wob (:jitter (road-profile (line-class gx))))
        az (* street-spacing wob (:jitter (road-profile (line-class gz))))
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

;; --- buildings --------------------------------------------------------------

(def building-stride 7)      ; x y z hx hz height kind
(def building-kinds 4)
(def ^:private building-attempts 48)

(defn chunk-buildings
  "City blocks, as a flat array of [x y z hx hz height kind ...]. Empty outside
  city chunks.

  A candidate is rejected if its centre or any corner is anywhere near a street,
  which is cheaper and far more robust than computing the blocks between streets
  analytically -- it keeps working when the road layout changes. W3 replaces
  this with real lots cut from the faces of the street graph.

  Ground height is the *lowest* of the four corners, so a building on a slope
  cuts into the hill instead of floating over the downhill side."
  [seed cx cz field]
  (if (not= :city (biome seed cx cz))
    (farray 0)
    (let [r   (prng/chunk-rng seed cx cz (:blocks k/salt))
          out (transient [])]
      (dotimes [_ building-attempts]
        (let [x  (* (+ cx (prng/next-range! r 0.06 0.94)) k/chunk-size)
              z  (* (+ cz (prng/next-range! r 0.06 0.94)) k/chunk-size)
              hx (prng/next-range! r 4.0 10.0)
              hz (prng/next-range! r 4.0 10.0)
              h  (prng/next-range! r 9.0 42.0)
              kind (prng/next-int! r building-kinds)
              corners [[x z] [(- x hx) (- z hz)] [(+ x hx) (- z hz)]
                       [(- x hx) (+ z hz)] [(+ x hx) (+ z hz)]]
              samples (mapv (fn [[px pz]] (surface seed field px pz)) corners)
              clear?  (every? (fn [[_ road]] (< road 0.12)) samples)]
          (when clear?
            (let [y (reduce min (map first samples))]
              (conj! out x) (conj! out y) (conj! out z)
              (conj! out hx) (conj! out hz) (conj! out h)
              (conj! out (double kind))))))
      (let [v (persistent! out)
            a (farray (count v))]
        (dotimes [i (count v)] (fput! a i (nth v i)))
        a))))

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
     :biome (biome seed cx cz)}))

(defn spawn-point
  "Somewhere on the street network, near the origin.

  Takes an actual street rather than a lattice node: a node can be a dead end in
  sparse country, whereas the midpoint of a street that exists is by definition
  on a road. Chunks are searched outward because a wilderness chunk may own no
  street at all."
  [seed]
  (let [rings (for [d (range 0 8), cx (range (- d) (inc d)), cz (range (- d) (inc d))
                    :when (= d (max (abs cx) (abs cz)))]
                [cx cz])
        [cx cz] (or (first (filter (fn [[cx cz]] (seq (chunk-lines seed cx cz))) rings))
                    [0 0])
        line (first (chunk-lines seed cx cz))
        pts  (:points line)
        [x z] (nth pts (quot (count pts) 2))]
    [x (+ 1.2 (height-at seed x z)) z]))
