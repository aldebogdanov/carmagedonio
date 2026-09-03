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
(defn- js-atan2 [y x] #?(:clj (Math/atan2 y x) :cljs (js/Math.atan2 y x)))
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
(defn- fget  [a i]   #?(:clj (aget ^floats a i)             :cljs (aget a i)))
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

(def river-scale 1500.0)   ; metres per meander
(def river-depth  11.0)    ; how far a channel is cut below the land
(def ^:private river-half  14.0)   ; metres of channel either side of the line
(def ^:private river-bank  12.0)   ; and how far the banks slope back up
(def ^:private river-wet   0.44)   ; wetness below which a region has no rivers
(def ^:private river-probe  6.0)   ; finite-difference step, metres

(defn river
  "How much river is at a point, in [0,1]: 1 in the channel, 0 on dry land.

  A river here is the zero-set of a noise field rather than a simulated flow.
  Taking the contour where fbm crosses its midpoint gives long meandering
  channels that any chunk can evaluate alone, with no upstream to consult --
  which is the whole requirement. They close on themselves rather than reaching
  a sea, which is the price of that and is not visible from a car.

  The distance to that contour is measured in *metres*, not in noise units, by
  dividing by the field's local gradient. Skipping that step is what makes
  contour rivers useless: where the field happens to be flat the band spreads
  out enormously, and a quarter of the world came out as riverbed.

  Rivers are also masked by a much coarser wetness field, so they run in some
  regions and not others rather than tiling the world with loops. The mask is
  two octaves against the channel's three and is checked first, because most of
  the world is dry and never pays for the rest."
  [seed x z]
  (let [wet (noise/fbm2d (+ seed 9911) (/ x (* 4.0 river-scale))
                         (/ z (* 4.0 river-scale)) 2)]
    (if (< wet river-wet)
      0.0
      (let [f (fn [px pz] (noise/fbm2d (+ seed 4409) (/ px river-scale)
                                       (/ pz river-scale) 3))
            n (f x z)
            d (abs (- n 0.5))]
        ;; Far from the contour in noise units it cannot be a river whatever the
        ;; gradient is, so the two extra samples are only paid for near water.
        (if (> d 0.25)
          0.0
          (let [gx (- (f (+ x river-probe) z) n)
                gz (- (f x (+ z river-probe)) n)
                g  (max 1e-9 (hypot gx gz))
                metres (* river-probe (/ d g))
                m (smootherstep-clamped (/ (- wet river-wet) 0.10))]
            (* m (- 1.0 (smootherstep-clamped
                         (/ (- metres river-half) river-bank))))))))))

(defn base-height
  "Terrain before roads are cut into it, with river channels carved out of it.

  The carve lives here rather than alongside the road cut on purpose: a river is
  part of the landscape, so roads have to deal with it, and dealing with it is
  what makes a bridge.

  The three-argument form takes an already-computed river amount. A caller that
  needs the water for its own reasons -- tinting the ground, say -- would
  otherwise evaluate the field twice per vertex, and the river field is the
  most expensive thing in the generator."
  ([seed x z] (base-height seed x z (river seed x z)))
  ([seed x z rv]
   (- (* terrain-amp (- (noise/fbm2d seed (/ x terrain-scale) (/ z terrain-scale)
                                     terrain-octaves terrain-gain 2.0)
                        0.5))
      (* river-depth rv))))

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

(def ^:private expressway-every 32)   ; lattice lines, so ~2 km apart
(def ^:private expressway-lift
  "How high an expressway rides over the surface grid. The two axes ride at
  different heights so that where two of them cross you get a stack rather than
  two decks fighting over the same piece of air."
  {:x 8.5 :z 15.0})
(def ^:private expressway-floor 1.0)   ; below this a deck is not worth building

(defn- expressway-line?
  "Is lattice line `i` an expressway? Every thirty-second line, which is a
  multiple of eight and therefore already an arterial -- an expressway is a
  main road that has been taken off the ground, not a new kind of road."
  [i]
  (zero? (mod i expressway-every)))

(defn node-lift
  "How far above the terrain an expressway sits at lattice node (gx, gz), for a
  street running along the given axis. Zero everywhere else.

  The lift follows density rather than switching on: an expressway climbs as it
  approaches a city and comes back down as it leaves, so the ramps are simply
  the streets where the lift happens to be part way up. That keeps the whole
  thing local -- every node works out its own height from the urbanness under
  it, and consecutive nodes agree without consulting each other -- and it
  spreads the climb over however many blocks the city edge takes, instead of
  putting a 13% gradient on one 64 m street."
  [seed gx gz along-x?]
  (let [line (if along-x? gz gx)]
    (if-not (expressway-line? line)
      0.0
      (let [[x z] (node seed gx gz)
            ;; Spread across nearly the whole of `urbanness`, not a narrow band
            ;; inside it. That field is already a steepened remap of the noise
            ;; and swings 0 to 1 in a couple of blocks; ramping over a slice of
            ;; it as well put the entire 8.5 m climb on one 64 m street, at 15%.
            t (smootherstep-clamped (/ (- (urbanness seed x z) 0.12) 0.70))
            l (* t (if along-x? (:x expressway-lift) (:z expressway-lift)))]
        (if (< l expressway-floor) 0.0 l)))))

(defn- edge-class
  "An edge running along X belongs to the horizontal line `gz`; one running
  along Z belongs to the vertical line `gx`."
  [gx gz along-x?]
  (line-class (if along-x? gz gx)))

(defn- run-of
  "Which stretch of a line node index `pos` belongs to, where a stretch runs
  between two consecutive crossings of a line one class higher."
  [pos span]
  (long (floor (/ (double pos) span))))

(defn- edge-exists?
  "Does the street from (gx,gz) to its +X or +Z neighbour exist?

  Arterials always do -- that single rule is what makes the network connected
  everywhere, forever, with no global pass.

  Below that the coin is flipped once per *run* rather than once per edge, and
  that is the whole of road continuity. A run is the entire stretch of one line
  between two crossings of a higher class: a collector runs arterial to
  arterial, a local street collector to collector. Every edge in a run asks the
  same question and gets the same answer, so a street either goes the whole way
  or is not there -- and where it does stop, it stops at a junction with a
  bigger road, which is the only place a road ending does not look like a bug.

  Per-edge coins are what this replaced, and they were the reason the map was
  full of stubs: a 15% chance in open country does not make a country lane, it
  makes sixty-four metres of tarmac between two fields. Roads now cross a
  region rather than dotting it, and they carry on into the next one for the
  same reason they carried on into this one -- nothing about a run knows where
  the chunk borders are."
  [seed gx gz along-x?]
  (let [cls (edge-class gx gz along-x?)]
    (if (= :arterial cls)
      true
      (let [;; The line this edge belongs to, and how far along it the edge is.
            line (if along-x? gz gx)
            pos  (if along-x? gx gz)
            span (if (= :collector cls) arterial-every collector-every)
            run  (run-of pos span)
            ;; Sampled at the middle of the run, on nominal lattice
            ;; coordinates: every edge in the run has to ask the same question
            ;; of the same place, and a per-line offset in here would let a run
            ;; disagree with itself.
            mid  (* street-spacing (+ (* run span) (* 0.5 span)))
            mx   (if along-x? mid (* street-spacing line))
            mz   (if along-x? (* street-spacing line) mid)
            u    (urbanness seed mx mz)
            ;; Held close to what the per-edge odds came to in a city, so the
            ;; downtown grid is as dense as it was. What changed is the country:
            ;; local lanes went from impossible to rare, and collectors from a
            ;; scattering of stubs to a road every few hundred metres.
            p    (case cls
                   :collector (+ 0.34 (* 0.55 u))
                   (+ 0.04 (* 0.78 u)))
            ;; Keyed on the line and the run, not the endpoints. The axis has
            ;; to be in the key: horizontal line 5 and vertical line 5 are two
            ;; different roads and must not share a coin.
            r    (prng/make (prng/hash-coords (+ seed 4409)
                                              (+ (* 2 line) (if along-x? 0 1))
                                              run))]
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

(def ^:private bridge-clearance 3.5)   ; metres of air under the chord
(def ^:private bridge-samples 6)

(defn- spans-a-gap?
  "Does the straight line between a street's two ends run well clear of the
  ground somewhere in between?

  This is the whole of bridge detection, and it deliberately does not mention
  rivers: a road spans a dry ravine for exactly the same reason it spans a
  river, and asking the terrain rather than the water field means one rule
  covers both."
  [seed [ax az] [bx bz] ya yb]
  (loop [i 1]
    (if (>= i bridge-samples)
      false
      (let [t (/ (double i) bridge-samples)
            x (+ ax (* t (- bx ax)))
            z (+ az (* t (- bz az)))
            chord (+ ya (* t (- yb ya)))]
        (if (> (- chord (base-height seed x z)) bridge-clearance)
          true
          (recur (inc i)))))))

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
        ;; A meander, not a zigzag.
        ;;
        ;; This was a per-edge coin between -15 and 15 m, which meant every
        ;; consecutive sixty-four metres of a country road bulged the opposite
        ;; way from the last one. Over a kilometre that is not a road that
        ;; wanders, it is a serpentine -- and on an arterial, which is supposed
        ;; to be the thing you can see continuing into the next valley, it was
        ;; the main reason a main road did not read as one.
        ;;
        ;; The bow is now a slow sine along the line, so consecutive segments
        ;; lean the same way and the road turns through its bends over about
        ;; nine blocks. Amplitude is per class: a lane can wander, a main road
        ;; barely does.
        bow (if (< u bend-threshold)
              (let [amp  (case cls :arterial 4.0 :collector 9.0 :local 15.0)
                    i    (if along-x? gx gz)
                    line (if along-x? gz gx)
                    ph   (prng/next-range!
                          (prng/make (prng/hash-coords (+ seed 7717)
                                                       (+ (* 2 line) (if along-x? 0 1))
                                                       0))
                          0.0 6.283185307179586)]
                (* (- 1.0 (/ u bend-threshold))
                   amp
                   (js-sin (+ ph (* 0.7 i)))))
              0.0)]
    (let [lift-a (node-lift seed gx gz along-x?)
          lift-b (node-lift seed hx hz along-x?)
          ya (+ (base-height seed (nth a 0) (nth a 1)) lift-a)
          yb (+ (base-height seed (nth b 0) (nth b 1)) lift-b)]
      {:points   (if (zero? bow) [a b] (bezier a b bow bend-samples))
       ;; Its own identity in the lattice. Recovering this from the polyline
       ;; does not work: a node is displaced by up to 13 m, so an endpoint can
       ;; sit in the neighbouring cell entirely.
       :gx       gx
       :gz       gz
       :along-x? along-x?
       :half     (:half (road-profile cls))
       :shoulder (verge cls u)
       :class    cls
       :ya       ya
       :yb       yb
       :lift-a   lift-a
       :lift-b   lift-b
       ;; A bridge is a street that has stopped touching the ground, whether
       ;; because the ground fell away under it or because it was lifted off.
       ;; Either way it is excluded from the terrain cut -- so the valley stays
       ;; a valley and the street below stays a street -- and gets a deck.
       :bridge?  (or (pos? lift-a) (pos? lift-b)
                     (spans-a-gap? seed a b ya yb))})))

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

(def ^:private seg-stride 9)   ; x1 z1 y1 x2 z2 y2 half shoulder paved

(defn- segments-of
  "Flatten streets into [x1 z1 y1 x2 z2 y2 half shoulder paved ...].

  `paved` is 0 for a lane and 1 for an arterial. It rides along in the segment
  array because the ground needs it: out in the country a local street is a dirt
  track and a main road is still tarmac, and the only thing that knows which is
  which by the time the colour is chosen is the segment that won.

  Flat and typed because `road-at` runs about a thousand times per chunk against
  this array; boxed vector access shows up plainly in a profile."
  [streets]
  (let [out (transient [])]
    (doseq [{:keys [points half shoulder ya yb class]} streets]
      (let [n (count points)
            paved (case class :arterial 1.0 :collector 0.65 0.0)]
        (dotimes [i (dec n)]
          (let [[x1 z1] (nth points i)
                [x2 z2] (nth points (inc i))
                t1 (/ (double i) (dec n))
                t2 (/ (double (inc i)) (dec n))]
            (conj! out (double x1)) (conj! out (double z1))
            (conj! out (+ ya (* (- yb ya) t1)))
            (conj! out (double x2)) (conj! out (double z2))
            (conj! out (+ ya (* (- yb ya) t2)))
            (conj! out (double half)) (conj! out (double shoulder))
            (conj! out paved)))))
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
        ;; Bridges are left out: flattening the ground under one would fill in
        ;; the valley it exists to cross.
        segs (segments-of (remove :bridge? (streets-in-bounds seed x0 z0 x1 z1)))]
    (assoc (index-segments segs x0 z0 x1 z1) :segs segs)))

(defn- road-at
  "How road-like a point is, the road surface height there, and how paved it is.

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
    (loop [i (iget starts c), best 0.0, wsum 0.0, wy 0.0, wp 0.0]
      (if (>= i e)
        (if (pos? wsum) [best (/ wy wsum) (/ wp wsum)] [0.0 0.0 0.0])
        (let [o  (* seg-stride (iget items i))
              x1 (dget segs o)       z1 (dget segs (+ o 1)) y1 (dget segs (+ o 2))
              x2 (dget segs (+ o 3)) z2 (dget segs (+ o 4)) y2 (dget segs (+ o 5))
              half (dget segs (+ o 6)) shoulder (dget segs (+ o 7))
              paved (dget segs (+ o 8))
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
                   (+ wy (* r (+ y1 (* t (- y2 y1)))))
                   (+ wp (* r paved)))
            (recur (inc i) best wsum wy wp)))))))

(defn surface-detail
  "Final height at a point, how road-like it is in [0,1], and how paved that
  road is -- 0 for a country lane, 1 for an arterial.

  `field` comes from `road-field`; it is passed in rather than rebuilt because a
  chunk evaluates this about a thousand times. `rv` is an already-computed river
  amount, for callers that need the water anyway."
  [seed field x z rv]
  (let [[road ry paved] (road-at field x z)]
    (cond
      (>= road 1.0) [ry 1.0 paved]
      (<= road 0.0) [(if rv (base-height seed x z rv) (base-height seed x z)) 0.0 0.0]
      :else (let [b (if rv (base-height seed x z rv) (base-height seed x z))]
              [(+ ry (* (- b ry) (- 1.0 road))) road paved]))))

(defn surface
  "Height and road-ness at a point. The common case; `surface-detail` also
  reports the surfacing, which only the ground colour cares about."
  ([seed field x z] (surface-detail seed field x z nil))
  ([seed field x z rv] (surface-detail seed field x z rv)))

;; --- farmland ---------------------------------------------------------------

(def crop-names
  [:pasture :wheat :plough :rape :fallow :scrub :orchard :woodland])

(def ^:private crop-tints
  "Multipliers over the ground texture, which is green. Ploughed earth and rape
  have to lift red hard to get anywhere near brown and yellow."
  [[0.82 1.14 0.68]    ; pasture
   [1.42 1.14 0.44]    ; wheat
   [1.30 0.72 0.50]    ; plough
   [1.52 1.28 0.28]    ; rape
   [1.20 1.06 0.74]    ; fallow
   [0.92 0.94 0.64]    ; scrub
   [0.76 1.02 0.64]    ; orchard
   [0.50 0.76 0.50]])  ; woodland

(def ^:private crop-weights [26 18 14 8 10 10 6 8])
(def ^:private crop-cumulative
  (vec (reductions + crop-weights)))
(def ^:private crop-total (peek crop-cumulative))

(defn field-index
  "Which crop is growing at a point.

  One crop per lattice cell -- the same cell the hedgerows are drawn around, so
  a field is a field: bounded by hedge and lane, and one thing growing in it.
  Splitting cells into sub-parcels gave more variety and read as noise, because
  the colour changed in the middle of a hedged field with nothing to mark it.

  Derived straight from `hash-coords` rather than by making a generator: this
  runs for every terrain vertex in the world, and allocating a PRNG per vertex
  is not affordable."
  [seed x z]
  (let [gx (grid-floor x street-spacing)
        gz (grid-floor z street-spacing)
        k  (bit-and (prng/hash-coords (+ seed 6151) gx gz) 0x7fffffff)
        r  (mod k crop-total)]
    (loop [i 0]
      (if (or (= i (dec (count crop-cumulative))) (< r (nth crop-cumulative i)))
        i
        (recur (inc i))))))

(defn crop-at [seed x z] (nth crop-names (field-index seed x z)))

(defn industrialness
  "A second field, independent of `urbanness`, that says where the works are.

  Industry is not simply 'less city': it clusters, and it clusters at the edge
  of a town rather than in the middle of it. Giving it its own field is what
  stops factories being scattered through the housing."
  [seed x z]
  (let [d (* 3.5 k/chunk-size)]
    (noise/fbm2d (+ seed 8123) (/ x d) (/ z d) 3)))

;; --- what a place is --------------------------------------------------------

(def area-kinds
  "Coarse labels for a place, in the order a map legend would want them."
  [:water :wild :woods :farm :village :suburb :industry :city :downtown])

(def area-labels
  {:water "water" :wild "wild" :woods "woods" :farm "farmland"
   :village "village" :suburb "suburb" :industry "industrial"
   :city "city" :downtown "downtown"})

(defn area-kind
  "What sort of place a chunk is, in one word.

  Deliberately cheap: it samples the same fields the generator does -- how
  built-up, how industrial, how wet, what is growing -- and none of the
  geometry. A map can ask this about a hundred chunks at once, which is the
  whole point, because the thing a player wants to know about the chunk two
  kilometres away is exactly this and nothing else. Generating that chunk to
  find out would cost six milliseconds; this costs four field samples."
  [seed cx cz]
  (let [x (* (+ cx 0.5) k/chunk-size)
        z (* (+ cz 0.5) k/chunk-size)
        u (urbanness seed x z)
        ind (industrialness seed x z)]
    (cond
      (> (river seed x z) 0.55) :water
      (and (> ind 0.68) (< 0.22 u 0.74)) :industry
      (> u 0.82) :downtown
      (> u 0.58) :city
      (> u 0.34) :suburb
      (> u 0.16) :village
      (= :woodland (crop-at seed x z)) :woods
      (> u 0.05) :farm
      :else :wild)))

(defn area-label [seed cx cz] (area-labels (area-kind seed cx cz)))

(defn arterial-line?
  "Is lattice line `i` a main road? Pure arithmetic on the index, so a map can
  draw the road grid without generating a single street."
  [i]
  (= :arterial (line-class i)))


(defn ground-sampler
  "A function giving the height of the ground at a point.

  The analytic form, for callers with no heightfield to hand."
  [seed field]
  (fn [x z] (first (surface seed field x z))))

(defn heightfield-sampler
  "A function giving the height of a chunk's *heightfield* at a point.

  This is the surface the collider is built from and the mesh is drawn from, so
  standing an object on it rather than on the analytic surface is not an
  approximation -- it is the correction. The two differ by up to 0.14 m where
  the grid cannot follow a road's edge, and that difference is exactly the gap
  between where a lamp post looks like it is and where the car can drive.

  It is also most of the cost of a chunk. Building corners alone asked the
  analytic surface seven hundred times per chunk, each answer a road lookup plus
  a four-octave fbm; this is four array reads. Outside the chunk it falls back,
  because a prop may be flung past the edge of the plot that owns it."
  [heights n x0 z0 step seed field]
  (fn [x z]
    (let [fx (/ (- x x0) step)
          fz (/ (- z z0) step)]
      (if (or (< fx 0.0) (< fz 0.0) (> fx (dec n)) (> fz (dec n)))
        (first (surface seed field x z))
        (let [i0 (long (floor fx)) j0 (long (floor fz))
              i1 (min (dec n) (inc i0)) j1 (min (dec n) (inc j0))
              tx (- fx i0) tz (- fz j0)
              h00 (fget heights (+ (* i0 n) j0))
              h10 (fget heights (+ (* i1 n) j0))
              h01 (fget heights (+ (* i0 n) j1))
              h11 (fget heights (+ (* i1 n) j1))
              a (+ h00 (* (- h10 h00) tx))
              b (+ h01 (* (- h11 h01) tx))]
          (+ a (* (- b a) tz)))))))

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

;; --- extruded shapes --------------------------------------------------------
;;
;; Building masses, bridge decks and trees are all built from the same handful
;; of volumes. Shared here rather than under buildings because the bridges are
;; generated first and a forward reference in a .cljc file is a compile error,
;; not a subtlety.

(def building-part-stride 10)   ; x y z yaw sx sy sz prim mat tint
(def building-prims [:box :gable :pyramid :cylinder :blob])
(def part-prims building-prims)

;; The generic parts layout: anything the client draws as an instanced volume
;; with an optional collider. Bridges were the first user, flora the second.
(def part-stride 11)            ; x y z yaw pitch sx sy sz prim tint solid

(def ^:private prim-index (zipmap building-prims (range)))

;; --- bridges ----------------------------------------------------------------

(def ^:private deck-thickness 0.55)
(def ^:private deck-margin 1.1)     ; deck overhangs the carriageway either side
(def ^:private rail-height 0.9)
(def ^:private rail-panel 4.0)      ; metres of parapet per breakable panel
(def ^:private pier-spacing 15.0)
(def ^:private pier-min 2.0)        ; below this a pier is a stub, not worth it

(def ^:private bridge-tint
  {:deck 0x3c3c40 :rail 0x9aa0a6 :pier 0x8c8880})

(defn chunk-bridges
  "Deck, parapets and piers for every bridge this chunk owns, as
  [x y z yaw pitch sx sy sz prim tint solid ...].

  `solid` is three-valued: 0 for parts with no collider at all, 1 for fixed
  scenery, 2 for scenery that can be knocked out of the way. The deck is 1 and
  the parapets are 2, which is the difference between a bridge and a corridor:
  a car that cannot leave the sides of a span is driving down a tube with a
  view. Piers are 0 -- a pier stands under the deck where nothing can reach it,
  and a collider each would pay for a broad-phase entry for nothing.

  Parapets come in short panels rather than one slab per segment, so what a car
  takes out is a gap rather than the whole side of the bridge.

  The deck follows the chord the street's endpoint heights were taken from, so
  it meets the road exactly at both ends -- the approach is flattened terrain
  and the span is not, and they agree at the node because both are
  `base-height` there."
  ([seed cx cz] (chunk-bridges seed cx cz (chunk-lines seed cx cz)))
  ([seed cx cz owned]
  (let [out (transient [])
        emit (fn [x y z yaw pitch sx sy sz prim tint solid]
               (conj! out x) (conj! out y) (conj! out z)
               (conj! out yaw) (conj! out pitch)
               (conj! out sx) (conj! out sy) (conj! out sz)
               (conj! out (double (prim-index prim)))
               (conj! out (double tint)) (conj! out (double solid)))]
    (doseq [{:keys [points half ya yb]} (filter :bridge? owned)]
      (let [n (count points)
            chord (fn [t] (+ ya (* t (- yb ya))))
            width (+ (* 2.0 half) (* 2.0 deck-margin))]
        (dotimes [i (dec n)]
          (let [[x1 z1] (nth points i)
                [x2 z2] (nth points (inc i))
                t1 (/ (double i) (dec n))
                t2 (/ (double (inc i)) (dec n))
                y1 (chord t1) y2 (chord t2)
                mx (* 0.5 (+ x1 x2)) mz (* 0.5 (+ z1 z2))
                my (* 0.5 (+ y1 y2))
                dx (- x2 x1) dz (- z2 z1) dy (- y2 y1)
                horiz (max 0.01 (hypot dx dz))
                ;; The deck is pitched to lie along the chord. A flat slab at
                ;; the average height instead leaves its near end floating over
                ;; the road by half the fall of the span, and the car drives
                ;; under the leading edge and straight into the river.
                span (hypot horiz dy)
                pitch (#?(:clj Math/atan2 :cljs js/Math.atan2) dy horiz)
                ;; Local +Z runs along the span, so a unit box scaled in Z is a
                ;; deck. Dropping the centre by half a thickness measured
                ;; vertically leaves the top face on the chord itself.
                yaw (#?(:clj Math/atan2 :cljs js/Math.atan2) dx dz)
                sink (/ (* 0.5 deck-thickness) (max 0.2 (js-cos pitch)))
                rx (/ (- dz) horiz) rz (/ dx horiz)
                off (- (* 0.5 width) 0.15)]
            (emit mx (- my sink) mz yaw pitch
                  width deck-thickness span :box (:deck bridge-tint) 1.0)
            (let [panels (max 1 (long (floor (/ span rail-panel))))
                  plen   (/ span panels)
                  ;; Along the span, in world terms. The deck is pitched, so
                  ;; the panels have to step up it rather than around it.
                  ax (/ dx horiz) az (/ dz horiz)
                  ay (/ dy span)
                  step (/ (* plen horiz) span)]
              (dotimes [p panels]
                (let [u (- (+ p 0.5) (* 0.5 panels))       ; panels either side of centre
                      cxp (+ mx (* ax step u))
                      czp (+ mz (* az step u))
                      cyp (+ my (* ay plen u))]
                  (doseq [sgn [1.0 -1.0]]
                    (emit (+ cxp (* rx off sgn)) (+ cyp (* 0.5 rail-height)) (+ czp (* rz off sgn))
                          yaw pitch 0.3 rail-height (* 0.92 plen)
                          :box (:rail bridge-tint) 2.0)))))))
        ;; Piers, spaced along the whole span rather than per segment.
        (let [[ax az] (first points)
              [bx bz] (peek points)
              span (hypot (- bx ax) (- bz az))
              piers (long (floor (/ span pier-spacing)))]
          (dotimes [i piers]
            (let [t (/ (+ i 0.5) (double piers))
                  px (+ ax (* t (- bx ax)))
                  pz (+ az (* t (- bz az)))
                  top (- (chord t) deck-thickness)
                  ground (base-height seed px pz)
                  h (- top ground)]
              (when (> h pier-min)
                (emit px (+ ground (* 0.5 h)) pz 0.0 0.0
                      2.0 h 2.0 :cylinder (:pier bridge-tint) 0.0)))))))
    (let [v (persistent! out)
          a (farray (count v))]
      (dotimes [i (count v)] (fput! a i (nth v i)))
      a))))

(defn street-between
  "The street joining two adjacent lattice nodes, in the order asked for.

  `street` is defined from the lower node outward, so a driver travelling the
  other way needs its polyline and chord reversed. Doing that here saves every
  caller from knowing which way round the lattice stores things."
  [seed [ax az] [bx bz]]
  (let [forward? (or (< ax bx) (< az bz))
        along-x? (not= ax bx)
        [gx gz] (if forward? [ax az] [bx bz])
        st (street seed gx gz along-x?)]
    (if forward?
      st
      (assoc st :points (vec (reverse (:points st)))
                :ya (:yb st) :yb (:ya st)))))

;; --- traffic ----------------------------------------------------------------

(def traffic-stride 6)   ; from-gx from-gz to-gx to-gz t0 speed

(def ^:private traffic-speed {:arterial 16.0 :collector 12.0 :local 8.0})

(defn- traffic-odds [u cls]
  (case cls
    :arterial  (+ 0.30 (* 0.75 u))
    :collector (+ 0.18 (* 0.65 u))
    (max 0.0 (- (* 0.55 u) 0.10))))

(defn chunk-traffic
  "Civilian cars for one chunk, as [from-gx from-gz to-gx to-gz t0 speed ...].

  A car is placed on a street this chunk owns and drives away from there; where
  it goes after that is decided at each node it reaches, so the spawn only has
  to say where it starts. Ownership is by street, exactly as with props, so no
  two chunks put a car on the same road.

  Two rolls per street: one car per street leaves a city grid looking
  abandoned, and two is enough to read as traffic."
  ([seed cx cz] (chunk-traffic seed cx cz (chunk-lines seed cx cz)))
  ([seed cx cz owned]
  (let [r   (prng/chunk-rng seed cx cz 1237)
        out (transient [])]
    (doseq [{:keys [points class gx gz along-x?]} owned]
      (let [[ax az] (first points)
            [bx bz] (peek points)
            u (urbanness seed (* 0.5 (+ ax bx)) (* 0.5 (+ az bz)))]
        (dotimes [_ 2]
          (when (< (prng/next-double! r) (traffic-odds u class))
            (let [back? (prng/next-bool! r)
                  [fx fz tx tz] (if along-x?
                                  (if back? [(inc gx) gz gx gz] [gx gz (inc gx) gz])
                                  (if back? [gx (inc gz) gx gz] [gx gz gx (inc gz)]))]
              (conj! out (double fx)) (conj! out (double fz))
              (conj! out (double tx)) (conj! out (double tz))
              (conj! out (prng/next-double! r))
              (conj! out (* (traffic-speed class 8.0)
                            (prng/next-range! r 0.82 1.12))))))))
    (let [v (persistent! out)
          a (farray (count v))]
      (dotimes [i (count v)] (fput! a i (nth v i)))
      a))))

;; --- flora ------------------------------------------------------------------

(def ^:private tree-grid 11.0)      ; metres between candidate positions
(def ^:private tree-urban 0.45)     ; above this there is no room for a wood
(def ^:private hedge-urban 0.35)

(def ^:private flora-tint
  {:trunk   0x5a4432
   :hedge   0x3d5f33
   :conifer 0x2c4f2e})

(def ^:private leaf-tints [0x3f6b32 0x4a7a38 0x35602c 0x54803c])

;; How likely a candidate position is to actually grow something, by crop. A
;; wood is a wood because the parcel says so, not because a density field
;; happened to peak there -- which is what keeps the tree line following the
;; field boundary the way a real one does.
(def ^:private crop-tree-odds
  {:woodland 0.88 :orchard 0.80 :scrub 0.17 :pasture 0.05
   :fallow 0.07 :wheat 0.012 :plough 0.008 :rape 0.010})

(defn- tree-parts!
  "A trunk and a canopy at (x, y, z). The trunk is solid and the canopy is not:
  a tree stops a car, and its branches are for driving through."
  [emit seed x y z h]
  (let [k (prng/hash-coords (+ seed 3313) (long (* x 4.0)) (long (* z 4.0)))
        conifer? (zero? (bit-and (prng/shr32 k 5) 3))
        r (* h (if conifer? 0.20 0.30))
        trunk (* h (if conifer? 0.30 0.45))
        leaf (nth leaf-tints (bit-and (prng/shr32 k 9) 3))]
    (emit x (+ y (* 0.5 trunk)) z 0.0 0.0
          (* 0.28 h 0.5) trunk (* 0.28 h 0.5) :cylinder (:trunk flora-tint) 1.0)
    (if conifer?
      (emit x (+ y trunk (* 0.5 (- h trunk))) z 0.0 0.0
            (* 2.0 r) (- h trunk) (* 2.0 r) :pyramid (:conifer flora-tint) 0.0)
      (emit x (+ y trunk (* 0.5 (- h trunk))) z 0.0 0.0
            (* 2.0 r) (- h trunk) (* 2.2 r) :blob leaf 0.0))))

(defn chunk-flora
  "Trees, orchards and hedgerows for one chunk, in the generic parts layout.

  Candidate tree positions come from a global grid, jittered per point, and a
  tree belongs to whichever chunk its jittered position lands in -- so the grid
  can be walked from either side of a border without a tree being planted twice
  or missed. An orchard is the same grid left unjittered, which is all it takes
  to read as planted rather than grown."
  [seed cx cz field]
  (let [out (transient [])
        emit (fn [x y z yaw pitch sx sy sz prim tint solid]
               (conj! out x) (conj! out y) (conj! out z)
               (conj! out yaw) (conj! out pitch)
               (conj! out sx) (conj! out sy) (conj! out sz)
               (conj! out (double (prim-index prim)))
               (conj! out (double tint)) (conj! out (double solid)))
        x0 (* cx k/chunk-size) z0 (* cz k/chunk-size)
        x1 (+ x0 k/chunk-size) z1 (+ z0 k/chunk-size)
        i0 (dec (grid-floor x0 tree-grid)) i1 (inc (grid-floor x1 tree-grid))
        j0 (dec (grid-floor z0 tree-grid)) j1 (inc (grid-floor z1 tree-grid))]
    ;; Trees.
    (doseq [gi (range i0 (inc i1)), gj (range j0 (inc j1))]
      (let [h (prng/hash-coords (+ seed 5107) gi gj)
            bx (* gi tree-grid) bz (* gj tree-grid)
            crop (crop-at seed bx bz)
            orchard? (= :orchard crop)
            jx (if orchard? 0.0 (* tree-grid 0.42 (- (/ (bit-and h 0xff) 127.5) 1.0)))
            jz (if orchard? 0.0 (* tree-grid 0.42
                                   (- (/ (bit-and (prng/shr32 h 8) 0xff) 127.5) 1.0)))
            x (+ bx jx) z (+ bz jz)]
        (when (and (<= x0 x) (< x x1) (<= z0 z) (< z z1))
          (let [odds (get crop-tree-odds crop 0.0)
                roll (/ (bit-and (prng/shr32 h 16) 0x3ff) 1024.0)]
            (when (< roll odds)
              (let [u (urbanness seed x z)
                    [y road] (surface seed field x z)]
                ;; Nothing grows downtown, in the river, or on the verge where
                ;; the lamp posts are.
                (when (and (< u tree-urban) (< (river seed x z) 0.3) (< road 0.12))
                  (let [hh (+ 5.0 (* 6.0 (/ (bit-and (prng/shr32 h 26) 0x3f) 63.0)))]
                    (tree-parts! emit seed x y z hh)))))))))
    ;; Hedgerows along the field boundaries the streets have not already taken.
    (doseq [gx (range (dec (grid-floor x0 street-spacing))
                      (inc (inc (grid-floor x1 street-spacing))))
            gz (range (dec (grid-floor z0 street-spacing))
                      (inc (inc (grid-floor z1 street-spacing))))]
      (let [[ax az] (node seed gx gz)
            [bx bz] (node seed (inc gx) gz)
            [dx dz] (node seed gx (inc gz))]
        (doseq [[side ex ez fx fz along-x?] [[:north ax az bx bz true]
                                             [:west ax az dx dz false]]]
          ;; Only where no street runs along it -- a lane is already a boundary.
          (when-not (edge-exists? seed gx gz along-x?)
            (let [mx (* 0.5 (+ ex fx)) mz (* 0.5 (+ ez fz))]
              (when (and (<= x0 mx) (< mx x1) (<= z0 mz) (< mz z1)
                         (< (urbanness seed mx mz) hedge-urban)
                         (< (river seed mx mz) 0.25))
                (let [ddx (- fx ex) ddz (- fz ez)
                      len (max 1.0 (hypot ddx ddz))
                      yaw (#?(:clj Math/atan2 :cljs js/Math.atan2) ddx ddz)
                      hh (prng/hash-coords (+ seed 7919) gx gz)
                      ;; Two runs with a gateway between them, rather than one
                      ;; unbroken wall across the field.
                      gap (+ 0.16 (* 0.12 (/ (bit-and hh 0xff) 255.0)))]
                  (doseq [[t0 t1] [[0.03 (- 0.5 (* 0.5 gap))]
                                   [(+ 0.5 (* 0.5 gap)) 0.97]]]
                    (let [tm (* 0.5 (+ t0 t1))
                          px (+ ex (* ddx tm)) pz (+ ez (* ddz tm))
                          [y _] (surface seed field px pz)]
                      (emit px (+ y 0.75) pz yaw 0.0
                            1.3 1.6 (* len (- t1 t0)) :box (:hedge flora-tint) 0.0))))))))))
    (let [v (persistent! out)
          a (farray (count v))]
      (dotimes [i (count v)] (fput! a i (nth v i)))
      a)))

;; --- props ------------------------------------------------------------------

(def prop-kinds
  "Smashable roadside clutter. Shared so the server can reason about what a
  chunk contains without rendering it.

  `volatile?` is the one that matters to play: a gas cylinder goes up when it
  is hit hard, taking its neighbours and anything standing near them with it."
  ;; `shape` is what it is drawn as; the collider is a box either way. Every
  ;; one of these used to be a box, and a tan box a metre across is
  ;; indistinguishable at speed from an amber crate you are meant to drive
  ;; through -- which is what players did, repeatedly, into a gas cylinder.
  [{:name :crate  :half [0.60 0.60 0.60] :density 40.0 :colour 0x9a7038
    :shape :box}
   {:name :barrel :half [0.45 0.75 0.45] :density 55.0 :colour 0x4a6a7a
    :shape :cylinder}
   {:name :sign   :half [0.12 1.10 0.80] :density 26.0 :colour 0xa8a49c
    :shape :box}
   ;; Red, round, and the only round red thing in the game.
   {:name :gas-barrel :half [0.46 0.80 0.46] :density 60.0 :colour 0xc4442e
    :shape :cylinder :volatile? true}])

(def gas-barrel-kind 3)

(defn prop-kind-at
  "Which piece of clutter stands at (x, z), given a uniform roll.

  Gas follows the works. A red cylinder outside a florist is a joke; outside a
  chemical plant it is a warning, and the player learns to read the district by
  what is stacked at the kerb. `roll` is drawn by the caller so the random
  stream advances identically wherever the barrel turns out to belong."
  [seed x z roll]
  (let [ind (industrialness seed x z)
        gas (cond (> ind 0.62) 0.38
                  (> ind 0.42) 0.16
                  (> ind 0.28) 0.05
                  :else 0.01)]
    (if (< roll gas)
      gas-barrel-kind
      (let [t (/ (- roll gas) (max 1e-6 (- 1.0 gas)))]
        (cond (< t 0.45) 0 (< t 0.80) 1 :else 2)))))

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
  ([seed cx cz field] (chunk-props seed cx cz field (chunk-lines seed cx cz)))
  ([seed cx cz field owned]
   (chunk-props seed cx cz field owned (ground-sampler seed field)))
  ([seed cx cz field owned ground]
  ;; Bridges are skipped: `surface` under a span reports the riverbed, so a
  ;; barrel placed along one would sit in the water forty feet below the road.
  (let [lines (remove :bridge? owned)]
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
                ;; Drawn here rather than after the position so the stream
                ;; advances the same amount however the placement turns out;
                ;; what it *means* is decided below, once there is a place.
                roll  (prng/next-double! r)
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
                y (ground x z)]
            (conj! out x) (conj! out y) (conj! out z)
            (conj! out yaw)
            (conj! out (double (prop-kind-at seed x z roll)))
            (conj! out scale)))
        (let [v (persistent! out)
              a (farray (count v))]
          (dotimes [i (count v)] (fput! a i (nth v i)))
          a))))))

;; --- blocks, lots and zoning ------------------------------------------------

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

;; --- landmarks --------------------------------------------------------------
;;
;; One per district, always. A city where every block is interchangeable has no
;; landmarks by definition: what makes somewhere a place is that it has a thing
;; in it you can point at from the next district over, and navigate by.
;;
;; A landmark takes a whole lattice cell and the lots in that cell are not
;; generated, so it replaces a block of housing rather than sitting on top of
;; one. It is built from the same handful of prims as bridges and trees, and
;; travels to the client down the same array.

(def district-chunks 4)          ; a district is 4x4 chunks, about a km square
(def ^:private cells-per-district
  (long (/ (* district-chunks k/chunk-size) street-spacing)))

(def landmark-kinds
  [:stadium :mall :park :plaza :works :silos :church :monument :mast])

(def landmark-labels
  {:stadium "the stadium" :mall "the shopping centre" :park "the park"
   :plaza "the plaza" :works "the works" :silos "the grain silos"
   :church "the church" :monument "the standing stones"
   :mast "the transmitter"})

(defn district-of
  "Which district a chunk belongs to. Floor division, so it keeps working west
  and north of the origin -- truncation would fold two districts into one along
  each axis."
  [cx cz]
  [(long (floor (/ (double cx) district-chunks)))
   (long (floor (/ (double cz) district-chunks)))])

(defn- landmark-for-place
  "What sort of landmark belongs somewhere like this.

  Drawn from the area kind rather than at random, because a grain silo in the
  middle of downtown is not a landmark, it is a mistake."
  [kind r]
  (case kind
    :downtown (nth [:stadium :plaza :mall] (prng/next-int! r 3))
    :city     (nth [:stadium :mall :park :plaza] (prng/next-int! r 4))
    :suburb   (nth [:mall :park :church] (prng/next-int! r 3))
    :industry :works
    :village  (if (prng/next-bool! r) :church :park)
    :farm     (if (prng/next-bool! r) :silos :mast)
    ;; Open country is most of the world, so it needs more than one answer or
    ;; half the landmarks anywhere are the same ring of stones.
    :woods    (if (prng/next-bool! r) :monument :mast)
    :wild     (nth [:monument :mast :monument :silos] (prng/next-int! r 4))
    :monument))

(defn landmark
  "The landmark of district (dx, dz): {:kind :cell [gx gz] :x :z :radius}, or
  nil where there is nowhere to put one.

  Eight tries at a cell rather than a scan of all 256. A scan would be the most
  expensive question the map can ask, and the map asks it about every district
  on screen; eight tries finds dry buildable ground everywhere except the middle
  of a lake, which is a district that should not have a landmark anyway."
  [seed dx dz]
  (let [r   (prng/chunk-rng seed dx dz (:landmarks k/salt))
        g0x (* dx cells-per-district)
        g0z (* dz cells-per-district)]
    (loop [i 0]
      (when (< i 8)
        (let [gx (+ g0x (prng/next-int! r cells-per-district))
              gz (+ g0z (prng/next-int! r cells-per-district))
              interior (cell-interior seed gx gz)]
          (if-let [{:keys [x0 x1 z0 z1]} interior]
            (let [x (* 0.5 (+ x0 x1))
                  z (* 0.5 (+ z0 z1))
                  ;; Rivers are checked directly rather than through
                  ;; `area-kind`, which answers for a whole chunk and would
                  ;; happily drop a stadium on the one wet corner of a dry one.
                  wet? (> (river seed x z) 0.35)
                  [cx cz] (chunk-of x z)]
              (if wet?
                (recur (inc i))
                {:kind   (landmark-for-place (area-kind seed cx cz) r)
                 :cell   [gx gz]
                 :x x :z z
                 :half-x (* 0.5 (- x1 x0))
                 :half-z (* 0.5 (- z1 z0))}))
            (recur (inc i))))))))

(defn landmark-cells
  "The landmark cells of every district touching lattice cells gx0..gx1,
  gz0..gz1, as a set. Districts are a kilometre across and chunks a quarter of
  that, so this is one district in the middle of one and four at a corner."
  [seed gx0 gx1 gz0 gz1]
  (let [d (fn [g] (long (floor (/ (double g) cells-per-district))))]
    (into #{}
          (for [dx (range (d gx0) (inc (d gx1)))
                dz (range (d gz0) (inc (d gz1)))
                :let [lm (landmark seed dx dz)]
                :when lm]
            (:cell lm)))))


(def ^:private tau 6.283185307179586)

(def ^:private landmark-tints
  {:concrete 0xb8b4ac :dark 0x4a4a4e :grass 0x4c7a3e :water 0x35617f
   :brick 0x9a5f47 :metal 0x8b9199 :roof 0x6b4a3c :stone 0x8f8a80
   :tarmac 0x3a3a3e :white 0xd8d0c4 :timber 0x6f5238 :leaf 0x3f6b32
   :red 0xa33b30})

(defn- lp
  "One landmark part, in the cell's own frame: centre at (0,0), y from the
  ground the landmark was levelled to."
  ([x y z sx sy sz prim tint] (lp x y z 0.0 sx sy sz prim tint 1.0))
  ([x y z yaw sx sy sz prim tint solid]
   {:x x :y y :z z :yaw yaw :sx sx :sy sy :sz sz :prim prim
    :tint (landmark-tints tint) :solid solid}))

(defn- apron
  "The slab a landmark stands on.

  Terrain under a landmark cell is not flattened -- flattening it would mean
  the heightfield and its collider disagreeing with the road field -- so
  everything is built to one height sampled at the centre and given a plinth
  deep enough to bury the difference. On a slope it reads as a raised
  platform, which is what a stadium on a hillside looks like anyway."
  [hx hz tint]
  (lp 0.0 -1.4 0.0 0.0 (* 1.9 hx) 3.0 (* 1.9 hz) :box tint 0.0))

(defn- ring
  "n points evenly around an ellipse: [x z angle]."
  [n rx rz]
  (for [i (range n)
        :let [a (* tau (/ (double i) n))]]
    [(* rx (js-sin a)) (* rz (js-cos a)) a]))

(defn- tree-at [x z h]
  [(lp x (* 0.5 h) z 0.0 0.5 h 0.5 :cylinder :timber 1.0)
   (lp x (* 1.05 h) z 0.0 (* 0.9 h) (* 0.8 h) (* 0.9 h) :blob :leaf 0.0)])

(defmulti ^:private landmark-shapes
  "The volumes a landmark is made of, in its own frame."
  (fn [kind _hx _hz _r] kind))

(defmethod landmark-shapes :stadium [_ hx hz r]
  (let [rx (* 0.52 hx) rz (* 0.52 hz)]
    (concat
     [(apron hx hz :concrete)
      ;; Track, then pitch inside it, and both ellipses. The pitch was a square
      ;; box first and its corners came out through the track that was meant to
      ;; be running round the outside of it.
      (lp 0.0 0.14 0.0 0.0 (* 2.5 rx) 0.28 (* 2.5 rz) :cylinder :tarmac 0.0)
      (lp 0.0 0.24 0.0 0.0 (* 2.0 rx) 0.30 (* 2.0 rz) :cylinder :grass 0.0)]
     ;; Stands, laid tangentially and overlapping slightly, so the silhouette
     ;; from outside is a wall and from above a ring.
     (for [[x z a] (ring 24 (* 1.45 rx) (* 1.45 rz))]
       (lp x 8.0 z a (* 0.30 (+ rx rz)) 16.0 13.0 :box :concrete 1.0))
     ;; Floodlights, which is what makes it a stadium from three blocks away.
     (for [[x z _] (ring 4 (* 1.75 rx) (* 1.75 rz))]
       (lp x 15.0 z 0.0 1.2 30.0 1.2 :cylinder :metal 1.0))
     (for [[x z _] (ring 4 (* 1.75 rx) (* 1.75 rz))]
       (lp x 31.0 z 0.0 5.0 2.0 1.5 :box :white 0.0)))))

(defmethod landmark-shapes :mall [_ hx hz r]
  [(apron hx hz :tarmac)
   ;; Car park stripes, then the shed itself set back from the road.
   (lp 0.0 0.10 (* 0.55 hz) 0.0 (* 1.7 hx) 0.2 (* 0.7 hz) :box :dark 0.0)
   (lp 0.0 5.5 (* -0.25 hz) 0.0 (* 1.5 hx) 11.0 (* 1.0 hz) :box :concrete 1.0)
   (lp 0.0 11.6 (* -0.25 hz) 0.0 (* 1.52 hx) 1.2 (* 1.02 hz) :box :metal 1.0)
   ;; Entrance canopy and the pylon sign that makes it visible three blocks off.
   (lp 0.0 3.4 (* 0.28 hz) 0.0 (* 0.45 hx) 6.8 6.0 :box :white 1.0)
   (lp (* -0.7 hx) 9.0 (* 0.7 hz) 0.0 1.0 18.0 1.0 :cylinder :metal 1.0)
   (lp (* -0.7 hx) 18.5 (* 0.7 hz) 0.0 4.0 3.0 1.2 :box :red 0.0)])

(defmethod landmark-shapes :park [_ hx hz r]
  (concat
   [(apron hx hz :grass)
    (lp 0.0 0.14 0.0 0.0 (* 1.85 hx) 0.28 (* 1.85 hz) :box :grass 0.0)
    ;; A pond, a path across it, and a bandstand to aim at.
    (lp (* 0.42 hx) 0.20 (* -0.35 hz) 0.0 (* 0.55 hx) 0.3 (* 0.5 hz)
        :cylinder :water 0.0)
    (lp 0.0 0.22 0.0 0.0 (* 1.8 hx) 0.32 3.0 :box :stone 0.0)
    (lp (* -0.45 hx) 2.2 (* 0.4 hz) 0.0 6.0 4.4 6.0 :cylinder :white 1.0)
    (lp (* -0.45 hx) 5.4 (* 0.4 hz) 0.0 7.0 3.0 7.0 :pyramid :roof 0.0)]
   (mapcat (fn [[x z _]] (tree-at x z (+ 6.0 (prng/next-range! r 0.0 4.0))))
           (ring 9 (* 0.78 hx) (* 0.78 hz)))))

(defmethod landmark-shapes :plaza [_ hx hz r]
  (concat
   [(apron hx hz :stone)
    (lp 0.0 0.16 0.0 0.0 (* 1.85 hx) 0.32 (* 1.85 hz) :box :stone 0.0)
    ;; An obelisk, because a plaza with nothing in the middle is a car park.
    (lp 0.0 1.0 0.0 0.0 7.0 2.0 7.0 :box :white 1.0)
    (lp 0.0 11.0 0.0 0.0 2.6 20.0 2.6 :box :white 1.0)
    (lp 0.0 23.0 0.0 0.0 2.8 4.0 2.8 :pyramid :white 0.0)
    (lp 0.0 0.7 (* 0.62 hz) 0.0 14.0 1.4 4.0 :box :water 1.0)]
   (mapcat (fn [[x z _]] (tree-at x z 7.0))
           (ring 8 (* 0.82 hx) (* 0.82 hz)))))

(defmethod landmark-shapes :works [_ hx hz r]
  (concat
   [(apron hx hz :tarmac)
    (lp (* -0.45 hx) 6.0 0.0 0.0 (* 0.85 hx) 12.0 (* 1.3 hz) :box :metal 1.0)
    (lp (* -0.45 hx) 14.0 0.0 0.0 (* 0.87 hx) 5.0 (* 1.32 hz) :gable :roof 0.0)
    (lp (* 0.5 hx) 4.5 (* -0.4 hz) 0.0 (* 0.6 hx) 9.0 (* 0.6 hz) :box :brick 1.0)]
   ;; Chimneys, which is what makes it readable from the other side of town.
   (for [[x z _] (ring 2 (* 0.55 hx) (* 0.55 hz))]
     (lp x 18.0 z 0.0 3.0 36.0 3.0 :cylinder :brick 1.0))
   ;; Tanks.
   (for [[x z _] (ring 3 (* 0.62 hx) (* 0.62 hz))]
     (lp x 4.0 z 0.0 9.0 8.0 9.0 :cylinder :metal 1.0))))

(defmethod landmark-shapes :silos [_ hx hz r]
  (concat
   [(apron hx hz :concrete)
    (lp (* 0.4 hx) 5.0 (* 0.35 hz) 0.0 (* 0.7 hx) 10.0 (* 0.7 hz) :box :timber 1.0)
    (lp (* 0.4 hx) 12.0 (* 0.35 hz) 0.0 (* 0.72 hx) 5.0 (* 0.72 hz) :gable :roof 0.0)
    ;; The conveyor, running from the barn to the silos.
    (lp 0.0 11.0 0.0 0.0 (* 1.2 hx) 1.2 1.2 :box :metal 1.0)]
   (for [i (range 5)]
     (lp (+ (* -0.55 hx) (* i 8.5)) 11.0 (* -0.35 hz) 0.0
         7.5 22.0 7.5 :cylinder :white 1.0))))

(defmethod landmark-shapes :church [_ hx hz r]
  [(apron hx hz :grass)
   (lp 0.0 0.14 0.0 0.0 (* 1.8 hx) 0.28 (* 1.8 hz) :box :grass 0.0)
   ;; Nave, roof, tower, spire. A spire is the one shape that says church at
   ;; four hundred metres.
   (lp 0.0 5.0 (* 0.15 hz) 0.0 12.0 10.0 (* 1.1 hz) :box :stone 1.0)
   (lp 0.0 12.0 (* 0.15 hz) 0.0 12.5 4.0 (* 1.12 hz) :gable :roof 0.0)
   (lp 0.0 9.0 (* -0.62 hz) 0.0 8.0 18.0 8.0 :box :stone 1.0)
   (lp 0.0 24.0 (* -0.62 hz) 0.0 8.5 12.0 8.5 :pyramid :roof 0.0)
   ;; The churchyard wall, which is what you actually hit.
   (lp 0.0 0.7 (* 0.92 hz) 0.0 (* 1.8 hx) 1.4 0.8 :box :stone 1.0)
   (lp 0.0 0.7 (* -0.92 hz) 0.0 (* 1.8 hx) 1.4 0.8 :box :stone 1.0)])

(defmethod landmark-shapes :monument [_ hx hz r]
  (concat
   [(lp 0.0 -0.8 0.0 0.0 (* 1.5 hx) 2.4 (* 1.5 hz) :cylinder :grass 0.0)]
   ;; A ring of stones, each leaning its own way. It is the only landmark that
   ;; belongs in open country, and the only one with no straight lines in it.
   (mapcat (fn [[x z a]]
             (let [h (prng/next-range! r 5.0 8.0)]
               [(lp x (* 0.5 h) z (+ a (prng/next-range! r -0.3 0.3))
                    2.6 h 1.4 :box :stone 1.0)]))
           (ring 9 (* 0.5 hx) (* 0.5 hz)))
   ;; Two lintels across the nearest pair, so it reads as built rather than
   ;; scattered.
   (for [[x z a] (take 2 (ring 9 (* 0.5 hx) (* 0.5 hz)))]
     (lp (* 0.94 x) 8.4 (* 0.94 z) a 6.0 1.2 1.4 :box :stone 1.0))))

(defmethod landmark-shapes :mast [_ hx hz r]
  (concat
   [(lp 0.0 0.3 0.0 0.0 14.0 1.0 14.0 :box :concrete 0.0)
    ;; A lattice mast: three legs and a stack of platforms. Nothing else in the
    ;; catalogue is visible from a district away in flat country.
    (lp 0.0 45.0 0.0 0.0 1.4 90.0 1.4 :cylinder :metal 1.0)
    (lp 0.0 92.0 0.0 0.0 0.7 6.0 0.7 :cylinder :red 0.0)
    ;; The compound: a hut and a fence you can drive through the middle of.
    (lp (* 0.4 hx) 2.0 (* 0.4 hz) 0.0 8.0 4.0 6.0 :box :white 1.0)]
   (for [[x z a] (ring 3 5.5 5.5)]
     (lp x 22.0 z a 0.8 44.0 0.8 :box :metal 1.0))
   (for [y [18.0 40.0 66.0]]
     (lp 0.0 y 0.0 0.0 7.0 0.8 7.0 :box :metal 0.0))))

(defn chunk-landmarks
  "The landmark this chunk owns, as a flat parts array in the same layout as
  `chunk-bridges`. Empty for the fifteen chunks in a district that do not own
  one, which is most of them.

  Ownership is by the landmark's centre, the same rule streets use, so exactly
  one chunk builds it however the districts and the chunk grid line up."
  [seed cx cz]
  (let [[dx dz] (district-of cx cz)
        out (transient [])]
    (doseq [ddx [-1 0 1], ddz [-1 0 1]
            :let [lm (landmark seed (+ dx ddx) (+ dz ddz))]
            :when lm
            :let [[ox oz] (chunk-of (:x lm) (:z lm))]
            :when (and (= ox cx) (= oz cz))]
      (let [{:keys [kind x z half-x half-z]} lm
            y0 (height-at seed x z)
            ;; Its own generator, so adding a landmark kind cannot shift the
            ;; trees in the next district.
            r  (prng/chunk-rng seed cx cz (+ 97 (:landmarks k/salt)))]
        (doseq [{:keys [x y z yaw sx sy sz prim tint solid]}
                (remove nil? (flatten (landmark-shapes kind half-x half-z r)))]
          (doseq [v [(+ (:x lm) x) (+ y0 y) (+ (:z lm) z) yaw 0.0 sx sy sz
                     (double (prim-index prim)) (double tint) solid]]
            (conj! out v)))))
    (let [v (persistent! out)
          a (farray (count v))]
      (dotimes [i (count v)] (fput! a i (nth v i)))
      a)))

(defn chunk-lots
  "The plots this chunk owns -- those whose centre lands inside it."
  [seed cx cz]
  (let [x0 (* cx k/chunk-size) z0 (* cz k/chunk-size)
        x1 (+ x0 k/chunk-size) z1 (+ z0 k/chunk-size)
        gx0 (dec (grid-floor x0 street-spacing))
        gx1 (inc (grid-floor x1 street-spacing))
        gz0 (dec (grid-floor z0 street-spacing))
        gz1 (inc (grid-floor z1 street-spacing))
        ;; A landmark takes the whole cell, so no lots are cut in it. Computed
        ;; once for the chunk rather than per cell: the answer is a property of
        ;; the district, and asking it twenty-five times would mean rerunning
        ;; the same district search twenty-five times.
        claimed (landmark-cells seed gx0 gx1 gz0 gz1)]
    (vec (for [gx (range gx0 (inc gx1))
               gz (range gz0 (inc gz1))
               :when (not (contains? claimed [gx gz]))
               lot (cell-lots seed gx gz)
               :when (and (<= x0 (:x lot)) (< (:x lot) x1)
                          (<= z0 (:z lot)) (< (:z lot) z1))]
           lot))))

;; --- buildings --------------------------------------------------------------

(def building-stride 8)      ; x y z hx hz height zone yaw

;; --- building masses --------------------------------------------------------
;;
;; A building is a handful of extruded volumes rather than one box. That is the
;; whole difference between "a box with a shop texture on it" and something that
;; reads as a shop at 90 km/h: the awning, the sign band and the parapet are
;; what the eye picks up, not the wall behind them.
;;
;; Parts are emitted in the building's own frame and transformed here, so the
;; client only has to place them. Local -Z is the street side.

(def plain-mat
  "Value in a part's `mat` slot meaning 'flat colour from `tint`'. Anything
  else is an index into the zone facades, i.e. a wall with windows in it."
  -1.0)

(def ^:private palette
  {:roof-dark  0x39383a
   :roof-tile  0x7d4636
   :roof-metal 0x8f959b
   :concrete   0xa5a29a
   :stone      0xbdb6a4
   :brick      0x8a5a48
   :wood       0x6d4b34
   :glass      0x7f96a8
   :awning     0xb2452f
   :sign       0xd8b23c
   :steel      0x7c8288
   :door       0x44444a})

(defn- part
  "One extruded volume, in the building's local frame."
  ([lx ly lz sx sy sz prim tint] (part lx ly lz 0.0 sx sy sz prim tint))
  ([lx ly lz lyaw sx sy sz prim tint]
   {:lx lx :ly ly :lz lz :lyaw lyaw :sx sx :sy sy :sz sz
    :prim prim :tint tint}))

(defn- wall
  "A part painted with the building's own facade rather than a flat colour."
  [lx ly lz sx sy sz]
  (assoc (part lx ly lz sx sy sz :box 0) :facade? true))

(defn- ridged-roof
  "A gable whose ridge runs along the building's longer horizontal axis.

  The primitive extrudes its triangle along Z, so a building that is wider than
  it is deep needs the roof turning a quarter turn and its extents swapped --
  otherwise every wide house gets a roof running the wrong way."
  [hx hz rise ly tint overhang]
  (let [ox (* hx overhang) oz (* hz overhang)]
    (if (> hx hz)
      (part 0.0 (+ ly (* 0.5 rise)) 0.0 (/ #?(:clj Math/PI :cljs js/Math.PI) 2)
            (* 2 oz) rise (* 2 ox) :gable tint)
      (part 0.0 (+ ly (* 0.5 rise)) 0.0
            (* 2 ox) rise (* 2 oz) :gable tint))))

(defn- mass-parts
  "The volumes that make up one building, in its local frame.

  Every zone gets a silhouette of its own -- a gable and a chimney, a slab with
  balcony bands, a shed with a sawtooth roof and a stack -- because silhouette
  is what survives at speed and at distance."
  [r zone hx hz h]
  (let [c   palette
        pick (fn [& ks] (nth (vec ks) (prng/next-int! r (count ks))))
        base (wall 0.0 (* 0.5 h) 0.0 (* 2 hx) h (* 2 hz))]
    (case zone
      :house
      (let [rise (max 1.4 (* 1.05 (min hx hz)))]
        [base
         (ridged-roof hx hz rise h (pick (:roof-tile c) (:roof-dark c)) 1.10)
         ;; Porch out toward the street, chimney up through the roof.
         (part 0.0 1.1 (- (+ hz 0.7)) (* hx 1.0) 2.2 1.4 :box (:wood c))
         (part (* hx 0.55) (+ h (* rise 0.75)) (* hz 0.25)
               0.6 (+ 1.4 rise) 0.6 :box (:brick c))])

      :townhouse
      [base
       ;; Parapet and a string course: terraces read by their horizontal lines.
       (part 0.0 (+ h 0.35) 0.0 (* 2 hx 1.04) 0.7 (* 2 hz 1.04) :box (:stone c))
       (part 0.0 (* h 0.46) (- (+ hz 0.06)) (* 2 hx) 0.35 0.16 :box (:stone c))
       (part 0.0 1.05 (- (+ hz 0.12)) 1.0 2.1 0.3 :box (:door c))]

      :apartment
      (let [bands (+ 2 (prng/next-int! r 3))]
        (into [base
               (part 0.0 (+ h 0.3) 0.0 (* 2 hx 1.03) 0.6 (* 2 hz 1.03) :box (:concrete c))
               ;; Lift plant on the roof.
               (part (* hx 0.3) (+ h 1.6) (* hz 0.2) (* hx 0.7) 2.2 (* hz 0.6)
                     :box (:concrete c))]
              (for [i (range bands)]
                (let [y (* h (/ (+ i 1.0) (+ bands 1.0)))]
                  (part 0.0 y (- (+ hz 0.35)) (* 2 hx 0.9) 0.28 0.7
                        :box (:concrete c))))))

      :shop
      [base
       ;; Awning, fascia sign and parapet -- the three things that say "shop".
       (part 0.0 3.4 (- (+ hz 0.85)) (* 2 hx 0.94) 0.22 1.7 :box (:awning c))
       (part 0.0 4.15 (- (+ hz 0.12)) (* 2 hx 0.9) 0.9 0.28 :box (:sign c))
       (part 0.0 (+ h 0.3) 0.0 (* 2 hx 1.05) 0.7 (* 2 hz 1.05) :box (:stone c))]

      :office
      (let [pod (min (* h 0.22) 7.0)
            tw  (* hx 0.82) td (* hz 0.82)]
        [(wall 0.0 (* 0.5 pod) 0.0 (* 2 hx 1.06) pod (* 2 hz 1.06))
         (wall 0.0 (+ pod (* 0.5 (- h pod))) 0.0 (* 2 tw) (- h pod) (* 2 td))
         (part 0.0 (+ h 0.9) 0.0 (* 2 tw 1.05) 1.8 (* 2 td 1.05) :box (:glass c))
         (part 0.0 (+ h 5.0) 0.0 0.35 8.0 0.35 :cylinder (:steel c))])

      :factory
      (let [teeth (+ 3 (prng/next-int! r 3))
            tw    (/ (* 2 hx) teeth)]
        (into [base
               (part (* hx 0.72) (+ h 6.0) (* hz 0.55) 1.5 14.0 1.5
                     :cylinder (:brick c))]
              (for [i (range teeth)]
                (part (+ (- hx) (* tw (+ i 0.5))) (+ h 0.9) 0.0
                      (* tw 0.96) 1.9 (* 2 hz) :gable (:roof-metal c)))))

      :warehouse
      (let [doors (+ 2 (prng/next-int! r 2))]
        (into [base
               (ridged-roof hx hz 1.8 h (:roof-metal c) 1.04)]
              (for [i (range doors)]
                (part (* hx (- (/ (* 2.0 (+ i 0.5)) doors) 1.0)) 2.0 (- (+ hz 0.1))
                      (* hx (/ 1.3 doors)) 4.0 0.3 :box (:door c)))))

      :civic
      (into [base
             (part 0.0 (+ h (* 0.45 hx)) 0.0 (* 2 hx 1.02) (* 0.9 hx) (* 2 hz 1.02)
                   :pyramid (:roof-metal c))
             ;; Portico: a slab out front on four columns.
             (part 0.0 (- h 0.5) (- (+ hz 1.3)) (* 2 hx 0.7) 1.0 2.8 :box (:stone c))]
            (for [i (range 4)]
              (part (* hx 0.7 (- (/ (* 2.0 i) 3.0) 1.0)) (* 0.5 (- h 1.0)) (- (+ hz 1.9))
                    0.55 (- h 1.0) 0.55 :cylinder (:stone c))))

      :barn
      (let [rise (max 2.2 (* 1.35 (min hx hz)))]
        [base
         (ridged-roof hx hz rise h (:roof-tile c) 1.12)
         (part (+ hx 2.2) 4.0 0.0 3.0 8.0 3.0 :cylinder (:concrete c))
         (part (+ hx 2.2) 9.0 0.0 3.2 2.0 3.2 :pyramid (:roof-metal c))])

      [base])))

(defn chunk-structures
  "Every building in one chunk, as two flat arrays.

  `:buildings` is the coarse footprint -- [x y z hx hz height zone yaw ...] --
  and is what the physics collider is built from: one box per building rather
  than one per part, because a porch is not worth a broad-phase entry.

  `:parts` is what actually gets drawn: [x y z yaw sx sy sz prim mat tint ...],
  already transformed out of the building's own frame. `mat` is either the
  zone's facade or a flat colour in `tint`.

  Both come out of the same pass over the chunk's plots, so the mass a player
  can see and the box they collide with cannot drift apart."
  ([seed cx cz field] (chunk-structures seed cx cz field (ground-sampler seed field)))
  ([seed cx cz field ground]
  (let [r     (prng/chunk-rng seed cx cz (+ 17 (:blocks k/salt)))
        boxes (transient [])
        parts (transient [])]
    (doseq [{:keys [x z hx hz yaw zone]} (chunk-lots seed cx cz)
            :when (not= :open zone)]
      (let [zi (zone-index zone)
            {:keys [cover height]} (nth building-zones zi)
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
            sy (js-sin yaw) cy (js-cos yaw)
            bx (- x (* sy back))
            bz (- z (* cy back))
            corners [[bx bz]
                     [(- bx bhx) (- bz bhz)] [(+ bx bhx) (- bz bhz)]
                     [(- bx bhx) (+ bz bhz)] [(+ bx bhx) (+ bz bhz)]]
            gy (reduce min (map (fn [[px pz]] (ground px pz)) corners))]
        (conj! boxes bx) (conj! boxes gy) (conj! boxes bz)
        (conj! boxes bhx) (conj! boxes bhz) (conj! boxes hgt)
        (conj! boxes (double zi)) (conj! boxes yaw)
        (doseq [pt (mass-parts r zone bhx bhz hgt)]
          ;; Local -> world: rotate the part about +Y by the building's yaw.
          ;; `ly` is measured from the building's own base, which is sunk 0.6 m
          ;; so a building on a slope meets the ground on every side rather
          ;; than showing daylight under the downhill corner.
          (let [wx (+ bx (* (:lx pt) cy) (* (:lz pt) sy))
                wz (+ bz (- (* (:lx pt) sy)) (* (:lz pt) cy))]
            (conj! parts wx)
            (conj! parts (+ gy -0.6 (:ly pt)))
            (conj! parts wz)
            (conj! parts (+ yaw (:lyaw pt)))
            (conj! parts (:sx pt)) (conj! parts (:sy pt)) (conj! parts (:sz pt))
            (conj! parts (double (prim-index (:prim pt))))
            (conj! parts (if (:facade? pt) (double zi) plain-mat))
            (conj! parts (double (:tint pt)))))))
    (let [bv (persistent! boxes)
          pv (persistent! parts)
          ba (farray (count bv))
          pa (farray (count pv))]
      (dotimes [i (count bv)] (fput! ba i (nth bv i)))
      (dotimes [i (count pv)] (fput! pa i (nth pv i)))
      {:buildings ba :parts pa}))))

(defn chunk-buildings
  "Coarse footprints only. Kept as its own name because the physics side and
  the tests care about the box, not about the porch."
  [seed cx cz field]
  (:buildings (chunk-structures seed cx cz field)))

;; --- pedestrians ------------------------------------------------------------

(def ped-stride 6)           ; x y z heading speed kind

(def ped-kinds
  "What is walking about. People line the streets; the rest belong to whatever
  land they are standing on.

  Everything from `:suit` on is a person who is somewhere for a reason -- see
  `gathering`. They are kinds rather than a flag on `:person` because the
  client draws them differently and the wire already carries a kind; adding to
  the end of this vector is safe, reordering it renames every pedestrian in
  every saved world."
  [:person :sheep :cow :deer :dog
   :suit :shopper :fan :drinker :streetwalker])

(def ^:private kind-index (zipmap ped-kinds (range)))

(defn peds-per-chunk
  "How busy a chunk's pavements are.

  Graded off `urbanness` rather than the city/not-city switch this was. One
  threshold and two numbers made a suburb as empty as a moor and downtown no
  busier than an industrial estate, and the whole point of the crowd is that it
  is thicker where the streets are.

  The three city tiers came down when groups arrived. The total on a downtown
  pavement is about what it was; what changed is how it is arranged, and a
  crowd that stands in groups reads as busier than the same number of people
  spaced evenly along a kerb."
  [seed cx cz]
  (let [x (* (+ cx 0.5) k/chunk-size)
        z (* (+ cz 0.5) k/chunk-size)
        u (urbanness seed x z)]
    (cond
      (> u 0.82) 24
      (> u 0.58) 19
      (> u 0.34) 13
      (> u 0.16) 9
      (> u 0.05) 4
      :else 2)))

(def ^:private herds-per-chunk 4)
(def ^:private herd-size 7)

(def ^:private groups-per-chunk 3)
(def ^:private group-min 3)
(def ^:private group-max 6)

(defn- gathering
  "What sort of group stands about at (x, z), or nil where nobody would.

  A crowd is a consequence of what is around it, so this reads the same fields
  the buildings do rather than scattering types at random: office density puts
  suits on the pavement, a shopping centre puts shoppers outside it, a stadium
  puts supporters on the road up to it, and housing pressed hard against the
  works is where the rest of it happens. It is the difference between a street
  with people on it and a street that is somewhere.

  `lm` is the district's landmark, passed in rather than looked up: finding one
  costs a search over sixteen candidate cells and every group in a chunk is in
  the same district."
  [seed x z lm r]
  (let [u    (urbanness seed x z)
        ind  (industrialness seed x z)
        near? (and lm (< (hypot (- x (:x lm)) (- z (:z lm))) 150.0))
        p    (prng/next-double! r)]
    (cond
      (and near? (= :mall (:kind lm)))    :shopper
      (and near? (= :stadium (:kind lm))) :fan
      (and near? (= :plaza (:kind lm)))   (if (< p 0.5) :suit :shopper)
      ;; The wrong side of the tracks: housing that backs onto the works.
      ;; Neither field says this by itself -- it is the overlap that does.
      (and (> ind 0.52) (< 0.30 u 0.78))
      (cond (< p 0.34) :streetwalker (< p 0.72) :drinker :else :shopper)
      (> u 0.80) (cond (< p 0.42) :suit (< p 0.70) :shopper (< p 0.92) :drinker
                       :else :streetwalker)
      (> u 0.58) (cond (< p 0.24) :suit (< p 0.58) :shopper :else :drinker)
      (> u 0.30) (if (< p 0.55) :shopper :drinker)
      :else nil)))

(defn- grazer-for
  "What, if anything, is grazing at a point. Livestock follow the crop, deer the
  woods, dogs the suburbs -- so an animal is where the land says it should be
  rather than scattered at random over it."
  [seed x z]
  (let [u (urbanness seed x z)
        crop (crop-at seed x z)]
    (cond
      (> u 0.55) nil
      (> u 0.28) :dog
      (= crop :woodland) :deer
      (contains? #{:pasture :fallow} crop) (if (> u 0.12) :cow :sheep)
      (= crop :scrub) :sheep
      :else nil)))

(def pickup-kinds
  "What is worth driving over. Order is the wire format: a pickup travels as an
  index, and reordering these renames every crate in every saved world."
  [:repair :nitro :grip :armour :flame :shock])

(def pickup-stride 4)          ; x y z kind
(def ^:private pickups-per-chunk 3)

(defn chunk-pickups
  "Crates of something useful, sitting on the carriageway.

  On the road rather than beside it, unlike props: the whole point is that they
  are collected by driving, and a bonus you have to stop and aim at is a bonus
  nobody takes at speed. Placed on the centre line for the same reason.

  Deterministic per chunk like everything else, so two players in one world
  drive over the same crates -- and the overlay records which have been taken,
  so they do not come back when the chunk does."
  ([seed cx cz] (chunk-pickups seed cx cz (chunk-lines seed cx cz)))
  ([seed cx cz owned]
   (let [lines (remove :bridge? owned)
         r (prng/chunk-rng seed cx cz (:pickups k/salt))
         out (transient [])]
     (when (seq lines)
       (dotimes [_ pickups-per-chunk]
         (let [line (nth lines (prng/next-int! r (count lines)))
               pts (:points line)
               i (prng/next-int! r (dec (count pts)))
               t (prng/next-range! r 0.2 0.8)
               kind (prng/next-int! r (count pickup-kinds))
               [ax az] (nth pts i)
               [bx bz] (nth pts (inc i))
               x (+ ax (* t (- bx ax)))
               z (+ az (* t (- bz az)))]
           (conj! out x)
           ;; A metre up: high enough to be seen over a kerb, low enough that
           ;; any car drives through it rather than under it.
           (conj! out (+ 1.0 (height-at seed x z)))
           (conj! out z)
           (conj! out (double kind)))))
     (let [v (persistent! out)
           a (farray (count v))]
       (dotimes [i (count v)] (fput! a i (nth v i)))
       a))))

(defn chunk-peds
  "Deterministic pedestrian and animal spawns, as
  [x y z heading speed kind ...].

  People are placed close to the carriageway -- they are meant to be in the way
  -- and set walking *along* the street rather than on an arbitrary bearing,
  which is most of the difference between a crowd and a scattering.

  Animals come in herds, because one sheep in a field is a mistake and six is a
  flock. Same draw-before-you-reject discipline as props, so the random stream
  advances by the same amount on every machine regardless of what the ground
  turns out to be."
  ([seed cx cz field] (chunk-peds seed cx cz field (chunk-lines seed cx cz)))
  ([seed cx cz field owned]
   (chunk-peds seed cx cz field owned (ground-sampler seed field)))
  ([seed cx cz field owned ground]
  (let [lines (remove :bridge? owned)
        r     (prng/chunk-rng seed cx cz (:peds k/salt))
        out   (transient [])
        emit  (fn [x z head speed kind]
                (let [y (ground x z)]
                  (conj! out x) (conj! out y) (conj! out z)
                  (conj! out head) (conj! out speed)
                  (conj! out (double (kind-index kind)))))]
    (when (seq lines)
      (dotimes [_ (peds-per-chunk seed cx cz)]
        (let [line  (nth lines (prng/next-int! r (count lines)))
              pts   (:points line)
              cnt   (count pts)
              i     (prng/next-int! r (dec cnt))
              t     (prng/next-double! r)
              side  (if (prng/next-bool! r) 1.0 -1.0)
              off   (prng/next-range! r 1.0 (+ (:half line) 3.0))
              back? (prng/next-bool! r)
              speed (prng/next-range! r 0.7 1.9)
              [ax az] (nth pts i)
              [bx bz] (nth pts (inc i))
              px (+ ax (* t (- bx ax)))
              pz (+ az (* t (- bz az)))
              dx (- bx ax) dz (- bz az)
              len (max 1e-6 (hypot dx dz))
              sgn (if back? -1.0 1.0)
              ;; Along the pavement, one way or the other. `walk!` reads this as
              ;; (cos h, sin h), so it is atan2 of dz over dx.
              head (js-atan2 (* sgn (/ dz len)) (* sgn (/ dx len)))]
          (emit (+ px (* side (* (- dz) (/ off len))))
                (+ pz (* side (* dx (/ off len))))
                head speed :person)))
      ;; And the groups. Same lines and the same draw-before-you-reject
      ;; discipline: the stream advances by the same amount whether or not
      ;; there turns out to be anybody worth putting here.
      (let [[ddx ddz] (district-of cx cz)
            lm (landmark seed ddx ddz)]
        (dotimes [_ groups-per-chunk]
          (let [line  (nth lines (prng/next-int! r (count lines)))
                pts   (:points line)
                i     (prng/next-int! r (dec (count pts)))
                t     (prng/next-double! r)
                side  (if (prng/next-bool! r) 1.0 -1.0)
                ;; Further back from the kerb than a lone pedestrian: a group
                ;; stands on the pavement, it does not queue along the gutter.
                off   (prng/next-range! r (+ (:half line) 1.5) (+ (:half line) 7.0))
                size  (+ group-min (prng/next-int! r (inc (- group-max group-min))))
                [ax az] (nth pts i)
                [bx bz] (nth pts (inc i))
                px (+ ax (* t (- bx ax)))
                pz (+ az (* t (- bz az)))
                dx (- bx ax) dz (- bz az)
                len (max 1e-6 (hypot dx dz))
                gx (+ px (* side (* (- dz) (/ off len))))
                gz (+ pz (* side (* dx (/ off len))))
                kind (gathering seed gx gz lm r)]
            (dotimes [_ size]
              (let [ox (prng/next-range! r -2.2 2.2)
                    oz (prng/next-range! r -2.2 2.2)
                    ;; Barely moving. A group that walks is a queue.
                    speed (prng/next-range! r 0.0 0.45)]
                (when kind
                  ;; Facing the middle of their own group, which is the only
                  ;; thing separating a group from six people who happen to be
                  ;; standing near each other.
                  (emit (+ gx ox) (+ gz oz)
                        (js-atan2 (- oz) (- ox))
                        speed kind))))))))
    (dotimes [_ herds-per-chunk]
      (let [hx (* (+ cx (prng/next-range! r 0.1 0.9)) k/chunk-size)
            hz (* (+ cz (prng/next-range! r 0.1 0.9)) k/chunk-size)
            kind (grazer-for seed hx hz)]
        (dotimes [_ herd-size]
          (let [ox (+ hx (prng/next-range! r -11.0 11.0))
                oz (+ hz (prng/next-range! r -11.0 11.0))
                head (prng/next-range! r 0.0 6.2831853)
                speed (prng/next-range! r 0.15 0.55)]
            (when (and kind (< (second (surface seed field ox oz)) 0.4))
              (emit ox oz head speed kind))))))
    (let [v (persistent! out)
          a (farray (count v))]
      (dotimes [i (count v)] (fput! a i (nth v i)))
      a))))

;; --- junctions and street furniture ----------------------------------------

(def ^:private class-rank {:local 0 :collector 1 :arterial 2})

(defn node-arms
  "The streets meeting at lattice node (gx, gz): a unit direction away from the
  node, the class of that street, how far off the ground it is, and the lattice
  node at its far end.

  Public because traffic routes on it. A driver arriving at a node asks what
  leaves it and picks one, which is all the navigation an infinite world can
  support -- there is no destination to plan a route to.

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
                       :class (edge-class ogx ogz along-x?)
                       :lift (node-lift seed gx gz along-x?)
                       :to [(+ gx dgx) (+ gz dgz)]
                       :along-x? along-x?}))))
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
  ;; Only the arms on the ground make a junction. Where an expressway is
  ;; overhead the traffic below simply passes under it, and signalling a
  ;; crossing that does not exist would hang lights in mid-air.
  (let [arms (filterv #(zero? (:lift %)) (node-arms seed gx gz))
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
  ([seed cx cz field] (chunk-furniture seed cx cz field (chunk-lines seed cx cz)))
  ([seed cx cz field owned]
   (chunk-furniture seed cx cz field owned (ground-sampler seed field)))
  ([seed cx cz field owned ground]
  (let [out (transient [])]
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
    (doseq [{:keys [points half class bridge? ya yb]} owned]
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
                    pz (+ az (* uz t))
                    ;; On a span the ground is the riverbed, so the paint has to
                    ;; follow the deck's chord instead.
                    y (if bridge?
                        (+ ya (* (/ t len) (- yb ya)))
                        (ground px pz))]
                (emit! out px (+ 0.02 y) pz yaw
                       (part-index :marking) 1.0 0 0.0)))))
        (when (and (not bridge?) (> u lamp-urbanness) (> len 1.0))
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
      a))))

;; --- chunk assembly ---------------------------------------------------------

;; Vertex colour multiplies the tiled ground texture, which is green. Simply
;; darkening it for roads gives dark grass, not asphalt -- so the road colour has
;; to actively cancel the texture's hue (green down, red and blue up) to land on
;; neutral grey.
(def ^:private road-colour-r 0.42)
(def ^:private road-colour-g 0.27)
(def ^:private road-colour-b 0.52)

;; An unmade lane: pale, dry and brown rather than dark and neutral.
(def ^:private track-colour-r 1.05)
(def ^:private track-colour-g 0.80)
(def ^:private track-colour-b 0.58)

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
        ;; Every generator below wants this chunk's own streets. Six of them
        ;; used to work it out independently, which meant building the same
        ;; fifty streets six times and was, by the end, most of the cost of a
        ;; chunk.
        owned (chunk-lines seed cx cz)
        heights (farray (* n n))
        colors  (farray (* n n 3))]
    ;; The ground comes first, and everything else is then placed on it. That
    ;; ordering is the point: the heightfield is what the collider is built from
    ;; and what the mesh is drawn from, so a lamp post standing on it stands
    ;; where the car can actually drive rather than up to 0.14 m away from it.
    ;; It is also four array reads instead of a road lookup and a four-octave
    ;; fbm, which is most of what a chunk used to cost.
    (dotimes [j n]                       ; j indexes z
      (dotimes [i n]                     ; i indexes x
        (let [x (+ x0 (* i step))
              z (+ z0 (* j step))
              rv (river seed x z)
              [y road paved] (surface-detail seed field x z rv)
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
              tr   (+ tr (* u (- (* gr 1.34) tr)))
              tg   (+ tg (* u (- (* gr 0.98) tg)))
              tb   (+ tb (* u (- (* gr 1.46) tb)))
              ;; Water. The ground texture is green, so as with the roads the
              ;; tint has to actively cancel that hue to read as anything else.
              tr   (+ tr (* rv (- (* gr 0.34) tr)))
              tg   (+ tg (* rv (- (* gr 0.58) tg)))
              tb   (+ tb (* rv (- (* gr 1.30) tb)))
              ;; Farmland. Fields fade out as the ground builds up, so the
              ;; patchwork stops at the edge of town rather than running under
              ;; it, and dry land only -- a river does not grow wheat.
              farm (* (- 1.0 u) (- 1.0 rv))
              [cr cg cb] (nth crop-tints (field-index seed x z))
              tr   (* tr (+ 1.0 (* farm (- cr 1.0))))
              tg   (* tg (+ 1.0 (* farm (- cg 1.0))))
              tb   (* tb (+ 1.0 (* farm (- cb 1.0))))
              ;; Out in the country a lane is a dirt track and a main road is
              ;; still tarmac, which is what `paved` in the segment array is for.
              dirt-road (* (- 1.0 u) (- 1.0 paved))
              rr   (+ road-colour-r (* dirt-road (- track-colour-r road-colour-r)))
              rg   (+ road-colour-g (* dirt-road (- track-colour-g road-colour-g)))
              rb   (+ road-colour-b (* dirt-road (- track-colour-b road-colour-b)))
              o    (* idx 3)]
          (fput! heights idx y)
          (fput! colors (+ o 0) (+ tr (* road (- rr tr))))
          (fput! colors (+ o 1) (+ tg (* road (- rg tg))))
          (fput! colors (+ o 2) (+ tb (* road (- rb tb)))))))
    (let [ground (heightfield-sampler heights n x0 z0 step seed field)
          props (chunk-props seed cx cz field owned ground)
          {:keys [buildings parts]} (chunk-structures seed cx cz field ground)
          peds  (chunk-peds seed cx cz field owned ground)
          furniture (chunk-furniture seed cx cz field owned ground)
          bridges (chunk-bridges seed cx cz owned)
          flora (chunk-flora seed cx cz field)
          landmarks (chunk-landmarks seed cx cz)
          pickups (chunk-pickups seed cx cz owned)
          traffic (chunk-traffic seed cx cz owned)]
    {:cx cx :cz cz :verts n :size k/chunk-size
     :origin [x0 z0]
     :heights heights
     :colors colors
     :props props
     :buildings buildings
     :building-parts parts
     :peds peds
     :furniture furniture
     :bridges bridges
     :flora flora
     :landmarks landmarks
     :pickups pickups
     :traffic traffic
     :biome (biome seed cx cz)})))

(defn road-point-near
  "A point on the street network near `[x z]`, and which way that street runs:
  `{:pos [x y z] :dir [dx dz]}`, or nil if no chunk within `rings` owns a road.

  Rings outward from the chunk the point is in rather than sampling around it: a
  chunk in open country may own no street at all, and the nearest one can be
  several hundred metres away.

  The point returned is the *middle* of a street, never an end. That is not
  cosmetic -- whatever is placed here is placed facing along `dir` and usually
  has other things queued up behind it, and a point chosen near a junction puts
  those on the pavement."
  ([seed x z] (road-point-near seed x z 4))
  ([seed x z rings]
   (let [[cx0 cz0] (chunk-of x z)
         ring (for [d (range 0 rings)
                    dx (range (- d) (inc d))
                    dz (range (- d) (inc d))
                    :when (= d (max (abs dx) (abs dz)))]
                [(+ cx0 dx) (+ cz0 dz)])
         ;; Bridge decks are excluded: a car dropped onto one lands on a
         ;; structure that only exists while that chunk is loaded.
         streets (fn [[cx cz]] (seq (remove :bridge? (chunk-lines seed cx cz))))]
     (when-let [ss (first (keep streets ring))]
       (let [;; Nearest by midpoint, so a rival respawned "near the player"
             ;; arrives on the closest street rather than an arbitrary one.
             mid   (fn [{:keys [points]}]
                     (let [i0 (max 0 (dec (quot (count points) 2)))
                           i1 (min (dec (count points)) (inc i0))]
                       [(nth points i0) (nth points i1)]))
             best  (apply min-key
                          (fn [s]
                            (let [[[ax az] [bx bz]] (mid s)]
                              (hypot (- (* 0.5 (+ ax bx)) x)
                                     (- (* 0.5 (+ az bz)) z))))
                          ss)
             [[ax az] [bx bz]] (mid best)
             px  (* 0.5 (+ ax bx))
             pz  (* 0.5 (+ az bz))
             len (max 1e-6 (hypot (- bx ax) (- bz az)))]
         {:pos [px (+ 1.2 (height-at seed px pz)) pz]
          :dir [(/ (- bx ax) len) (/ (- bz az) len)]})))))

(defn spawn-point
  "Somewhere on the street network near the origin, and which way that street
  runs: {:pos [x y z] :dir [dx dz]}.

  The direction matters as much as the point. Opponents have to line up along
  the carriageway, because since buildings started standing on real plots a ring
  of cars around the spawn puts most of them inside one.

  Takes a street rather than a lattice node: a node can be a dead end in sparse
  country, whereas the middle of a street that exists is by definition on a
  road."
  [seed]
  (or (road-point-near seed 0.0 0.0 8)
      {:pos [0.0 (+ 1.2 (height-at seed 0.0 0.0)) 0.0] :dir [0.0 -1.0]}))
