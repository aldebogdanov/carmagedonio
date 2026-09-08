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
       ;; How made-up the surface is, 0 for a dirt lane and 1 for tarmac.
       ;;
       ;; Decided here, where the urbanness under the street is already known,
       ;; and not from the class alone -- which is what it was. A back street
       ;; in the middle of a city was classified exactly like a farm track and
       ;; drawn as one, so every low-grade road in every town came out the
       ;; colour of mud. A lane is a dirt track because of where it is, not
       ;; because of what it is called.
       :paved    (case cls
                   :arterial  1.0
                   :collector (max 0.65 (min 1.0 (* 1.5 u)))
                   (min 1.0 (* 1.7 u)))
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

(defn cell-streets
  "The streets lattice cell (gx, gz) owns: the one leaving it along +X and the
  one along +Z, where they exist.

  The unit `streets-in-bounds` is built from, and public because a caller that
  redraws the same ground repeatedly -- the map -- wants to cache per cell
  rather than per query box. A cell's streets are a function of the seed and
  never change, so caching them is caching forever."
  [seed gx gz]
  (cond-> []
    (edge-exists? seed gx gz true)  (conj (street seed gx gz true))
    (edge-exists? seed gx gz false) (conj (street seed gx gz false))))

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
      (doseq [s (cell-streets seed gx gz)] (conj! out s)))
    (persistent! out)))

;; --- road field: the streets near one chunk, indexed for lookup -------------

(def ^:private seg-stride 9)   ; x1 z1 y1 x2 z2 y2 half shoulder paved

(defn- segments-of
  "Flatten streets into [x1 z1 y1 x2 z2 y2 half shoulder paved ...].

  `paved` is 0 for a lane and 1 for tarmac, and it is decided by `street` from
  the urbanness under it rather than from the class. It rides along in the
  segment array because the ground needs it: out in the country a local street
  is a dirt track and a main road is still tarmac, and the only thing that
  knows which is which by the time the colour is chosen is the segment that
  won.

  Flat and typed because `road-at` runs about a thousand times per chunk against
  this array; boxed vector access shows up plainly in a profile."
  [streets]
  (let [out (transient [])]
    (doseq [{:keys [points half shoulder ya yb paved]} streets]
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

;; --- regions ----------------------------------------------------------------
;;
;; Two more fields, coarser than the city one but not by much: how warm a place
;; is and how wet it is. A region comes out about two kilometres across, which
;; is a couple of districts -- a minute's driving, so a long run passes through
;; several rather than staying in one.
;;
;; The first version made them ten kilometres across on the theory that a
;; region should be somewhere you drive *out of* rather than past. That is true
;; of a real country and wrong for this: at 200 km/h a ten-kilometre region is
;; three minutes of identical scenery, and a whole session can be spent inside
;; one without ever learning that the others exist.
;;
;; What a region changes is deliberately only the countryside: what grows,
;; what colour the ground is, and which landmark a village gets. A city is a
;; city everywhere, and trying to make downtown regional would mean four sets
;; of facades for something you see from inside a car at 90 km/h anyway.

(def region-kinds [:heartland :taiga :sierra :paddies])

(def region-labels
  {:heartland "the heartland" :taiga "the taiga" :sierra "the sierra"
   :paddies "the paddies"})

(def ^:private region-scale (* 8.0 k/chunk-size))    ; ~2 km
(def ^:private region-lod 64.0)   ; m between ground-colour samples

(defn warmth
  "How warm a point is, in fbm's own [0,1]. One of the two axes a region is
  read off."
  [seed x z]
  (noise/fbm2d (+ seed 9311) (/ x region-scale) (/ z region-scale) 2))

(defn damp
  "How wet a point is -- the other axis. Independent of `river`, which is a
  channel; this is a climate."
  [seed x z]
  (noise/fbm2d (+ seed 4177) (/ x region-scale) (/ z region-scale) 2))

(defn region-of
  "Which region a (warmth, damp) pair falls in.

  Split out from `region` because the ground tint interpolates the two fields
  across a chunk rather than sampling them at every vertex, and has to bucket
  the interpolated pair the same way this does or the colour and the trees
  would disagree about where the taiga starts."
  [w m]
  ;; Thresholds are percentiles of the two fields, measured, not guessed: fbm
  ;; sits well below 0.5 at the median, and a nominal-looking 0.44 for "cold"
  ;; made nearly half the world taiga.
  ;;
  ;; Warm country is always one of the two warm regions rather than sometimes
  ;; falling back to the heartland. With the earlier, laxer version the
  ;; heartland took over half the map, which meant most of the countryside was
  ;; the one region with nothing to say about itself.
  (cond
    (< w 0.33) :taiga
    (> w 0.52) (if (< m 0.48) :sierra :paddies)
    :else :heartland))

(defn region [seed x z] (region-of (warmth seed x z) (damp seed x z)))

(def ^:private region-ground
  "What a region does to its own open ground: how hard to pull, and where to.

  A lerp towards a target rather than a multiplier over the crop tint. That was
  the first attempt and it does not work, for the same reason it did not work
  for the industrial estates: the ground texture is painted green, a vertex
  colour multiplies it, and multiplying a green by numbers near one gives a
  lighter green however the numbers are chosen. Changing the *hue* needs a
  colour that actively cancels it, which means a lerp towards one.

  The strength is what keeps the field patchwork: at 1.0 a region would be one
  flat wash, and the patchwork is most of what makes farmland read as farmland.
  The sierra pulls hardest because sand is the furthest from green of the
  three, and it is the region a player is most likely to be able to name.

  Every number here is the colour wanted divided by the ground texture's, and
  divided by it *after gamma*. That second step is the one that took three
  attempts: the texture is sRGB and three.js linearises it before multiplying,
  which stretches its green lead over its red from 1.24 to 1.58. A vertex
  colour that beats 1.24 and reads as sand on paper still comes out olive on
  screen, which is exactly what the first two sets of numbers here did."
  {:heartland [0.0  1.00 1.00 1.00]
   :taiga     [0.50 1.15 0.80 1.05]
   :sierra    [1.00 2.45 0.78 1.06]
   :paddies   [0.50 0.72 0.86 0.46]})

;; The three entries above, resolved once. Looking a keyword up in a map and
;; destructuring the vector it returns is not free at terrain-vertex rate.
(def ^:private rg-sierra (region-ground :sierra))
(def ^:private rg-paddies (region-ground :paddies))
(def ^:private rg-taiga (region-ground :taiga))

(defn- region-blend!
  "Write the ground treatment at a (warmth, damp) pair into `out` as
  [strength r g b].

  `region-of` draws a hard line, which is right for a tree -- it is a birch or
  it is a cactus -- and wrong for the ground, where it drew a visible contour
  across the terrain wherever warmth happened to cross the threshold. The
  weights below are the same three conditions with soft edges, so the sand
  fades into the grass over a couple of hundred metres the way it does on the
  ground.

  It writes into a caller-owned array instead of returning a vector because
  `chunk-data` evaluates it twenty-five times per chunk and has nowhere to put
  the garbage."
  [^doubles out w m]
  (let [warm (smootherstep-clamped (/ (- w 0.46) 0.12))
        cold (smootherstep-clamped (/ (- 0.39 w) 0.12))
        wet  (smootherstep-clamped (/ (- m 0.44) 0.10))
        ws   (* warm (- 1.0 wet) (- 1.0 cold))
        wp   (* warm wet (- 1.0 cold))
        wt   cold
        tot  (+ ws wp wt)]
    (if (< tot 1.0e-4)
      (do (dput! out 0 0.0) (dput! out 1 1.0)
          (dput! out 2 1.0) (dput! out 3 1.0))
      ;; The colour is normalised by the total weight, so it is an average of
      ;; whichever regions are in play; the strength is not, so it still falls
      ;; to zero out in the heartland where none of them are.
      (do (dput! out 0 (min 1.0 (+ (* ws (nth rg-sierra 0))
                                   (* wp (nth rg-paddies 0))
                                   (* wt (nth rg-taiga 0)))))
          (dotimes [c 3]
            (let [k (inc c)]
              (dput! out k (/ (+ (* ws (nth rg-sierra k))
                                 (* wp (nth rg-paddies k))
                                 (* wt (nth rg-taiga k)))
                              tot))))))
    out))

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
   :conifer 0x2c4f2e
   ;; Regional species. A birch trunk is the one tree anybody can name at
   ;; distance, and it is the whole reason the taiga reads as somewhere else.
   :birch   0xd6d2c4
   :cactus  0x4a7c46
   :palm    0x7b6240
   :frond   0x6f9a34})

(def ^:private leaf-tints [0x3f6b32 0x4a7a38 0x35602c 0x54803c])

;; How likely a candidate position is to actually grow something, by crop. A
;; wood is a wood because the parcel says so, not because a density field
;; happened to peak there -- which is what keeps the tree line following the
;; field boundary the way a real one does.
(def ^:private crop-tree-odds
  {:woodland 0.88 :orchard 0.80 :scrub 0.17 :pasture 0.05
   :fallow 0.07 :wheat 0.012 :plough 0.008 :rape 0.010})

;; How much of that a region actually grows. The sierra is the point of this
;; table: scrub at the heartland's odds is not a desert, it is a slightly thin
;; wood, and a cactus every eleven metres reads as an orchard of cacti.
(def ^:private region-tree-odds
  {:heartland 1.0 :taiga 1.15 :sierra 0.45 :paddies 1.10})

(defn- tree-parts!
  "A trunk and a canopy at (x, y, z). The trunk is solid and the canopy is not:
  a tree stops a car, and its branches are for driving through.

  Four species, one per region. Everything below is the same two or three
  volumes rearranged -- which is the only budget a tree has when there are two
  hundred of them in view -- but trunk colour and canopy shape between them are
  enough that you can tell where you are without reading the map."
  [emit seed x y z h region]
  (let [k (prng/hash-coords (+ seed 3313) (long (* x 4.0)) (long (* z 4.0)))
        pick (bit-and (prng/shr32 k 5) 3)
        leaf (nth leaf-tints (bit-and (prng/shr32 k 9) 3))
        stem (fn [tint frac girth]
               (emit x (+ y (* 0.5 frac h)) z 0.0 0.0
                     (* girth h) (* frac h) (* girth h)
                     :cylinder tint 1.0))]
    (case region
      ;; Birch and spruce. The birches are pale and narrow and the spruces are
      ;; nearly black, so a taiga wood is striped rather than uniform.
      :taiga
      (if (< pick 2)
        (do (stem (:conifer flora-tint) 0.26 0.09)
            (emit x (+ y (* 0.26 h) (* 0.37 h)) z 0.0 0.0
                  (* 0.36 h) (* 0.74 h) (* 0.36 h)
                  :pyramid (:conifer flora-tint) 0.0))
        (do (stem (:birch flora-tint) 0.55 0.055)
            (emit x (+ y (* 0.55 h) (* 0.24 h)) z 0.0 0.0
                  (* 0.40 h) (* 0.48 h) (* 0.40 h) :blob leaf 0.0)))

      ;; A saguaro: one column and one or two arms. No canopy at all, which is
      ;; most of why the sierra looks empty even where things are growing.
      :sierra
      (let [ah (* 0.42 h)]
        (stem (:cactus flora-tint) 1.0 0.11)
        (emit (+ x (* 0.16 h)) (+ y (* 0.62 h)) z 0.0 0.0
              (* 0.30 h) (* 0.09 h) (* 0.09 h) :box (:cactus flora-tint) 0.0)
        (emit (+ x (* 0.29 h)) (+ y (* 0.62 h) (* 0.5 ah)) z 0.0 0.0
              (* 0.09 h) ah (* 0.09 h) :cylinder (:cactus flora-tint) 0.0)
        (when (odd? pick)
          (emit (- x (* 0.14 h)) (+ y (* 0.50 h)) z 0.0 0.0
                (* 0.26 h) (* 0.09 h) (* 0.09 h) :box (:cactus flora-tint) 0.0)
          (emit (- x (* 0.25 h)) (+ y (* 0.50 h) (* 0.18 h)) z 0.0 0.0
                (* 0.09 h) (* 0.36 h) (* 0.09 h)
                :cylinder (:cactus flora-tint) 0.0)))

      ;; A palm: all trunk, with the crown pushed out sideways rather than up.
      :paddies
      (do (stem (:palm flora-tint) 0.82 0.055)
          (emit x (+ y (* 0.86 h)) z 0.0 0.0
                (* 0.95 h) (* 0.16 h) (* 0.95 h) :blob (:frond flora-tint) 0.0)
          (emit x (+ y (* 0.80 h)) z 0.0 0.0
                (* 0.16 h) (* 0.14 h) (* 0.16 h) :blob (:frond flora-tint) 0.0))

      ;; The heartland keeps what was here before: broadleaf, one conifer in
      ;; four.
      (let [conifer? (zero? pick)
            r (* h (if conifer? 0.20 0.30))
            trunk (* h (if conifer? 0.30 0.45))]
        (emit x (+ y (* 0.5 trunk)) z 0.0 0.0
              (* 0.28 h 0.5) trunk (* 0.28 h 0.5)
              :cylinder (:trunk flora-tint) 1.0)
        (if conifer?
          (emit x (+ y trunk (* 0.5 (- h trunk))) z 0.0 0.0
                (* 2.0 r) (- h trunk) (* 2.0 r) :pyramid (:conifer flora-tint) 0.0)
          (emit x (+ y trunk (* 0.5 (- h trunk))) z 0.0 0.0
                (* 2.0 r) (- h trunk) (* 2.2 r) :blob leaf 0.0))))))

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
          (let [reg  (region seed x z)
                odds (* (get crop-tree-odds crop 0.0)
                        (get region-tree-odds reg 1.0))
                roll (/ (bit-and (prng/shr32 h 16) 0x3ff) 1024.0)]
            (when (< roll odds)
              (let [u (urbanness seed x z)
                    [y road] (surface seed field x z)]
                ;; Nothing grows downtown, in the river, or on the verge where
                ;; the lamp posts are.
                (when (and (< u tree-urban) (< (river seed x z) 0.3) (< road 0.12))
                  (let [hh (+ 5.0 (* 6.0 (/ (bit-and (prng/shr32 h 26) 0x3f) 63.0)))]
                    (tree-parts! emit seed x y z hh reg)))))))))
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
   {:name :barn      :cover 0.45 :height [5.0 8.5]}
   ;; Heavy industry. Appended, because the index into this vector travels in
   ;; the buildings array and reordering it rebuilds every city ever generated.
   ;;
   ;; An industrial estate was a district of identical sheds: `:factory` and
   ;; `:warehouse` are light production and stay exactly as they were, but a
   ;; works is not a bigger shed -- it is tanks, a stack you can see from two
   ;; districts away, and a gantry. Low and wide, because the height is in the
   ;; plant rather than in the building.
   {:name :plant     :cover 0.86 :height [7.0 12.0]}
   ;; And the yards between them, which were empty ground.
   {:name :yard      :cover 0.92 :height [2.4 4.0]}])

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
      ;; Heavy plant at the core of an estate, light units around its edge.
      ;; That gradient is what makes an industrial area read as one place
      ;; rather than as a lot of the same shed.
      (if (> ind 0.80)
        (cond (< p 0.32) :plant (< p 0.54) :yard (< p 0.80) :factory :else :warehouse)
        (cond (< p 0.40) :factory (< p 0.74) :warehouse (< p 0.88) :yard :else :plant))

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
  [:stadium :mall :park :plaza :works :silos :church :monument :mast
   :tower :station :museum :funfair :school :refinery :scrapyard
   :windmill :water-tower :ruins :drive-in
   :airport :bazaar :statue :speedway :windfarm :cemetery :quarry
   :izbas :cantina :pagoda])

(def landmark-labels
  {:stadium "the stadium" :mall "the shopping centre" :park "the park"
   :plaza "the plaza" :works "the works" :silos "the grain silos"
   :church "the church" :monument "the standing stones"
   :mast "the transmitter" :tower "the tower" :station "the station"
   :museum "the museum" :funfair "the funfair" :school "the school"
   :refinery "the refinery" :scrapyard "the scrapyard"
   :windmill "the windmill" :water-tower "the water tower"
   :ruins "the ruins" :drive-in "the drive-in"
   :airport "the airfield" :bazaar "the flea market" :statue "the statue"
   :speedway "the speedway" :windfarm "the wind farm"
   :cemetery "the cemetery" :quarry "the quarry"
   :izbas "the log village" :cantina "the cantina" :pagoda "the pagoda"})

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
  [kind region r]
  (let [pick (fn [ks] (nth ks (prng/next-int! r (count ks))))
        ;; What the region builds, and only out of town: a hamlet of log huts
        ;; is a landmark in the taiga and a mistake in the middle of a city.
        ;; Weighted at two entries so a region reads as itself without the
        ;; countryside becoming one repeated building.
        home (case region
               :taiga :izbas :sierra :cantina :paddies :pagoda nil)
        rural (fn [ks] (pick (if home (into [home home] ks) ks)))]
    (case kind
      :downtown (pick [:tower :tower :station :museum :statue :stadium :plaza
                       :mall])
      :city     (pick [:tower :station :museum :funfair :statue :bazaar
                       :stadium :mall :park :plaza])
      :suburb   (pick [:school :funfair :cemetery :bazaar :water-tower :mall
                       :park :church :museum])
      :industry (pick [:works :refinery :scrapyard :quarry])
      :village  (rural [:church :park :school :funfair :bazaar :cemetery
                        :windmill :water-tower])
      :farm     (rural [:silos :mast :windmill :water-tower :drive-in :airport
                        :windfarm])
      ;; Open country is most of the world, so it needs more than one answer or
      ;; half the landmarks anywhere are the same ring of stones.
      :woods    (rural [:monument :mast :ruins :windmill :cemetery :quarry])
      :wild     (rural [:monument :mast :ruins :drive-in :scrapyard :silos
                        :airport :speedway :windfarm :quarry])
      :monument)))

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
                {:kind   (landmark-for-place (area-kind seed cx cz)
                                            (region seed x z) r)
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
   :red 0xa33b30 :glass 0x7c98ad :rust 0x7b4630 :sign 0xd8b23c
   :sand 0xc0ad8c :adobe 0xc19a6e :lacquer 0x8e2f26 :gold 0xc9a63c
   :bronze 0x6f6244 :fur 0x5b4130 :log 0x8a6b46 :rubber 0x2a2a2c
   :cactus 0x4a7c46 :cloth-a 0xc4442f :cloth-b 0x2f7fa8 :cloth-c 0xd8a63a
   :cloth-d 0x4a8c52})

(defn- lp
  "One landmark part, in the cell's own frame: centre at (0,0), y from the
  ground the landmark was levelled to."
  ([x y z sx sy sz prim tint] (lp x y z 0.0 sx sy sz prim tint 1.0))
  ([x y z yaw sx sy sz prim tint solid]
   {:x x :y y :z z :yaw yaw :pitch 0.0 :sx sx :sy sy :sz sz :prim prim
    :tint (landmark-tints tint) :solid solid}))

(def rotor-stride 10)  ; pivot-x pivot-y pivot-z axis rate mode amp phase first count

(defn- spinning
  "Mark a run of parts as moving together about `axis` through `pivot`.

  Everything in the world is a static instance uploaded once, which is right
  for a building and wrong for a windmill: a mill whose sails do not move is
  not a mill, it is a monument to one. Rather than give every part in the world
  six more floats it will never use -- flora is by far the largest user of this
  array and none of it moves -- the moving parts are described separately, as
  runs: where in the parts array the run starts, and what it does.

  Four modes, and between them they cover nearly everything in the world that
  is not a vehicle:

    :rigid  turns about the pivot, orientation and all -- sails, blades,
            clock hands, a radar
    :orbit  goes round the pivot the right way up -- a ferris wheel's
            gondolas, a carousel's horses. Drawing those rigidly puts the
            passengers at the top upside down
    :swing  the same rotation, but `amp` radians either side of where it
            started instead of all the way round -- a windsock, a hanging
            sign, the beam of a pump jack
    :blink  no rotation at all: present for `amp` of each cycle and gone for
            the rest. A red lamp on a mast is the only thing in the catalogue
            visible from a kilometre away at night

  `phase` offsets the cycle, which is what keeps the two hands of a clock and
  the lamps down a runway from moving in lockstep.

  `pivot` is in the landmark's own frame; `chunk-landmarks` moves it into the
  world along with everything else."
  [parts {:keys [rate axis mode pivot amp phase]}]
  (let [sp {:rate rate :axis (or axis :z) :mode (or mode :rigid) :pivot pivot
            :amp (or amp 1.0) :phase (or phase 0.0)}]
    (map #(assoc % :spin sp) parts)))

(defn- tilt
  "The same part, tipped about its own X axis.

  Rotations are applied yaw first, so a part yawed a quarter turn and then
  tilted stands in the world's XY plane -- which is the only way to build a
  spoke, a sail or anything else that leans out of the horizontal, since the
  wire format carries one angle per axis and no full basis."
  [p a]
  (assoc p :pitch a))

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
    (lp 0.0 45.0 0.0 0.0 1.4 90.0 1.4 :cylinder :metal 1.0)]
   ;; The warning lamp. Ninety metres up and the only thing in the catalogue
   ;; you can navigate by after dark, which it cannot do if it is a red stick
   ;; that never changes.
   (spinning [(lp 0.0 92.0 0.0 0.0 1.5 4.0 1.5 :cylinder :red 0.0)]
             {:mode :blink :rate 0.55 :amp 0.42 :pivot [0.0 92.0 0.0]})
   [
    ;; The compound: a hut and a fence you can drive through the middle of.
    (lp (* 0.4 hx) 2.0 (* 0.4 hz) 0.0 8.0 4.0 6.0 :box :white 1.0)]
   (for [[x z a] (ring 3 5.5 5.5)]
     (lp x 22.0 z a 0.8 44.0 0.8 :box :metal 1.0))
   (for [y [18.0 40.0 66.0]]
     (lp 0.0 y 0.0 0.0 7.0 0.8 7.0 :box :metal 0.0))))

(def ^:private quarter (* 0.25 tau))

;; The nine originals were one per area kind and no more, so a district read as
;; its category rather than as a place: every industrial district in the world
;; was the same works. What follows is the second answer for each kind -- and
;; the third and fourth for open country, which is most of the map.

(defmethod landmark-shapes :tower [_ hx hz r]
  (let [w  (* 0.30 hx)
        d  (* 0.30 hz)
        ;; Three setbacks. The steps are the whole point: a single extrusion of
        ;; the same volume reads as a block of flats however tall it is.
        h1 (prng/next-range! r 34.0 46.0)]
    (concat
     [(apron hx hz :stone)
      (lp 0.0 0.16 0.0 0.0 (* 1.85 hx) 0.32 (* 1.85 hz) :box :stone 0.0)
      (lp 0.0 (* 0.5 h1) 0.0 0.0 (* 2.0 w) h1 (* 2.0 d) :box :glass 1.0)
      (lp 0.0 (+ h1 14.0) 0.0 0.0 (* 1.5 w) 28.0 (* 1.5 d) :box :glass 1.0)
      (lp 0.0 (+ h1 34.0) 0.0 0.0 w 12.0 d :box :concrete 1.0)
      (lp 0.0 (+ h1 41.0) 0.0 0.0 (* 1.15 w) 2.0 (* 1.15 d) :box :metal 0.0)
      (lp 0.0 (+ h1 52.0) 0.0 0.0 0.6 20.0 0.6 :cylinder :metal 0.0)]
     (spinning [(lp 0.0 (+ h1 63.0) 0.0 0.0 1.4 1.6 1.4 :blob :red 0.0)]
               {:mode :blink :rate 0.7 :amp 0.35 :pivot [0.0 (+ h1 63.0) 0.0]})
     ;; Planters, which is what the plaza round the foot of one is made of.
     (for [[x z a] (ring 6 (* 0.88 hx) (* 0.88 hz))]
       (lp x 0.9 z a 4.5 1.8 4.0 :box :stone 1.0)))))

(defmethod landmark-shapes :station [_ hx hz r]
  (let [len (* 1.6 hx)]
    (concat
     [(apron hx hz :concrete)
      ;; The shed: two long walls with a roof over the tracks between them.
      (lp 0.0 5.0 (* -0.62 hz) 0.0 len 10.0 2.0 :box :brick 1.0)
      (lp 0.0 5.0 (* 0.62 hz) 0.0 len 10.0 2.0 :box :brick 1.0)
      (lp 0.0 11.5 0.0 quarter (* 1.32 hz) 5.0 len :gable :metal 0.0)
      ;; Head house and clock tower: the half of a station that faces the road
      ;; rather than the track, and the half you navigate by.
      (lp (* -0.74 hx) 4.5 (* 0.15 hz) 0.0 (* 0.4 hx) 9.0 (* 1.2 hz)
          :box :stone 1.0)
      (lp (* -0.74 hx) 13.5 (* 0.6 hz) 0.0 7.0 27.0 7.0 :box :stone 1.0)
      (lp (* -0.74 hx) 28.5 (* 0.6 hz) 0.0 7.6 3.5 7.6 :pyramid :roof 0.0)
      ;; Island platform, with a road either side of it.
      (lp 0.0 0.6 0.0 0.0 len 1.2 5.0 :box :concrete 1.0)
      ;; And the clock. A tower with a blank face on it is a tower; a tower
      ;; with hands on it is a station, and it is the one clock in the world
      ;; that anybody can read at two hundred metres.
      (tilt (lp (* -0.74 hx) 21.5 (+ (* 0.6 hz) 3.5) 0.0 5.0 0.5 5.0
                :cylinder :white 0.0) quarter)]
     ;; Clockwise, hence the negative rates: rotation about +Z is
     ;; anticlockwise seen from +Z, which is the side the face is on.
     (spinning [(lp (* -0.74 hx) (+ 21.5 0.9) (+ (* 0.6 hz) 3.9) quarter
                    0.24 1.8 0.24 :box :dark 0.0)]
               {:mode :rigid :rate -0.0175
                :pivot [(* -0.74 hx) 21.5 (+ (* 0.6 hz) 3.9)]})
     (spinning [(lp (* -0.74 hx) (+ 21.5 1.3) (+ (* 0.6 hz) 4.0) quarter
                    0.18 2.6 0.18 :box :dark 0.0)]
               {:mode :rigid :rate -0.209
                :pivot [(* -0.74 hx) 21.5 (+ (* 0.6 hz) 4.0)]})
     (for [z [(* -0.38 hz) (* 0.38 hz)]]
       (lp 0.0 0.14 z 0.0 len 0.28 3.4 :box :dark 0.0))
     ;; Something standing at the near platform, because an empty station is a
     ;; shed.
     (for [i (range 3)]
       (lp (* (- (double i) 1.0) 0.34 len) 2.8 (* -0.38 hz) 0.0
           (* 0.3 len) 4.4 3.0 :box :red 1.0)))))

(defmethod landmark-shapes :museum [_ hx hz r]
  (let [w (* 1.15 hx) d (* 0.85 hz)]
    (concat
     [(apron hx hz :stone)
      (lp 0.0 0.16 0.0 0.0 (* 1.85 hx) 0.32 (* 1.85 hz) :box :stone 0.0)
      ;; Steps, terrace, block, cornice -- the order a classical front is read
      ;; in, and the reason it does not need a sign on it.
      (lp 0.0 0.7 (* 0.95 hz) 0.0 (* 0.8 w) 1.4 (* 0.28 hz) :box :white 1.0)
      (lp 0.0 1.4 (* 0.1 hz) 0.0 (* 1.05 w) 2.8 (* 1.5 d) :box :white 1.0)
      (lp 0.0 9.0 (* -0.15 hz) 0.0 w 12.0 d :box :white 1.0)
      (lp 0.0 15.6 (* -0.15 hz) 0.0 (* 1.06 w) 1.2 (* 1.08 d) :box :stone 0.0)
      ;; A drum and a dome. The icosahedron is a ball, so most of it is buried
      ;; in the drum and only the cap shows.
      (lp 0.0 17.0 (* -0.15 hz) 0.0 (* 0.44 w) 3.6 (* 0.44 w)
          :cylinder :stone 0.0)
      (lp 0.0 19.0 (* -0.15 hz) 0.0 (* 0.46 w) (* 0.46 w) (* 0.46 w)
          :blob :white 0.0)
      ;; The portico: entablature and pediment over the columns.
      (lp 0.0 12.6 (* 0.52 hz) 0.0 (* 0.62 w) 2.0 (* 0.5 d) :box :white 0.0)
      (lp 0.0 15.4 (* 0.52 hz) 0.0 (* 0.64 w) 4.0 (* 0.52 d) :gable :white 0.0)]
     (for [i (range 6)]
       (lp (+ (* -0.26 w) (* i 0.104 w)) 6.5 (* 0.52 hz) 0.0
           1.6 11.0 1.6 :cylinder :white 1.0)))))

(defmethod landmark-shapes :funfair [_ hx hz r]
  (let [wr (min 18.0 (* 0.62 hz))            ; wheel radius
        wx (* -0.42 hx)                      ; and where it stands
        cy (+ wr 5.0)]
    (concat
     [(apron hx hz :tarmac)
      ;; Big top and carousel, so the wheel has a fair round it.
      (lp (* 0.5 hx) 3.5 (* 0.42 hz) 0.0 (* 0.66 hx) 7.0 (* 0.66 hz)
          :cylinder :white 1.0)
      (lp (* 0.5 hx) 11.0 (* 0.42 hz) 0.0 (* 0.72 hx) 9.0 (* 0.72 hz)
          :pyramid :red 0.0)
      (lp (* 0.46 hx) 2.2 (* -0.5 hz) 0.0 10.0 4.4 10.0 :cylinder :sign 1.0)
      (lp (* 0.46 hx) 5.6 (* -0.5 hz) 0.0 11.0 3.2 11.0 :pyramid :red 0.0)]
     ;; The carousel turns, but a plain drum turning about its own axis is a
     ;; drum standing still. The horses are what makes it visible -- and they
     ;; keep facing the way they are going, which is what `:rigid` about the
     ;; vertical means and what a carousel does.
     (spinning
      (mapcat
       (fn [[x z a]]
         [(lp (+ (* 0.46 hx) x) 3.6 (+ (* -0.5 hz) z) a 0.24 3.6 0.24
              :cylinder :gold 0.0)
          (lp (+ (* 0.46 hx) x) 4.1 (+ (* -0.5 hz) z) a 2.2 1.3 0.9
              :box :white 0.0)])
       (ring 6 6.6 6.6))
      {:mode :rigid :axis :y :rate 0.5
       :pivot [(* 0.46 hx) 4.0 (* -0.5 hz)]})
     [;; The wheel stands in the plane z = wx, so its spokes and rim are parts
      ;; yawed a quarter turn and then tilted -- the only way to lean anything
      ;; out of the horizontal.
      (lp wx cy 0.0 0.0 2.4 2.4 4.0 :cylinder :metal 1.0)]
     (for [sx' [-1.0 1.0], sz' [-1.0 1.0]]
       (lp (+ wx (* sx' 0.34 wr)) (* 0.5 cy) (* sz' 0.5 wr) 0.0
           1.4 cy 1.4 :box :metal 1.0))
     ;; Spokes and rim, bolted to the hub and turning with it.
     (spinning
      (concat
       (for [i (range 8) :let [a (* tau (/ (double i) 16.0))]]
         (tilt (lp wx cy 0.0 quarter 0.7 (* 2.0 wr) 0.9 :box :metal 0.0) a))
       (for [i (range 16) :let [a (* tau (/ (double i) 16.0))]]
         (tilt (lp (+ wx (* wr (js-sin a))) (+ cy (* wr (js-cos a))) 0.0 quarter
                   0.7 (* 0.42 wr) 0.7 :box :metal 0.0)
               (+ a quarter))))
      {:rate 0.26 :pivot [wx cy 0.0]})
     ;; Gondolas, hung round the rim: they go round with it and stay the right
     ;; way up, which is the entire difference between a fairground ride and a
     ;; tumble dryer.
     (spinning
      (for [i (range 8) :let [a (* tau (/ (double i) 8.0))]]
        (lp (+ wx (* wr (js-sin a))) (- (+ cy (* wr (js-cos a))) 2.0) 0.0 0.0
            2.8 2.6 3.0 :box :sign 0.0))
      {:rate 0.26 :mode :orbit :pivot [wx cy 0.0]}))))

(defmethod landmark-shapes :school [_ hx hz r]
  (concat
   [(apron hx hz :tarmac)
    (lp 0.0 0.14 (* -0.45 hz) 0.0 (* 1.8 hx) 0.28 (* 0.95 hz) :box :grass 0.0)
    ;; Two wings, one of them a storey taller, and flat roofs on both. Nobody
    ;; has ever built a school that looked like anything else.
    (lp (* -0.45 hx) 5.0 (* 0.42 hz) 0.0 (* 0.9 hx) 10.0 (* 0.5 hz)
        :box :brick 1.0)
    (lp (* 0.55 hx) 4.0 (* 0.42 hz) 0.0 (* 0.55 hx) 8.0 (* 0.5 hz)
        :box :brick 1.0)
    (lp (* -0.45 hx) 10.6 (* 0.42 hz) 0.0 (* 0.92 hx) 1.2 (* 0.53 hz)
        :box :roof 0.0)
    (lp (* 0.55 hx) 8.6 (* 0.42 hz) 0.0 (* 0.57 hx) 1.2 (* 0.53 hz)
        :box :roof 0.0)
    ;; The court, which is the half of the site you can actually drive on.
    (lp 0.0 0.2 (* -0.45 hz) 0.0 (* 0.95 hx) 0.32 (* 0.62 hz) :box :dark 0.0)
    (lp (* 0.92 hx) 6.5 (* 0.8 hz) 0.0 0.4 13.0 0.4 :cylinder :white 1.0)]
   ;; The flag, flying to one side of the pole and swinging round it. Offset,
   ;; because a swing about a pivot the part is centred on turns it on the spot
   ;; and a flag that pirouettes about its own middle is not a flag.
   (spinning [(lp (+ (* 0.92 hx) 1.7) 11.8 (* 0.8 hz) 0.0
                  3.0 1.8 0.2 :box :red 0.0)]
             {:mode :swing :axis :y :rate 1.1 :amp 0.55
              :pivot [(* 0.92 hx) 11.8 (* 0.8 hz)]})
   (mapcat (fn [z] [(lp 0.0 1.7 z 0.0 0.5 3.4 0.5 :cylinder :metal 1.0)
                    (lp 0.0 3.9 z 0.0 3.0 1.8 0.3 :box :white 0.0)])
           [(* -0.88 hz) (* -0.04 hz)])))

(defmethod landmark-shapes :refinery [_ hx hz r]
  (let [tr (max 6.0 (min 11.0 (* 0.34 hx)))]
    (concat
     [(apron hx hz :concrete)
      ;; The flare. It is the one part of a refinery anybody can name, and the
      ;; only part visible from the next district.
      (lp (* -0.82 hx) 26.0 (* -0.72 hz) 0.0 2.0 52.0 2.0 :cylinder :metal 1.0)
      ;; A pipe rack across the whole yard, on trestles.
      (lp 0.0 6.4 (* 0.66 hz) 0.0 (* 1.7 hx) 1.6 3.0 :box :rust 1.0)
      (lp 0.0 8.2 (* 0.66 hz) 0.0 (* 1.7 hx) 1.2 2.2 :box :metal 0.0)]
     ;; The flare itself, which is a flame and therefore the one thing here
     ;; that has no business holding still. Fast and uneven rather than a
     ;; pulse: it reads as burning instead of as a warning light.
     (spinning [(lp (* -0.82 hx) 54.5 (* -0.72 hz) 0.0 3.0 6.0 3.0
                    :cylinder :red 0.0)]
               {:mode :blink :rate 5.5 :amp 0.72
                :pivot [(* -0.82 hx) 54.5 (* -0.72 hz)]})
     ;; A pump jack. The walking beam is the reason it is here: a nodding
     ;; donkey is recognisable from a kilometre away and from any angle, and
     ;; it is the cheapest moving thing in the catalogue -- one box on a
     ;; swing.
     (let [jx (* 0.72 hx) jz (* -0.62 hz)]
       (concat
        [(lp jx 0.4 jz 0.0 9.0 0.8 5.0 :box :concrete 1.0)
         (lp jx 3.4 jz 0.0 1.6 6.0 1.6 :box :rust 1.0)
         (lp (+ jx 3.6) 1.4 jz 0.0 3.6 2.0 2.6 :box :rust 1.0)]
        (spinning [(lp jx 6.6 jz 0.0 11.0 1.0 1.0 :box :rust 0.0)
                   (lp (- jx 5.0) 6.6 jz 0.0 2.4 2.4 1.8 :box :dark 0.0)]
                  {:mode :swing :rate 1.5 :amp 0.32 :pivot [jx 6.6 jz]})))
     (for [i (range 5)]
       (lp (+ (* -0.7 hx) (* i 0.35 hx)) 3.2 (* 0.66 hz) 0.0
           1.0 6.4 1.0 :box :metal 1.0))
     ;; Distillation columns on a common plinth, no two the same height.
     (cons
      (lp (* -0.25 hx) 0.5 (* -0.12 hz) 0.0 (* 1.0 hx) 1.0 (* 0.55 hz)
          :box :concrete 0.0)
      (for [i (range 4)
            :let [h (+ 24.0 (* 9.0 (js-sin (* 2.3 (double i)))))]]
        (lp (+ (* -0.62 hx) (* i 0.25 hx)) (* 0.5 h) (* -0.12 hz) 0.0
            5.0 h 5.0 :cylinder :white 1.0)))
     ;; Tank farm, each tank inside its own bund.
     (mapcat (fn [[x z _]]
               [(lp x 0.6 z 0.0 (* 2.4 tr) 1.2 (* 2.4 tr) :box :concrete 1.0)
                (lp x 5.0 z 0.0 (* 2.0 tr) 10.0 (* 2.0 tr) :cylinder :metal 1.0)
                (lp x 10.4 z 0.0 (* 2.05 tr) 1.0 (* 2.05 tr)
                    :cylinder :white 0.0)])
             (ring 3 (* 0.6 hx) (* 0.6 hz))))))

(defmethod landmark-shapes :scrapyard [_ hx hz r]
  (concat
   [(apron hx hz :dark)
    ;; The crane, with its jib out over the yard, and the shed it feeds.
    (lp (* 0.4 hx) 9.0 (* -0.3 hz) 0.0 3.0 18.0 3.0 :box :rust 1.0)
    (tilt (lp (* 0.4 hx) 19.0 (* -0.3 hz) quarter 1.0 (* 0.95 hx) 1.4
              :box :rust 0.0)
          (* 0.3 tau))
    (lp (* -0.4 hx) 3.5 (* 0.45 hz) 0.0 (* 0.5 hx) 7.0 (* 0.4 hz)
        :box :metal 1.0)]
   ;; Stacks of what used to be cars, three or four high, none of them square
   ;; to the next.
   (mapcat
    (fn [_]
      (let [x (prng/next-range! r (* -0.8 hx) (* 0.8 hx))
            z (prng/next-range! r (* -0.8 hz) (* 0.8 hz))
            n (+ 2 (prng/next-int! r 3))]
        (for [i (range n)]
          (lp (+ x (prng/next-range! r -0.5 0.5)) (+ 0.8 (* i 1.5))
              (+ z (prng/next-range! r -0.5 0.5))
              (prng/next-range! r 0.0 tau)
              4.6 1.5 2.3 :box
              (nth [:red :rust :white :metal :sign] (prng/next-int! r 5))
              1.0))))
    (range 9))
   ;; The fence, with a gap where the gate is -- a yard you can only look into
   ;; is scenery, not a landmark.
   (keep-indexed
    (fn [i [x z a]]
      (when-not (= i 2)
        (lp x 1.6 z a (* 0.7 hx) 3.2 0.5 :box :rust 1.0)))
    (ring 10 (* 0.95 hx) (* 0.95 hz)))))

(defmethod landmark-shapes :windmill [_ hx hz r]
  (let [th 22.0                              ; tower height
        sr 11.0                              ; sail radius
        hy (+ th 1.0)                        ; and the height of the shaft
        a0 (prng/next-range! r 0.0 tau)]
    (concat
     [(apron hx hz :grass)
      (lp 0.0 0.14 0.0 0.0 (* 1.8 hx) 0.28 (* 1.8 hz) :box :grass 0.0)
      ;; A tower mill: stone tower, cap, windshaft, and four sails on the
      ;; front of it. The shaft is a cylinder tipped on its side, which is what
      ;; the pitch angle is for.
      (lp 0.0 (* 0.5 th) 0.0 0.0 11.0 th 11.0 :cylinder :stone 1.0)
      (lp 0.0 (+ th 1.5) 0.0 0.0 9.5 5.0 9.5 :blob :dark 0.0)
      (tilt (lp 0.0 hy -4.0 0.0 1.4 8.0 1.4 :cylinder :metal 0.0) quarter)
      ;; The mill house, and a cart track up to its door.
      (lp (* 0.6 hx) 3.0 (* 0.5 hz) 0.0 12.0 6.0 9.0 :box :stone 1.0)
      (lp (* 0.6 hx) 7.5 (* 0.5 hz) 0.0 13.0 3.0 10.0 :gable :roof 0.0)]
     ;; Sails: an arm across the full diameter and a panel of cloth on each,
     ;; and the whole lot turning about the windshaft.
     (spinning
      (mapcat
       (fn [i]
         (let [a (+ a0 (* tau (/ (double i) 4.0)))
               ax (* -0.5 sr (js-sin a))
               ay (* 0.5 sr (js-cos a))]
           [(tilt (lp 0.0 hy -7.5 quarter 0.7 (* 2.0 sr) 0.9 :box :timber 0.0) a)
            (tilt (lp ax (+ hy ay) -7.8 quarter 0.3 (* 0.85 sr) 3.4
                      :box :white 0.0)
                  a)]))
       (range 4))
      {:rate 0.85 :pivot [0.0 hy -7.5]}))))

(defmethod landmark-shapes :water-tower [_ hx hz r]
  (let [h  26.0
        lr (* 0.5 (max 9.0 (min 16.0 (* 0.55 hx))))]   ; half the leg spread
    (concat
     [(apron hx hz :grass)
      (lp 0.0 0.14 0.0 0.0 (* 1.8 hx) 0.28 (* 1.8 hz) :box :grass 0.0)
      ;; Tank, ring beam, cap.
      (lp 0.0 (+ h 5.0) 0.0 0.0 (* 4.4 lr) 10.0 (* 4.4 lr) :cylinder :metal 1.0)
      (lp 0.0 h 0.0 0.0 (* 4.5 lr) 1.4 (* 4.5 lr) :cylinder :white 0.0)
      (lp 0.0 (+ h 11.5) 0.0 0.0 (* 4.2 lr) 4.0 (* 4.2 lr) :pyramid :roof 0.0)
      ;; The ladder, and the pump house it starts behind.
      (lp 0.0 (* 0.5 h) (* 1.15 lr) 0.0 1.2 h 0.4 :box :metal 0.0)
      (lp (* 0.62 hx) 2.4 (* 0.5 hz) 0.0 10.0 4.8 8.0 :box :brick 1.0)
      (lp (* 0.62 hx) 5.6 (* 0.5 hz) 0.0 11.0 2.4 9.0 :gable :roof 0.0)]
     ;; Four legs and the bracing between them, which is the whole silhouette.
     (for [sx' [-1.0 1.0], sz' [-1.0 1.0]]
       (lp (* sx' lr) (* 0.5 h) (* sz' lr) 0.0 1.6 h 1.6 :cylinder :metal 1.0))
     (for [y [(* 0.35 h) (* 0.75 h)], s [-1.0 1.0]]
       (lp (* s lr) y 0.0 quarter (* 2.0 lr) 0.7 0.7 :box :metal 0.0))
     (for [y [(* 0.35 h) (* 0.75 h)], s [-1.0 1.0]]
       (lp 0.0 y (* s lr) 0.0 (* 2.0 lr) 0.7 0.7 :box :metal 0.0)))))

(defmethod landmark-shapes :ruins [_ hx hz r]
  (concat
   [(lp 0.0 -0.6 0.0 0.0 (* 1.6 hx) 2.0 (* 1.6 hz) :box :grass 0.0)
    ;; The keep, standing to full height on one side and sheared off on the
    ;; other, plus the one corner tower that survived.
    (lp (* -0.25 hx) 7.0 0.0 0.0 (* 0.5 hx) 14.0 (* 0.45 hz) :box :stone 1.0)
    (lp (* -0.25 hx) 16.0 (* -0.26 hz) 0.0 (* 0.51 hx) 4.0 (* 0.2 hz)
        :box :stone 1.0)
    (lp (* 0.15 hx) 11.0 (* 0.32 hz) 0.0 9.0 22.0 9.0 :cylinder :stone 1.0)
    (lp (* 0.15 hx) 22.5 (* 0.32 hz) 0.0 9.6 2.0 9.6 :cylinder :stone 0.0)]
   ;; The curtain wall: a run of merlons with pieces missing out of it.
   (for [i (range 9)
         :when (not (#{2 6} i))
         :let [h (prng/next-range! r 3.5 7.5)]]
     (lp (+ (* -0.85 hx) (* i 0.21 hx)) (* 0.5 h) (* 0.82 hz) 0.0
         (* 0.19 hx) h 2.4 :box :stone 1.0))
   ;; Fallen blocks, lying where they landed.
   (for [_ (range 10)]
     (lp (prng/next-range! r (* -0.9 hx) (* 0.9 hx)) 0.7
         (prng/next-range! r (* -0.9 hz) (* 0.9 hz))
         (prng/next-range! r 0.0 tau)
         (prng/next-range! r 1.6 3.4) 1.4 (prng/next-range! r 1.4 2.8)
         :box :stone 1.0))
   ;; And the trees that grew up through it afterwards.
   (mapcat (fn [[x z _]] (tree-at x z (+ 7.0 (prng/next-range! r 0.0 5.0))))
           (take 4 (ring 7 (* 0.82 hx) (* 0.82 hz))))))

(defmethod landmark-shapes :drive-in [_ hx hz r]
  (concat
   [(apron hx hz :tarmac)
    ;; The screen: a billboard the size of a house, which is why a drive-in is
    ;; the one thing you can pick out of flat country besides the mast.
    (lp 0.0 11.0 (* -0.82 hz) 0.0 (* 1.5 hx) 22.0 1.0 :box :white 1.0)
    (lp 0.0 22.5 (* -0.82 hz) 0.0 (* 1.52 hx) 1.0 1.6 :box :red 0.0)
    (lp 0.0 5.0 (* -0.9 hz) 0.0 (* 1.4 hx) 10.0 0.8 :box :dark 1.0)
    ;; Projection hut, and the sign out at the gate.
    (lp 0.0 2.5 (* 0.62 hz) 0.0 7.0 5.0 6.0 :box :white 1.0)
    (lp (* 0.85 hx) 6.0 (* 0.88 hz) 0.0 0.8 12.0 0.8 :cylinder :metal 1.0)
    (lp (* 0.85 hx) 13.0 (* 0.88 hz) 0.0 6.0 4.0 0.6 :box :sign 0.0)]
   ;; The ramps: a low bank under each row, so the cars point up at the screen.
   (for [i (range 4)]
     (lp 0.0 0.35 (+ (* -0.5 hz) (* i 0.32 hz)) 0.0
         (* 1.6 hx) 0.7 2.2 :box :sand 0.0))
   ;; Speaker posts down each row.
   (for [i (range 4), j (range 5)]
     (lp (+ (* -0.8 hx) (* j 0.4 hx)) 1.0 (+ (* -0.44 hz) (* i 0.32 hz)) 0.0
         0.25 2.0 0.25 :cylinder :dark 1.0))))

;; --- landmarks worth the detour ---------------------------------------------
;;
;; The catalogue up to here answers "what sort of place is this". These answer
;; "why would I drive over there", which is a different question: each is
;; either a shape nothing else in the world has (a wheel, a runway, a colossus)
;; or somewhere with room to drive *inside* it. `chunk-pickups` puts a ring of
;; coins round every one of them, so the answer is also literally worth money.

(defmethod landmark-shapes :airport [_ hx hz r]
  (let [rw (* 1.8 hx)
        ax (* 0.1 hx) az (* 0.2 hz)]
    (concat
     [(apron hx hz :grass)
      ;; A runway is a flat grey stripe a hundred metres long, and nothing else
      ;; in the world is that shape. It does the whole job of identification on
      ;; its own; everything below is what stops it being a car park.
      (lp 0.0 0.12 (* -0.4 hz) 0.0 rw 0.3 15.0 :box :tarmac 0.0)
      (lp 0.0 0.16 (* 0.1 hz) 0.0 (* 1.2 hx) 0.3 9.0 :box :tarmac 0.0)
      ;; Control tower.
      (lp (* -0.62 hx) 8.0 (* 0.62 hz) 0.0 6.0 16.0 6.0 :box :concrete 1.0)
      (lp (* -0.62 hx) 17.5 (* 0.62 hz) 0.0 9.0 4.0 9.0 :box :glass 1.0)
      (lp (* -0.62 hx) 20.2 (* 0.62 hz) 0.0 9.6 1.4 9.6 :box :dark 0.0)
      (lp (* -0.62 hx) 24.0 (* 0.62 hz) 0.0 0.4 6.0 0.4 :cylinder :metal 0.0)]
     ;; The radar. A bar going round on top of the tower, which is what an
     ;; airfield does when nothing is landing.
     (spinning [(lp (* -0.62 hx) 27.4 (* 0.62 hz) 0.0 7.0 0.5 1.2
                    :box :white 0.0)]
               {:mode :rigid :axis :y :rate 1.1
                :pivot [(* -0.62 hx) 27.4 (* 0.62 hz)]})
     [;; Terminal, then the hangar with its doors facing the apron.
      (lp (* 0.05 hx) 3.5 (* 0.66 hz) 0.0 (* 0.55 hx) 7.0 (* 0.35 hz)
          :box :white 1.0)
      (lp (* 0.05 hx) 7.4 (* 0.66 hz) 0.0 (* 0.57 hx) 1.0 (* 0.37 hz)
          :box :metal 0.0)
      (lp (* 0.72 hx) 6.0 (* 0.62 hz) 0.0 (* 0.35 hx) 12.0 (* 0.4 hz)
          :box :metal 1.0)
      (lp (* 0.72 hx) 14.0 (* 0.62 hz) quarter (* 0.42 hz) 5.0 (* 0.36 hx)
          :gable :metal 0.0)
      ;; The windsock: two volumes, and the detail that names the place.
      (lp (* -0.92 hx) 4.0 (* 0.05 hz) 0.0 0.3 8.0 0.3 :cylinder :white 1.0)]
     ;; And the sock on it, which does not point anywhere in particular and
     ;; never has. It hangs *off* the pole rather than on it -- a swing about a
     ;; pivot the part is already centred on is a part twisting on the spot,
     ;; which is a radar and is not a windsock.
     (spinning [(tilt (lp (- (* -0.92 hx) 2.4) 7.6 (* 0.05 hz) quarter
                          1.3 4.4 1.3 :cylinder :red 0.0)
                      quarter)]
               {:mode :swing :axis :y :rate 0.9 :amp 0.8
                :pivot [(* -0.92 hx) 7.6 (* 0.05 hz)]})
     ;; Centreline.
     (for [i (range 7)]
       (lp (+ (* -0.39 rw) (* i 0.13 rw)) 0.24 (* -0.4 hz) 0.0
           (* 0.06 rw) 0.3 1.2 :box :white 0.0))
     ;; And an aeroplane on the apron. Fuselage, nose, wing, two engines,
     ;; tailplane, fin -- seven volumes, and unmistakable from anywhere.
     [(tilt (lp ax 4.6 az 0.0 3.2 22.0 3.2 :cylinder :white 1.0) quarter)
      (lp ax 4.6 (- az 10.5) 0.0 3.2 3.0 3.4 :blob :white 0.0)
      (lp ax 3.4 az 0.0 26.0 0.8 5.5 :box :white 1.0)
      (tilt (lp (- ax 7.0) 2.4 (+ az 1.0) 0.0 2.4 5.5 2.4
                :cylinder :metal 0.0) quarter)
      (tilt (lp (+ ax 7.0) 2.4 (+ az 1.0) 0.0 2.4 5.5 2.4
                :cylinder :metal 0.0) quarter)
      (lp ax 3.6 (+ az 9.0) 0.0 10.0 0.7 3.0 :box :white 0.0)
      (lp ax 7.4 (+ az 9.8) 0.0 0.7 6.6 4.5 :box :red 1.0)])))

(defmethod landmark-shapes :bazaar [_ hx hz r]
  (let [cloths [:cloth-a :cloth-b :cloth-c :cloth-d]]
    (concat
     [(apron hx hz :tarmac)
      (lp 0.0 0.16 0.0 0.0 (* 1.8 hx) 0.32 (* 1.8 hz) :box :stone 0.0)
      ;; The banner over the way in, which is what you see before the stalls.
      (lp (* -0.88 hx) 3.2 (* 0.92 hz) 0.0 0.6 6.4 0.6 :cylinder :timber 1.0)
      (lp (* 0.88 hx) 3.2 (* 0.92 hz) 0.0 0.6 6.4 0.6 :cylinder :timber 1.0)
      ;; A couple of vans that brought it all, parked at the back.
      (lp (* -0.62 hx) 1.6 (* -0.86 hz) 0.0 6.0 3.2 2.6 :box :cloth-b 1.0)
      (lp (* 0.66 hx) 1.6 (* -0.86 hz) 0.4 6.0 3.2 2.6 :box :white 1.0)]
     ;; The banner, slung between the two posts and not quite still. A tenth
     ;; of a radian is nothing to look at and everything to notice: a market
     ;; where the only cloth in it is rigid reads as a model of a market.
     (spinning [(lp 0.0 5.6 (* 0.92 hz) 0.0 (* 1.8 hx) 2.4 0.3
                    :box :cloth-a 0.0)]
               {:mode :swing :axis :y :rate 1.3 :amp 0.11
                :pivot [0.0 5.6 (* 0.92 hz)]})
     ;; Three rows of stalls with aisles between them: a market is a grid you
     ;; drive down, not a heap you park beside.
     (mapcat
      (fn [[x z]]
        (let [c (nth cloths (prng/next-int! r 4))
              a (prng/next-range! r -0.12 0.12)]
          [(lp x 0.5 z a 5.4 1.0 2.0 :box :timber 1.0)
           (lp x 1.35 z a 0.16 1.7 0.16 :cylinder :timber 0.0)
           (lp x 2.7 z a 6.0 1.1 3.0 :gable c 0.0)
           ;; The junk on the ground beside it, which is the whole idea.
           (lp (+ x (prng/next-range! r -2.4 2.4)) 0.45
               (+ z (prng/next-range! r 1.6 2.4)) (prng/next-range! r 0.0 tau)
               1.1 0.9 1.1 :box :timber 1.0)]))
      (for [row (range 3), col (range 4)]
        [(+ (* -0.62 hx) (* col 0.42 hx))
         (+ (* -0.55 hz) (* row 0.5 hz))])))))

(defmethod landmark-shapes :statue [_ hx hz r]
  (let [ph 7.0                          ; plinth
        y0 (+ 3.3 ph)                   ; where the boots start
        arm 7.5]
    (concat
     [(apron hx hz :stone)
      (lp 0.0 0.16 0.0 0.0 (* 1.85 hx) 0.32 (* 1.85 hz) :box :stone 0.0)
      ;; Two steps and a plinth, and the plinth is half the height of the whole
      ;; thing. That proportion is what makes it a monument rather than a very
      ;; large man standing in a square.
      (lp 0.0 0.7 0.0 0.0 24.0 1.4 24.0 :box :stone 1.0)
      (lp 0.0 1.9 0.0 0.0 19.0 1.2 19.0 :box :stone 1.0)
      (lp 0.0 (+ 2.5 (* 0.5 ph)) 0.0 0.0 12.0 ph 12.0 :box :white 1.0)
      (lp 0.0 (+ 2.5 ph 0.3) 0.0 0.0 13.2 1.0 13.2 :box :stone 0.0)
      (lp 0.0 (+ 2.5 (* 0.5 ph)) 6.2 0.0 8.0 2.2 0.3 :box :gold 0.0)
      ;; Him. Boots, coat, chest, head, cap -- and one arm out over the square,
      ;; which is the pose and the only part that has to read at two hundred
      ;; metres.
      (lp -1.9 (+ y0 2.6) 0.0 0.0 2.2 5.2 2.6 :box :bronze 1.0)
      (lp 1.9 (+ y0 2.6) 0.0 0.0 2.2 5.2 2.6 :box :bronze 1.0)
      (lp 0.0 (+ y0 8.0) 0.0 0.0 7.0 6.6 4.4 :box :bronze 1.0)
      (lp 0.0 (+ y0 12.2) 0.0 0.0 7.8 2.4 4.8 :box :bronze 1.0)
      (lp 0.0 (+ y0 14.6) 0.0 0.0 3.2 3.4 3.2 :blob :bronze 0.0)
      (lp 0.0 (+ y0 16.2) 0.0 0.0 4.2 0.9 4.2 :cylinder :bronze 0.0)
      (lp -4.4 (+ y0 9.0) 0.0 0.0 1.9 7.6 2.0 :box :bronze 1.0)]
     ;; The raised arm, a spoke in the world's vertical plane at 55 degrees.
     (let [a 0.95]
       [(tilt (lp (+ 3.6 (* 0.5 arm (js-sin a)))
                  (+ y0 11.4 (* 0.5 arm (js-cos a))) 0.0
                  quarter 2.0 arm 1.9 :box :bronze 0.0)
              (- a))])
     ;; Benches round the edge, so the square has something in it besides him.
     (for [[x z a] (ring 6 (* 0.88 hx) (* 0.88 hz))]
       (lp x 0.7 z a 4.6 0.5 1.4 :box :timber 1.0)))))

(defmethod landmark-shapes :speedway [_ hx hz r]
  (let [rx (* 0.5 hx) rz (* 0.5 hz)]
    (concat
     [(apron hx hz :grass)
      ;; A dirt oval: the graded surface, then the infield cut back out of it.
      ;; Same two-ellipse trick as the stadium, in the colour of a shale track
      ;; rather than tarmac.
      (lp 0.0 0.30 0.0 0.0 (* 3.4 rx) 0.5 (* 3.4 rz) :cylinder :rust 0.0)
      (lp 0.0 0.36 0.0 0.0 (* 2.2 rx) 0.5 (* 2.2 rz) :cylinder :grass 0.0)
      ;; Grandstand down the near side.
      (lp 0.0 2.6 (* 2.05 rz) 0.0 (* 2.4 rx) 5.2 8.0 :box :timber 1.0)
      (lp 0.0 6.6 (* 2.2 rz) 0.0 (* 2.45 rx) 2.6 10.0 :gable :metal 0.0)
      ;; And the gantry over the start line.
      (lp (* -1.55 rx) 4.0 (* 1.1 rz) 0.0 1.0 8.0 1.0 :box :metal 1.0)
      (lp (* -1.55 rx) 8.6 (* 1.1 rz) quarter 1.0 0.9 (* 2.4 rz)
          :box :metal 0.0)
      (lp (* -1.55 rx) 9.8 (* 1.1 rz) quarter 1.6 2.2 (* 1.6 rz)
          :box :sign 0.0)]
     ;; The tyre wall, which is what you actually hit.
     (for [[x z a] (ring 20 (* 1.62 rx) (* 1.62 rz))]
       (lp x 0.55 z a (* 0.22 (+ rx rz)) 1.1 1.0 :box :rubber 1.0))
     ;; Floodlights.
     (mapcat (fn [[x z _]]
               [(lp x 9.0 z 0.0 1.0 18.0 1.0 :cylinder :metal 1.0)
                (lp x 18.8 z 0.0 4.0 1.6 1.2 :box :white 0.0)])
             (ring 4 (* 1.95 rx) (* 1.95 rz)))
     ;; Two of last week's, still in the infield.
     (for [_ (range 2)]
       (lp (prng/next-range! r (* -1.3 rx) (* 1.3 rx)) 0.9
           (prng/next-range! r (* -1.3 rz) (* 1.3 rz))
           (prng/next-range! r 0.0 tau) 4.6 1.8 2.2 :box :rust 1.0)))))

(defmethod landmark-shapes :windfarm [_ hx hz r]
  (let [th 38.0 br 13.0]
    (concat
     [(apron hx hz :grass)
      (lp 0.0 0.14 0.0 0.0 (* 1.8 hx) 0.28 (* 1.8 hz) :box :grass 0.0)
      (lp (* 0.78 hx) 2.0 (* 0.82 hz) 0.0 8.0 4.0 6.0 :box :concrete 1.0)]
     (mapcat
      (fn [[tx tz]]
        (let [a0 (prng/next-range! r 0.0 tau)
              hy (+ th 1.0)
              hzz (- tz 4.5)]
          (concat
           [(lp tx 0.6 tz 0.0 9.0 1.2 9.0 :cylinder :concrete 1.0)
            (lp tx (* 0.5 th) tz 0.0 2.8 th 2.8 :cylinder :white 1.0)
            ;; The nacelle, lying along Z with the rotor in front of it.
            (tilt (lp tx hy (- tz 2.0) 0.0 2.6 7.0 2.6 :cylinder :white 0.0)
                  quarter)]
           ;; Three blades from the hub outward -- offset half a length along
           ;; their own direction, because a part centred on the hub would come
           ;; out the other side and give six.
           (spinning
            (for [i (range 3)
                  :let [a (+ a0 (* tau (/ (double i) 3.0)))]]
              (tilt (lp (- tx (* 0.5 br (js-sin a)))
                        (+ hy (* 0.5 br (js-cos a))) hzz
                        quarter 0.5 br 1.5 :box :white 0.0)
                    a))
            {:rate 1.35 :pivot [tx hy hzz]}))))
      [[(* -0.6 hx) (* -0.45 hz)]
       [(* 0.55 hx) (* -0.15 hz)]
       [(* -0.1 hx) (* 0.6 hz)]]))))

(defmethod landmark-shapes :cemetery [_ hx hz r]
  (concat
   [(apron hx hz :grass)
    (lp 0.0 0.14 0.0 0.0 (* 1.8 hx) 0.28 (* 1.8 hz) :box :grass 0.0)
    ;; The chapel: small, and with the one spire that says what it is.
    (lp (* -0.6 hx) 3.5 (* -0.5 hz) 0.0 10.0 7.0 14.0 :box :stone 1.0)
    (lp (* -0.6 hx) 8.6 (* -0.5 hz) 0.0 10.6 3.2 14.6 :gable :roof 0.0)
    (lp (* -0.6 hx) 12.4 (* -0.5 hz) 0.0 1.0 5.0 1.0 :box :stone 0.0)
    (lp (* -0.6 hx) 13.6 (* -0.5 hz) 0.0 3.0 0.8 0.7 :box :stone 0.0)
    ;; The gate piers.
    (lp -4.0 1.8 (* 0.96 hz) 0.0 1.6 3.6 1.6 :box :stone 1.0)
    (lp 4.0 1.8 (* 0.96 hz) 0.0 1.6 3.6 1.6 :box :stone 1.0)]
   ;; The wall, with the gate left out of it.
   (keep-indexed
    (fn [i [x z a]]
      (when-not (= i 6)
        (lp x 0.9 z a (* 0.6 hx) 1.8 0.6 :box :stone 1.0)))
    (ring 10 (* 0.95 hx) (* 0.95 hz)))
   ;; Rows of headstones, none quite square to the next -- which is the only
   ;; thing separating a graveyard from a car park with bollards in it.
   (for [i (range 6), j (range 6)
         :let [h (prng/next-range! r 0.9 1.8)
               a (prng/next-range! r -0.16 0.16)]]
     (lp (+ (* -0.5 hx) (* i 0.2 hx) (prng/next-range! r -0.6 0.6))
         (* 0.5 h)
         (+ (* -0.3 hz) (* j 0.22 hz) (prng/next-range! r -0.5 0.5))
         a 1.1 h 0.35 :box :stone 1.0))
   ;; Two tombs and a rank of cypresses.
   (for [_ (range 2)]
     (lp (prng/next-range! r (* -0.6 hx) (* 0.6 hx)) 0.9
         (prng/next-range! r (* -0.7 hz) (* 0.7 hz))
         (prng/next-range! r -0.2 0.2) 2.6 1.8 4.2 :box :white 1.0))
   (mapcat (fn [[x z _]]
             [(lp x 3.0 z 0.0 1.0 6.0 1.0 :cylinder :timber 1.0)
              (lp x 8.0 z 0.0 3.2 12.0 3.2 :pyramid :leaf 0.0)])
           (take 4 (ring 9 (* 0.82 hx) (* 0.82 hz))))))

(defmethod landmark-shapes :quarry [_ hx hz r]
  (concat
   [(apron hx hz :sand)
    ;; The ground is a heightfield the collider is built from, so nothing here
    ;; can dig a hole in it. The pit is made by building the *rim* up instead,
    ;; which from a car is the same picture and from a plan is a ring of spoil.
    (lp 0.0 0.3 0.0 0.0 (* 1.5 hx) 0.6 (* 1.5 hz) :cylinder :sand 0.0)
    (lp (* 0.3 hx) 0.42 (* 0.25 hz) 0.0 (* 0.55 hx) 0.6 (* 0.45 hz)
        :cylinder :water 0.0)
    ;; Crusher, and the conveyor running up to the top of it.
    (lp (* -0.6 hx) 7.0 (* -0.5 hz) 0.0 9.0 14.0 9.0 :box :rust 1.0)
    (lp (* -0.6 hx) 15.0 (* -0.5 hz) 0.0 10.0 2.0 10.0 :box :metal 0.0)
    (tilt (lp (* -0.3 hx) 7.5 (* -0.28 hz) quarter 1.4 26.0 3.2
              :box :metal 0.0)
          (* 0.33 tau))
    ;; The stockpile it drops onto.
    (lp (* -0.02 hx) 3.0 (* -0.1 hz) 0.0 15.0 6.0 15.0 :pyramid :sand 1.0)]
   ;; Spoil on the rim.
   (for [[x z _] (ring 7 (* 0.92 hx) (* 0.92 hz))]
     (lp x 3.2 z 0.0 15.0 6.4 15.0 :pyramid :sand 1.0))
   ;; And the dumpers that put it there.
   (for [_ (range 3)]
     (lp (prng/next-range! r (* -0.7 hx) (* 0.7 hx)) 1.5
         (prng/next-range! r (* -0.7 hz) (* 0.7 hz))
         (prng/next-range! r 0.0 tau) 6.4 3.0 3.4 :box :sign 1.0))))

;; --- landmarks that say where in the world you are --------------------------
;;
;; One per region, and they only appear out of town. The point of them is not
;; accuracy, it is that the countryside stops being interchangeable: you can be
;; four kilometres from anything and still know which part of the map you are
;; on, because the huts have log walls or the roofs have no pitch at all.

(defmethod landmark-shapes :izbas [_ hx hz r]
  (let [bx 0.0 by 0.0 bz 0.0]           ; where the bear stands: the green
    (concat
     [(apron hx hz :grass)
      (lp 0.0 0.14 0.0 0.0 (* 1.8 hx) 0.28 (* 1.8 hz) :box :grass 0.0)
      ;; The well, which is what the huts are arranged around.
      (lp (* 0.5 hx) 0.7 (* -0.45 hz) 0.0 3.0 1.4 3.0 :cylinder :stone 1.0)
      (lp (* 0.5 hx) 2.6 (* -0.45 hz) 0.0 0.3 4.0 0.3 :cylinder :log 0.0)
      (lp (* 0.5 hx) 4.8 (* -0.45 hz) 0.0 3.6 1.6 3.6 :gable :roof 0.0)
      ;; A woodpile, because there is always a woodpile.
      (lp (* -0.65 hx) 0.9 (* 0.6 hz) 0.35 6.0 1.8 2.2 :box :log 1.0)]
     ;; Five log huts round the green, each turned its own way. Steep roofs and
     ;; a carved board on the gable end: two details, and neither is a shape
     ;; any other building in the game has.
     (mapcat
      (fn [[x z a]]
        (let [w (prng/next-range! r 6.0 8.0)
              d (prng/next-range! r 7.0 9.0)]
          [(lp x 2.1 z a w 4.2 d :box :log 1.0)
           (lp x 5.8 z a (* 1.12 w) 3.4 (* 1.1 d) :gable :timber 0.0)
           (lp x 4.6 (+ z (* 0.52 d)) a (* 0.8 w) 0.5 0.3 :box :white 0.0)
           (lp x 1.4 (+ z (* 0.52 d)) a 1.4 2.6 0.3 :box :timber 0.0)]))
      (ring 5 (* 0.66 hx) (* 0.66 hz)))
     ;; Birches, which are the other half of the picture.
     (mapcat (fn [[x z _]]
               [(lp x 3.4 z 0.0 0.5 6.8 0.5 :cylinder :white 1.0)
                (lp x 8.6 z 0.0 4.4 5.6 4.4 :blob :leaf 0.0)])
             (take 6 (ring 11 (* 0.88 hx) (* 0.88 hz))))
     ;; And a bear on its hind legs with a balalaika. It is the single most
     ;; ridiculous object in the world and it is worth every one of its eleven
     ;; volumes: nobody who sees it once forgets where the taiga is.
     [(lp (- bx 0.7) (+ by 0.8) bz 0.0 0.8 1.6 0.9 :cylinder :fur 1.0)
      (lp (+ bx 0.7) (+ by 0.8) bz 0.0 0.8 1.6 0.9 :cylinder :fur 1.0)
      (lp bx (+ by 2.6) bz 0.0 2.4 2.6 1.9 :blob :fur 1.0)
      (lp bx (+ by 4.3) bz 0.0 1.5 1.5 1.5 :blob :fur 0.0)
      (lp bx (+ by 4.1) (+ bz 0.75) 0.0 0.7 0.6 0.8 :blob :fur 0.0)
      (lp (- bx 0.62) (+ by 4.9) bz 0.0 0.5 0.5 0.4 :blob :fur 0.0)
      (lp (+ bx 0.62) (+ by 4.9) bz 0.0 0.5 0.5 0.4 :blob :fur 0.0)
      ;; The balalaika: a triangle and a neck, held across the chest.
      (tilt (lp (- bx 0.35) (+ by 2.5) (+ bz 1.15) 0.0 1.5 1.1 0.22
                :gable :timber 0.0) 0.35)
      (tilt (lp (+ bx 0.95) (+ by 3.3) (+ bz 1.05) quarter 0.16 2.2 0.16
                :box :timber 0.0) 2.1)
      ;; Both arms, one over the strings and one on the neck.
      (tilt (lp (- bx 1.15) (+ by 2.9) (+ bz 0.9) quarter 0.45 2.0 0.5
                :box :fur 0.0) 2.5)
      (tilt (lp (+ bx 1.25) (+ by 3.2) (+ bz 0.85) quarter 0.45 2.0 0.5
                :box :fur 0.0) 3.9)])))

(defmethod landmark-shapes :cantina [_ hx hz r]
  (concat
   [(apron hx hz :sand)
    (lp 0.0 0.16 0.0 0.0 (* 1.8 hx) 0.32 (* 1.8 hz) :box :sand 0.0)
    ;; Adobe, and a flat roof with a parapet. Every other building in the game
    ;; has a pitch on it; this one having none is most of what places it.
    (lp 0.0 3.0 (* -0.35 hz) 0.0 (* 1.05 hx) 6.0 (* 0.65 hz) :box :adobe 1.0)
    (lp 0.0 6.5 (* -0.35 hz) 0.0 (* 1.09 hx) 1.0 (* 0.69 hz) :box :adobe 0.0)
    ;; The porch across the front, on rough posts.
    (lp 0.0 4.4 (* 0.16 hz) 0.0 (* 1.05 hx) 0.4 (* 0.4 hz) :box :timber 0.0)
    (lp 0.0 1.4 (* 0.02 hz) 0.0 3.0 2.8 0.3 :box :timber 0.0)
    ]
   ;; And the sign, which is the only straight-edged thing on it -- and, in a
   ;; still afternoon in the sierra, the only thing on it that moves.
   (spinning [(lp 0.0 8.0 (* -0.02 hz) 0.0 (* 0.6 hx) 2.2 0.3
                  :box :lacquer 0.0)]
             {:mode :swing :axis :y :rate 0.75 :amp 0.13
              :pivot [0.0 8.0 (* -0.02 hz)]})
   (for [i (range 5)]
     (lp (+ (* -0.9 hx) (* i 0.45 hx)) 2.2 (* 0.34 hz) 0.0
         0.5 4.4 0.5 :cylinder :timber 1.0))
   ;; Barrels stacked by the door, a cart, and agave along the front.
   (for [i (range 6)]
     (lp (+ (* 0.5 hx) (* (mod i 3) 1.7)) (+ 0.8 (* 1.6 (quot i 3)))
         (* 0.22 hz) 0.0 1.5 1.5 1.5 :cylinder :timber 1.0))
   [(lp (* -0.62 hx) 1.0 (* 0.6 hz) 0.3 4.4 1.0 2.4 :box :timber 1.0)
    (lp (* -0.62 hx) 0.6 (* 0.6 hz) 0.3 0.4 1.2 1.2 :cylinder :log 0.0)]
   (mapcat (fn [[x z _]]
             (for [k (range 5)]
               (tilt (lp x 1.4 z quarter 0.28 3.0 0.9 :box :cactus 0.0)
                     (+ (* 0.42 (- (double k) 2.0))
                        (* 0.35 (js-sin (* 2.1 (double k))))))))
           (take 5 (ring 9 (* 0.85 hx) (* 0.85 hz))))
   ;; Two of the regulars, in hats, not moving.
   (mapcat
    (fn [[x z]]
      [(lp x 0.85 z 0.0 0.8 1.7 0.8 :cylinder :white 1.0)
       (lp x 2.1 z 0.0 2.0 1.3 2.0 :pyramid :cloth-a 0.0)
       (lp x 2.75 z 0.0 0.55 0.6 0.55 :blob :adobe 0.0)
       (lp x 3.05 z 0.0 2.2 0.14 2.2 :cylinder :sand 0.0)
       (lp x 3.3 z 0.0 0.8 0.5 0.8 :cylinder :sand 0.0)])
    [[(* -0.2 hx) (* 0.28 hz)] [(* 0.16 hx) (* 0.3 hz)]])))

(defmethod landmark-shapes :pagoda [_ hx hz r]
  (concat
   [(apron hx hz :stone)
    (lp 0.0 0.16 0.0 0.0 (* 1.8 hx) 0.32 (* 1.8 hz) :box :stone 0.0)
    ;; A pond with a red bridge over it.
    (lp (* 0.58 hx) 0.24 (* 0.5 hz) 0.0 (* 0.62 hx) 0.4 (* 0.62 hz)
        :cylinder :water 0.0)
    (lp (* 0.58 hx) 1.1 (* 0.5 hz) 0.4 (* 0.72 hx) 0.5 2.6 :box :lacquer 1.0)
    ;; The gate: two posts and two beams, which is a silhouette everybody
    ;; already knows.
    (lp (* -0.22 hx) 4.2 (* 0.86 hz) 0.0 1.0 8.4 1.0 :cylinder :lacquer 1.0)
    (lp (* 0.22 hx) 4.2 (* 0.86 hz) 0.0 1.0 8.4 1.0 :cylinder :lacquer 1.0)
    (lp 0.0 8.8 (* 0.86 hz) 0.0 (* 0.66 hx) 0.9 1.4 :box :lacquer 0.0)
    (lp 0.0 7.2 (* 0.86 hz) 0.0 (* 0.5 hx) 0.6 1.0 :box :lacquer 0.0)]
   ;; Five tiers, each smaller than the one below, each with the wide flat roof
   ;; that is the entire silhouette. Nothing else in the game overhangs.
   (mapcat
    (fn [i]
      (let [w (- 13.0 (* 1.7 (double i)))
            y (+ 1.0 (* 6.2 (double i)))]
        [(lp (* -0.15 hx) (+ y 2.2) (* -0.2 hz) 0.0 w 4.4 w
             :box (if (even? i) :white :adobe) 1.0)
         (lp (* -0.15 hx) (+ y 5.2) (* -0.2 hz) 0.0 (* 1.7 w) 2.0 (* 1.7 w)
             :pyramid :lacquer 0.0)
         (lp (* -0.15 hx) (+ y 4.5) (* -0.2 hz) 0.0 (* 1.05 w) 0.6 (* 1.05 w)
             :box :timber 0.0)]))
    (range 5))
   [(lp (* -0.15 hx) 33.0 (* -0.2 hz) 0.0 0.6 5.0 0.6 :cylinder :gold 0.0)]
   ;; Lanterns on a line from the gate.
   (for [i (range 6)]
     (lp (+ (* -0.5 hx) (* i 0.2 hx)) 2.6 (* 0.62 hz) 0.0
         1.0 1.2 1.0 :blob :lacquer 0.0))))

(defn chunk-landmarks
  "The landmark this chunk owns: `{:parts a :rotors b}`.

  `:parts` is a flat array in the same layout as `chunk-bridges`. `:rotors`
  describes the runs of it that move -- see `spinning` -- as
  [pivot-x pivot-y pivot-z axis rate mode amp phase first count], with axis 0
  for the vertical and 1 for the world Z, and mode 0 rigid, 1 orbit, 2 swing,
  3 blink. Both are empty for the fifteen chunks in a district that do not own
  a landmark, which is most of them.

  Ownership is by the landmark's centre, the same rule streets use, so exactly
  one chunk builds it however the districts and the chunk grid line up."
  [seed cx cz]
  (let [[dx dz] (district-of cx cz)
        out (transient [])
        rot (transient [])
        ;; Parts written so far. A chunk can own two landmarks -- districts and
        ;; chunks are different grids -- and a rotor's run index is into the
        ;; whole chunk's array, not into the landmark that produced it.
        written (volatile! 0)]
    (doseq [ddx [-1 0 1], ddz [-1 0 1]
            :let [lm (landmark seed (+ dx ddx) (+ dz ddz))]
            :when lm
            :let [[ox oz] (chunk-of (:x lm) (:z lm))]
            :when (and (= ox cx) (= oz cz))]
      (let [{:keys [kind half-x half-z]} lm
            y0 (height-at seed (:x lm) (:z lm))
            ;; Its own generator, so adding a landmark kind cannot shift the
            ;; trees in the next district.
            r  (prng/chunk-rng seed cx cz (+ 97 (:landmarks k/salt)))
            shapes (vec (remove nil?
                                (flatten (landmark-shapes kind half-x half-z r))))
            base @written]
        (doseq [{:keys [x y z yaw pitch sx sy sz prim tint solid]
                 :or {pitch 0.0}} shapes]
          (doseq [v [(+ (:x lm) x) (+ y0 y) (+ (:z lm) z) yaw pitch sx sy sz
                     (double (prim-index prim)) (double tint) solid]]
            (conj! out v))
          (vswap! written inc))
        ;; Runs of consecutive parts that share one spin. Consecutive because
        ;; the shapes emit them that way: a rotor is a range rather than a list
        ;; of indices, which keeps it to eight numbers however many blades it
        ;; turns out to have.
        (loop [i 0]
          (when (< i (count shapes))
            (if-let [sp (:spin (nth shapes i))]
              (let [j (loop [j i]
                        (if (and (< j (count shapes))
                                 (= sp (:spin (nth shapes j))))
                          (recur (inc j))
                          j))
                    [pvx pvy pvz] (:pivot sp)]
                (doseq [v [(+ (:x lm) pvx) (+ y0 pvy) (+ (:z lm) pvz)
                           (if (= :y (:axis sp)) 0.0 1.0)
                           (:rate sp)
                           (case (:mode sp) :rigid 0.0 :orbit 1.0
                                 :swing 2.0 :blink 3.0)
                           (:amp sp) (:phase sp)
                           (double (+ base i)) (double (- j i))]]
                  (conj! rot v))
                (recur j))
              (recur (inc i)))))))
    (let [v (persistent! out)
          a (farray (count v))
          w (persistent! rot)
          b (farray (count w))]
      (dotimes [i (count v)] (fput! a i (nth v i)))
      (dotimes [i (count w)] (fput! b i (nth w i)))
      {:parts a :rotors b})))

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

      :plant
      ;; A works: a low shed, a rank of storage tanks beside it, a stack that
      ;; can be seen from the next district, and a gantry over the yard.
      (let [tanks (+ 2 (prng/next-int! r 3))
            tr    (min 3.4 (* 0.30 hx))
            th    (* tr (prng/next-range! r 2.2 3.4))]
        (into [base
               ;; The stack. Twice a factory chimney and set on its own base,
               ;; because a chimney growing straight out of a roof reads as a
               ;; pipe and a chimney on a plinth reads as a chimney.
               (part (* hx -0.68) (* 0.5 h) (* hz 0.55) 2.6 h 2.6 :box (:concrete c))
               (part (* hx -0.68) (+ h 13.0) (* hz 0.55) 1.7 28.0 1.7
                     :cylinder (:brick c))
               ;; Gantry: two legs and a beam across the yard.
               (part (* hx 0.86) (* 0.5 (+ h 3.0)) (* hz -0.75) 0.7 (+ h 3.0) 0.7
                     :box (:steel c))
               (part (* hx 0.86) (* 0.5 (+ h 3.0)) (* hz 0.75) 0.7 (+ h 3.0) 0.7
                     :box (:steel c))
               (part (* hx 0.86) (+ h 3.0) 0.0 1.1 0.9 (* 2 hz 0.9)
                     :box (:steel c))
               ;; A pipe run along the roof, which is the detail that says the
               ;; building is doing something rather than storing something.
               (part 0.0 (+ h 0.8) (* hz -0.55) (* 2 hx 0.92) 0.5 0.5
                     :cylinder (:steel c))]
              (for [i (range tanks)]
                (part (+ (* hx -0.1) (* i 2.4 tr))
                      (+ h (* 0.5 th))
                      (* hz 0.1)
                      (* 2 tr) th (* 2 tr) :cylinder (:roof-metal c)))))

      :yard
      ;; Stacked containers and a couple of squat tanks. The base is deliberately
      ;; short: what is being drawn is a yard with things in it, not a building.
      (let [rows (+ 2 (prng/next-int! r 3))
            cw   (/ (* 2 hx) rows)]
        (into [(part 0.0 (* 0.5 h) 0.0 (* 2 hx) h (* 2 hz) :box (:concrete c))
               (part (* hx 0.6) (+ h 2.2) (* hz -0.5) 2.0 4.4 2.0
                     :cylinder (:roof-metal c))]
              (for [i (range rows)]
                (let [stack (+ 1 (prng/next-int! r 3))]
                  (part (+ (- hx) (* cw (+ i 0.5)))
                        (+ h (* 1.3 stack))
                        (* hz (prng/next-range! r -0.4 0.4))
                        (* cw 0.82) (* 2.6 stack) (* 2 hz 0.55)
                        :box (pick (:awning c) (:steel c) (:door c) (:sign c)))))))

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
            ;; A shade for this building's walls, drawn once and shared by
            ;; every one of them.
            ;;
            ;; The facade texture is per *zone*, so before this a street of
            ;; offices was not merely similar, it was the same building
            ;; repeated -- identical windows, identical brick, as far as the
            ;; fog. A multiplier rather than a colour, because it modulates
            ;; the texture rather than replacing it, and it is allowed above
            ;; 1.0 as well as below so the average street does not darken.
            shade (prng/next-range! r 0.78 1.18)
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
            ;; The last slot means two things, and which one is decided by the
            ;; slot before it: a packed colour for a flat part, and a shade
            ;; multiplier for a wall, whose colour is its zone's texture.
            (conj! parts (double (if (:facade? pt) shade (:tint pt))))))))
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
  index, and reordering these renames every crate in every saved world.

  The last two are not power-ups and hold nothing: they are points. Appending
  is safe, which is the whole reason this is a vector and not a set."
  [:repair :nitro :grip :armour :flame :shock :coin :nugget])

(def pickup-stride 4)          ; x y z kind
(def ^:private pickups-per-chunk 3)

(def ^:private coin-kind 6)
(def ^:private nugget-kind 7)
(def ^:private coin-runs 2)         ; trails of coins per chunk
(def ^:private coin-run-max 9)
(def ^:private coin-spacing 4.6)    ; metres between coins in a trail
(def ^:private nugget-odds 0.4)     ; per chunk
(def ^:private landmark-coins 16)   ; in the ring round a landmark

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
         out (transient [])
         emit (fn [x z kind lift]
                (conj! out x)
                ;; About a metre up: high enough to be seen over a kerb, low
                ;; enough that any car drives through it rather than under it.
                (conj! out (+ lift (height-at seed x z)))
                (conj! out z)
                (conj! out (double kind)))]
     (when (seq lines)
       (dotimes [_ pickups-per-chunk]
         (let [line (nth lines (prng/next-int! r (count lines)))
               pts (:points line)
               i (prng/next-int! r (dec (count pts)))
               t (prng/next-range! r 0.2 0.8)
               ;; Power-ups only: the last two entries are points, and they are
               ;; placed by the passes below rather than scattered like these.
               kind (prng/next-int! r (- (count pickup-kinds) 2))
               [ax az] (nth pts i)
               [bx bz] (nth pts (inc i))
               x (+ ax (* t (- bx ax)))
               z (+ az (* t (- bz az)))]
           (emit x z kind 1.0)))
       ;; Coins, in a line down one side of a carriageway rather than scattered.
       ;; A row of them is a *line to drive*, which is a different and much
       ;; better instruction than a dot to aim at -- it tells you where to put
       ;; the car for the next forty metres.
       (dotimes [_ coin-runs]
         (let [line (nth lines (prng/next-int! r (count lines)))
               pts (:points line)
               i (prng/next-int! r (dec (count pts)))
               side (if (prng/next-bool! r) 1.0 -1.0)
               ;; Drawn before it can be clamped, so the stream advances by the
               ;; same amount however long the segment turns out to be.
               want (+ 4 (prng/next-int! r (- coin-run-max 3)))
               start (prng/next-range! r 2.0 6.0)
               [ax az] (nth pts i)
               [bx bz] (nth pts (inc i))
               dx (- bx ax) dz (- bz az)
               len (max 1e-6 (hypot dx dz))
               ux (/ dx len) uz (/ dz len)
               off (* 0.45 (:half line) side)
               n (min want (long (/ (max 0.0 (- len start 1.0)) coin-spacing)))]
           (dotimes [j n]
             (let [d (+ start (* j coin-spacing))]
               (emit (+ ax (* ux d) (* (- uz) off))
                     (+ az (* uz d) (* ux off))
                     coin-kind 0.9)))))
       ;; And sometimes one big one, on its own, worth a short trail by itself.
       (let [roll (prng/next-double! r)
             line (nth lines (prng/next-int! r (count lines)))
             pts (:points line)
             i (prng/next-int! r (dec (count pts)))
             t (prng/next-range! r 0.25 0.75)
             [ax az] (nth pts i)
             [bx bz] (nth pts (inc i))]
         (when (< roll nugget-odds)
           (emit (+ ax (* t (- bx ax))) (+ az (* t (- bz az))) nugget-kind 1.1))))
     ;; And whatever is worth having at the landmark, if this chunk owns one.
     ;;
     ;; Until now there was no reason to drive to a landmark: you could see it
     ;; from a district away and there was nothing in it. A ring round the
     ;; apron rather than a heap in the middle, because a ring is a lap -- you
     ;; arrive, you go round, you leave -- and a heap is a full stop.
     ;;
     ;; Ground height is sampled once, at the centre, and reused for the whole
     ;; ring. That is not a shortcut: the landmark itself is built to one
     ;; height off one sample and given a plinth deep enough to bury the
     ;; slope, so a coin that followed the terrain instead would be the one
     ;; thing in the cell that did.
     (let [[dx dz] (district-of cx cz)]
       (doseq [ddx [-1 0 1], ddz [-1 0 1]
               :let [lm (landmark seed (+ dx ddx) (+ dz ddz))]
               :when lm
               :let [{:keys [x z half-x half-z]} lm
                     [ox oz] (chunk-of x z)]
               :when (and (= ox cx) (= oz cz))]
         (let [y0 (height-at seed x z)
               put (fn [px pz kind lift]
                     (conj! out px) (conj! out (+ y0 lift)) (conj! out pz)
                     (conj! out (double kind)))
               ph (prng/next-range! r 0.0 tau)]
           (dotimes [i landmark-coins]
             (let [a (+ ph (* tau (/ (double i) landmark-coins)))]
               (put (+ x (* 1.15 half-x (js-sin a)))
                    (+ z (* 1.15 half-z (js-cos a)))
                    coin-kind 0.9)))
           ;; One big one on the far side, so the lap is worth finishing, and
           ;; two power-ups where the ring meets the road.
           (put (+ x (* 1.15 half-x (js-sin (+ ph 3.14159265))))
                (+ z (* 1.15 half-z (js-cos (+ ph 3.14159265))))
                nugget-kind 1.1)
           (dotimes [i 2]
             (let [a (+ ph 1.5707963 (* i 3.14159265))]
               (put (+ x (* 1.35 half-x (js-sin a)))
                    (+ z (* 1.35 half-z (js-cos a)))
                    (prng/next-int! r (- (count pickup-kinds) 2))
                    1.0))))))
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
                    ;; Slower than a passer-by, because a group that walks
                    ;; at walking pace is a queue -- but not the 0.0 to 0.45
                    ;; this was, which reads as a group of people who have been
                    ;; switched off. At a fifth of a metre a second they mill
                    ;; about, and `panic` still multiplies it by two and a half
                    ;; when a car turns up, which is the other thing a group
                    ;; standing at zero could not do.
                    speed (prng/next-range! r 0.20 0.55)]
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
(def ^:private edge-inset 0.55)     ; m in from the kerb an edge line is painted

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
        ;;
        ;; Local streets get edge lines instead, and only in town. That is not
        ;; a stylistic choice, it is the only way they read as roads at all: a
        ;; local carriageway is 7.6 m wide and the terrain vertices it is
        ;; coloured on are 8 m apart, so the asphalt is one vertex wide and
        ;; the quad either side of it is a gradient from road to grass. The
        ;; result at any distance is a soft dark band that looks like mown
        ;; verge. Two crisp lines at the kerbs are geometry rather than vertex
        ;; colour, and they draw the edges the ground cannot.
        (when (> len 1.0)
          (let [ux (/ dx len) uz (/ dz len)
                rx (- uz) rz ux
                n  (long (floor (/ len centreline-spacing)))
                yaw (#?(:clj Math/atan2 :cljs js/Math.atan2) ux uz)
                ;; Nothing on a country lane, which has no markings on it in
                ;; life either and is the one place the soft edge is right.
                offs (cond (not= :local class) [0.0]
                           (> u lamp-urbanness) [(- edge-inset half)
                                                 (- half edge-inset)]
                           :else nil)]
            (when offs
              (dotimes [i n]
                (let [t (* (+ i 0.5) centreline-spacing)
                      ;; On a span the ground is the riverbed, so the paint has
                      ;; to follow the deck's chord instead.
                      y (if bridge?
                          (+ ya (* (/ t len) (- yb ya)))
                          (ground (+ ax (* ux t)) (+ az (* uz t))))]
                  (doseq [o offs]
                    (let [px (+ ax (* ux t) (* rx o))
                          pz (+ az (* uz t) (* rz o))]
                      (emit! out px (+ 0.02 y) pz yaw
                             (part-index :marking) 1.0 0 0.0))))))))
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
        ;; The two region fields, on a lattice of their own. Two more noise
        ;; calls per terrain vertex on top of the five already there is not
        ;; affordable, and sampling them once per chunk would step the ground
        ;; colour at every chunk border -- so they are sampled every 64 m and
        ;; interpolated between, which is a fiftieth of a region and smooth
        ;; enough that the join cannot be found.
        ;;
        ;; The lattice is in world coordinates, not chunk-relative, so the
        ;; samples on a chunk edge are the same numbers its neighbour uses and
        ;; the interpolation is continuous across the border. When this
        ;; sampled the four *corners* of the chunk it was continuous too, and
        ;; it was fine while a region was forty chunks across; at eight it
        ;; creased visibly along every boundary.
        rn    (inc (long (/ k/chunk-size region-lod)))
        ;; The *blend* is evaluated on the lattice, not the fields: four
        ;; numbers per lattice point, interpolated per vertex. Interpolating
        ;; its output rather than its input is not the same arithmetic, and at
        ;; 64 m spacing on a 2 km field the difference is below the precision
        ;; of a vertex colour.
        ;; Hinted, and the hint is not decoration: `aget` on an unhinted local
        ;; reflects, and the sixteen of them in the vertex loop below took
        ;; chunk generation from 27 ms to 76 ms without a warning anybody sees
        ;; unless they ask for one.
        ^doubles rq (darray (* rn rn 4))
        _     (let [^doubles scratch (darray 4)]
                (dotimes [rj rn]
                  (dotimes [ri rn]
                    (let [px (+ x0 (* ri region-lod))
                          pz (+ z0 (* rj region-lod))
                          o  (* 4 (+ (* rj rn) ri))]
                      (region-blend! scratch (warmth seed px pz) (damp seed px pz))
                      (dotimes [c 4] (dput! rq (+ o c) (aget scratch c)))))))
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
              ;; And what part of the world this is. Same mask as the crops --
              ;; a region colours its countryside, not its cities and not its
              ;; rivers -- and the same hue-cancelling arithmetic, because the
              ;; ground texture is green and sand is not a lighter green.
              ru   (/ (* i step) region-lod)
              rv'  (/ (* j step) region-lod)
              gi   (min (- rn 2) (long ru))
              gj   (min (- rn 2) (long rv'))
              fu   (- ru gi)
              fv   (- rv' gj)
              ;; Written out rather than looped: this is the innermost line of
              ;; the whole generator, and a helper closure here is eleven
              ;; hundred allocations a chunk.
              o00  (* 4 (+ (* gj rn) gi))
              o10  (+ o00 4)
              o01  (+ o00 (* 4 rn))
              o11  (+ o01 4)
              c00  (* (- 1.0 fu) (- 1.0 fv))
              c10  (* fu (- 1.0 fv))
              c01  (* (- 1.0 fu) fv)
              c11  (* fu fv)
              q    (* farm (+ (* c00 (aget rq o00)) (* c10 (aget rq o10))
                              (* c01 (aget rq o01)) (* c11 (aget rq o11))))
              qr   (+ (* c00 (aget rq (+ o00 1))) (* c10 (aget rq (+ o10 1)))
                      (* c01 (aget rq (+ o01 1))) (* c11 (aget rq (+ o11 1))))
              qg   (+ (* c00 (aget rq (+ o00 2))) (* c10 (aget rq (+ o10 2)))
                      (* c01 (aget rq (+ o01 2))) (* c11 (aget rq (+ o11 2))))
              qb   (+ (* c00 (aget rq (+ o00 3))) (* c10 (aget rq (+ o10 3)))
                      (* c01 (aget rq (+ o01 3))) (* c11 (aget rq (+ o11 3))))
              tr   (+ tr (* q (- (* gr qr) tr)))
              tg   (+ tg (* q (- (* gr qg) tg)))
              tb   (+ tb (* q (- (* gr qb) tb)))
              ;; Hardstanding. An estate is not built on grass -- the ground
              ;; between the sheds is concrete, oil and gravel, and it was
              ;; coming out the same bright green as a meadow because the only
              ;; thing greying the ground was how *built-up* a place is, and an
              ;; industrial estate is deliberately in the middle of that range.
              ;; Dry land only: the river runs through a works as it does
              ;; through anywhere else.
              ;; Like the roads and the water, this has to *cancel* the green
              ;; rather than merely lighten it: the ground texture is painted
              ;; green and the vertex colour multiplies it, so three numbers
              ;; that are nearly equal make pale grass, not concrete. The
              ;; first attempt was 1.16/1.08/1.02 -- greyed on paper, and
              ;; still a meadow on screen.
              ;;
              ;; Warmer and dirtier than the city's pale blue concrete: an
              ;; estate is oil and gravel, not a pavement.
              ind  (* (max 0.0 (- (industrialness seed x z) 0.55)) 2.2 (- 1.0 rv))
              ind  (min 1.0 ind)
              tr   (+ tr (* ind (- (* gr 1.22) tr)))
              tg   (+ tg (* ind (- (* gr 0.95) tg)))
              tb   (+ tb (* ind (- (* gr 1.12) tb)))
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
          {lm-parts :parts lm-rotors :rotors} (chunk-landmarks seed cx cz)
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
     :landmarks lm-parts
     :landmark-rotors lm-rotors
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
