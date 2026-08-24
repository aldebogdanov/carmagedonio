(ns carmageddon.shared.worldgen
  "Infinite world generation. Pure, seeded, and shared by client and server.

  Every feature derives from (world-seed, integer coordinates) and never from
  generation order, so chunks can be produced in any order, on any thread, on
  any machine, and still agree. That is what lets clients stream terrain with
  zero network traffic and lets the server reason about a place it has never
  simulated.

  Chunks stitch without communicating. Where two chunks meet, the road crossing
  point comes from `prng/edge-seed`, which canonicalises the pair of chunk
  coordinates -- so each side computes the identical portal independently, and
  the roads line up.

  Layout inside a chunk is hub-and-spoke: one hub near the middle, one portal on
  each of the four edges, a curved road from each portal to the hub. Guarantees
  a connected network in every direction forever. Biome-specific topologies
  (city grids, highway corridors) slot in later by replacing `spokes`."
  (:require [carmageddon.shared.constants :as k]
            [carmageddon.shared.noise :as noise]
            [carmageddon.shared.prng :as prng]))

;; Portable maths. Clojure's Math/* interop and CLJS's js/Math.* are not the
;; same forms, and float arrays are written differently on each platform.

(defn- sqrt  [x] #?(:clj (Math/sqrt x)  :cljs (js/Math.sqrt x)))
(defn- floor [x] #?(:clj (Math/floor x) :cljs (js/Math.floor x)))
(defn- hypot [x y] (sqrt (+ (* x x) (* y y))))

(defn- smootherstep-clamped [t]
  (let [t (max 0.0 (min 1.0 t))]
    (* t t (- 3.0 (* 2.0 t)))))

(defn- farray  [n] #?(:clj (float-array n)  :cljs (js/Float32Array. n)))
(defn- darray  [n] #?(:clj (double-array n) :cljs (js/Float64Array. n)))
(defn- fput! [a i v] #?(:clj (aset ^floats a i (float v))   :cljs (aset a i v)))
(defn- dput! [a i v] #?(:clj (aset ^doubles a i (double v)) :cljs (aset a i v)))
(defn- dget  [a i]   #?(:clj (aget ^doubles a i)            :cljs (aget a i)))
(defn- alen  [a]     #?(:clj (alength ^doubles a)           :cljs (.-length a)))

;; --- terrain shape ----------------------------------------------------------

(def terrain-scale 520.0)   ; metres per major undulation
(def terrain-amp    34.0)   ; peak-to-trough metres

;; Four octaves at a low gain rather than five at 0.5. The extra octaves add
;; detail with a ~25 m wavelength and roughly a metre of amplitude, which is
;; invisible from a distance but launches a car at speed -- measured at 45% of
;; ticks airborne before this was tuned down.
(def ^:private terrain-octaves 4)
(def ^:private terrain-gain 0.42)

(def road-half-width 6.0)   ; flat carriageway

;; Chosen to cancel the ground texture's green and land on dark neutral asphalt.
(def ^:private road-colour-r 0.55)
(def ^:private road-colour-g 0.36)
(def ^:private road-colour-b 0.68)
(def road-shoulder  18.0)   ; blend from carriageway back to open terrain

(defn base-height
  "Terrain before roads are cut into it."
  [seed x z]
  (* terrain-amp (- (noise/fbm2d seed (/ x terrain-scale) (/ z terrain-scale)
                                 terrain-octaves terrain-gain 2.0)
                    0.5)))

;; --- road network -----------------------------------------------------------

(def edge-neighbours {:west [-1 0] :east [1 0] :north [0 -1] :south [0 1]})
(def edge-order [:west :east :north :south])

(defn hub
  "World-space point every road in this chunk converges on."
  [seed cx cz]
  (let [r (prng/chunk-rng seed cx cz (:roads k/salt))
        u (prng/next-range! r 0.35 0.65)
        v (prng/next-range! r 0.35 0.65)]
    [(* (+ cx u) k/chunk-size) (* (+ cz v) k/chunk-size)]))

(defn portal
  "Where a road crosses the boundary between this chunk and its neighbour.

  Derived from the canonical edge seed, so the neighbour computes bit-identical
  coordinates from its own side without either chunk knowing the other exists."
  [seed cx cz edge]
  (let [[dx dz] (edge-neighbours edge)
        r (prng/make (prng/edge-seed seed cx cz (+ cx dx) (+ cz dz)))
        t (prng/next-range! r 0.25 0.75)]
    (if (zero? dz)
      [(* (if (pos? dx) (inc cx) cx) k/chunk-size)
       (* (+ cz t) k/chunk-size)]
      [(* (+ cx t) k/chunk-size)
       (* (if (pos? dz) (inc cz) cz) k/chunk-size)])))

;; --- biomes -----------------------------------------------------------------

(def ^:private city-threshold 0.56)
(def ^:private city-scale 6.0)      ; chunks per biome blob, so ~1.5 km of city

(defn biome
  "Which kind of place a chunk is. A function of chunk coordinates only, so a
  chunk has exactly one biome and neighbours can work out each other's without
  asking.

  Biome does not affect stitching: whatever a chunk does inside, it must reach
  the four edge portals, and those come from the shared edge seed."
  [seed cx cz]
  (if (> (noise/fbm2d (+ seed 5501) (/ cx city-scale) (/ cz city-scale) 3)
         city-threshold)
    :city
    :country))

(defn urbanness
  "How built-up a *point* is, in [0,1].

  `biome` is a hard per-chunk decision, which is right for layout but wrong for
  colour: using it to tint the ground would draw a visible seam along every
  chunk boundary where city meets country. This samples the same noise field
  continuously in world space and feathers it across the threshold, so the
  ground shades from grass to concrete gradually. At a chunk origin it agrees
  with `biome` exactly, because the coordinates work out the same."
  [seed x z]
  (let [d (* city-scale k/chunk-size)
        n (noise/fbm2d (+ seed 5501) (/ x d) (/ z d) 3)
        t (/ (- n (- city-threshold 0.09)) 0.18)]
    (smootherstep-clamped t)))

(def ^:private spoke-samples 12)

(defn- bezier
  "Quadratic curve from a to b, bowed sideways so roads are not spokes on a
  wheel. The bow is chunk-internal, so it needs no cross-chunk agreement."
  [[ax az] [bx bz] bow n]
  (let [mx (* 0.5 (+ ax bx))
        mz (* 0.5 (+ az bz))
        dx (- bx ax) dz (- bz az)
        len (max 1e-6 (hypot dx dz))
        ;; perpendicular offset
        px (* (- dz) (/ bow len))
        pz (* dx (/ bow len))
        cx (+ mx px) cz (+ mz pz)]
    (mapv (fn [i]
            (let [t (/ (double i) (dec n))
                  u (- 1.0 t)]
              [(+ (* u u ax) (* 2 u t cx) (* t t bx))
               (+ (* u u az) (* 2 u t cz) (* t t bz))]))
          (range n))))

(defn- country-lines
  "One bowed road from each edge portal in to the hub."
  [seed cx cz]
  (let [h (hub seed cx cz)]
    (mapv (fn [i edge]
            (let [p (portal seed cx cz edge)
                  r (prng/chunk-rng seed cx cz (+ 40 i))
                  bow (prng/next-range! r -28.0 28.0)]
              (bezier p h bow spoke-samples)))
          (range 4)
          edge-order)))

(def street-fractions [0.28 0.72])

(defn- city-lines
  "A grid of straight streets, plus a stub from each edge portal in to the
  nearest street of the perpendicular family.

  The stubs are what keep a city chunk stitchable: the grid alone would not
  reach the portals, and portals are the only thing neighbouring chunks agree
  on. Everything here is straight -- a city that curves is just a village."
  [seed cx cz]
  (let [x0 (* cx k/chunk-size)
        z0 (* cz k/chunk-size)
        at (fn [f] (* f k/chunk-size))
        verticals   (for [f street-fractions]
                      [[(+ x0 (at f)) z0] [(+ x0 (at f)) (+ z0 k/chunk-size)]])
        horizontals (for [f street-fractions]
                      [[x0 (+ z0 (at f))] [(+ x0 k/chunk-size) (+ z0 (at f))]])
        [wx wz] (portal seed cx cz :west)
        [ex ez] (portal seed cx cz :east)
        [nx nz] (portal seed cx cz :north)
        [sx sz] (portal seed cx cz :south)
        stubs [[[wx wz] [(+ x0 (at (first street-fractions))) wz]]
               [[ex ez] [(+ x0 (at (second street-fractions))) ez]]
               [[nx nz] [nx (+ z0 (at (first street-fractions)))]]
               [[sx sz] [sx (+ z0 (at (second street-fractions)))]]]]
    (mapv vec (concat verticals horizontals stubs))))

(defn spokes
  "The chunk's road polylines, whichever shape its biome calls for."
  [seed cx cz]
  (if (= :city (biome seed cx cz))
    (city-lines seed cx cz)
    (country-lines seed cx cz)))

(defn road-segments
  "Every road segment in the 3x3 chunk neighbourhood, flattened into
  [x1 z1 y1 x2 z2 y2 ...].

  The neighbourhood matters: a road just over the border still cuts the terrain
  on this side, and if each chunk only considered its own roads there would be a
  visible step at every boundary.

  Segment endpoint heights are taken from the *unroaded* terrain at the polyline
  ends and interpolated along it, which is what a real road does -- cut and fill
  to a gentle grade rather than following every bump."
  [seed cx cz]
  (let [out (transient [])]
    (doseq [ox [-1 0 1], oz [-1 0 1]]
      (let [ax (+ cx ox), az (+ cz oz)]
        (doseq [line (spokes seed ax az)]
          (let [[sx sz] (first line)
                [ex ez] (peek line)
                hs (base-height seed sx sz)
                he (base-height seed ex ez)
                n  (count line)]
            (dotimes [i (dec n)]
              (let [[x1 z1] (nth line i)
                    [x2 z2] (nth line (inc i))
                    t1 (/ (double i) (dec n))
                    t2 (/ (double (inc i)) (dec n))]
                (conj! out (double x1)) (conj! out (double z1))
                (conj! out (+ hs (* (- he hs) t1)))
                (conj! out (double x2)) (conj! out (double z2))
                (conj! out (+ hs (* (- he hs) t2)))))))))
    ;; Into a flat typed array: a chunk evaluates `nearest-road` ~1000 times
    ;; against a few hundred segments, so this is the hot loop of worldgen and
    ;; boxed vector access shows up plainly in a profile.
    (let [v (persistent! out)
          a (darray (count v))]
      (dotimes [i (count v)] (dput! a i (nth v i)))
      a)))

(defn- nearest-road
  "Squared distance to the closest road segment, and that road's height there.
  Returns [dist road-y]."
  [segs x z]
  (let [n (alen segs)]
    (loop [i 0, best 1e18, besty 0.0]
      (if (>= i n)
        [(sqrt best) besty]
        (let [x1 (dget segs i) z1 (dget segs (+ i 1)) y1 (dget segs (+ i 2))
              x2 (dget segs (+ i 3)) z2 (dget segs (+ i 4)) y2 (dget segs (+ i 5))
              dx (- x2 x1) dz (- z2 z1)
              len2 (+ (* dx dx) (* dz dz))
              t (if (< len2 1e-9)
                  0.0
                  (max 0.0 (min 1.0 (/ (+ (* (- x x1) dx) (* (- z z1) dz)) len2))))
              px (+ x1 (* t dx)) pz (+ z1 (* t dz))
              ddx (- x px) ddz (- z pz)
              d2 (+ (* ddx ddx) (* ddz ddz))]
          (if (< d2 best)
            (recur (+ i 6) d2 (+ y1 (* t (- y2 y1))))
            (recur (+ i 6) best besty)))))))

(defn- smoothstep [t] (* t t (- 3.0 (* 2.0 t))))


(defn surface
  "Final height at a point plus how road-like it is, in [0,1].

  `segs` is the neighbourhood road set from `road-segments`; it is passed in
  rather than recomputed because a chunk evaluates this ~1000 times."
  [seed segs x z]
  (let [[d ry] (nearest-road segs x z)]
    (cond
      (<= d road-half-width) [ry 1.0]
      (>= d (+ road-half-width road-shoulder)) [(base-height seed x z) 0.0]
      :else
      (let [s (smoothstep (/ (- d road-half-width) road-shoulder))
            b (base-height seed x z)]
        [(+ ry (* (- b ry) s)) (- 1.0 s)]))))

(defn chunk-of [x z]
  [(long (floor (/ x k/chunk-size)))
   (long (floor (/ z k/chunk-size)))])

(defn height-at
  "Convenience for one-off queries (spawning, placing objects). Regenerates the
  neighbourhood road set, so do not call it in a loop."
  [seed x z]
  (let [[cx cz] (chunk-of x z)]
    (first (surface seed (road-segments seed cx cz) x z))))

;; --- chunk assembly ---------------------------------------------------------

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

  Props are placed *along* this chunk's own roads and pushed sideways clear of
  the carriageway, rather than scattered over the chunk and rejected when they
  miss. Scattering wasted three quarters of every batch, because the roadside
  band is about 12 m wide out of 256. Using this chunk's spokes only -- not the
  neighbourhood set -- keeps each prop owned by exactly one chunk."
  [seed cx cz segs]
  (let [r     (prng/chunk-rng seed cx cz (:props k/salt))
        lines (spokes seed cx cz)
        out   (transient [])]
    (dotimes [_ props-per-chunk]
      (let [line  (nth lines (prng/next-int! r (count lines)))
            n     (count line)
            i     (prng/next-int! r (dec n))
            t     (prng/next-double! r)
            side  (if (prng/next-bool! r) 1.0 -1.0)
            off   (+ road-half-width (prng/next-range! r 0.8 6.5))
            kind  (prng/next-int! r (count prop-kinds))
            yaw   (prng/next-range! r 0.0 6.2831853)
            scale (prng/next-range! r 0.8 1.35)
            [ax az] (nth line i)
            [bx bz] (nth line (inc i))
            px (+ ax (* t (- bx ax)))
            pz (+ az (* t (- bz az)))
            dx (- bx ax) dz (- bz az)
            len (max 1e-6 (hypot dx dz))
            ;; perpendicular to the road at this point
            ux (* (- dz) (/ side len))
            uz (* dx (/ side len))
            ;; Perpendicular to *this* spoke, but near the hub all four spokes
            ;; converge, so stepping clear of one road often lands on another.
            ;; Try progressively further out and take the first spot that is not
            ;; on a carriageway. Multipliers are fixed, not drawn, so this costs
            ;; no extra randomness and stays identical on every machine.
            [x z] (loop [ms [1.0 1.9 3.0 4.4]]
                    (let [mul (first ms)
                          cx' (+ px (* ux off mul))
                          cz' (+ pz (* uz off mul))]
                      (if (or (empty? (rest ms))
                              (<= (second (surface seed segs cx' cz')) 0.85))
                        [cx' cz']
                        (recur (rest ms)))))
            [y _] (surface seed segs x z)]
        (conj! out x) (conj! out y) (conj! out z)
        (conj! out yaw) (conj! out (double kind)) (conj! out scale)))
    (let [v (persistent! out)
          a (farray (count v))]
      (dotimes [i (count v)] (fput! a i (nth v i)))
      a)))


;; --- buildings --------------------------------------------------------------

(def building-stride 7)      ; x y z hx hz height kind
(def building-kinds 4)
(def ^:private building-attempts 48)

(defn chunk-buildings
  "City blocks, as a flat array of [x y z hx hz height kind ...]. Empty outside
  city chunks.

  A candidate is rejected if its centre or any corner is anywhere near a street,
  which is cheaper and far more robust than trying to compute the blocks between
  streets analytically -- it keeps working when the road layout changes.

  Ground height is the *lowest* of the four corners, so a building on a slope
  cuts into the hill instead of floating over the downhill side."
  [seed cx cz segs]
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
              samples (mapv (fn [[px pz]] (surface seed segs px pz)) corners)
              ;; 0.12 rather than 0: the shoulder that blends terrain back to
              ;; open ground is 18 m wide, and demanding a building clear all of
              ;; it left almost nothing standing in a 72 m block.
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
  [seed cx cz segs]
  (let [r     (prng/chunk-rng seed cx cz (:peds k/salt))
        lines (spokes seed cx cz)
        n     (peds-per-chunk seed cx cz)
        out   (transient [])]
    (dotimes [_ n]
      (let [line  (nth lines (prng/next-int! r (count lines)))
            cnt   (count line)
            i     (prng/next-int! r (dec cnt))
            t     (prng/next-double! r)
            side  (if (prng/next-bool! r) 1.0 -1.0)
            off   (prng/next-range! r 1.0 (+ road-half-width 3.0))
            head  (prng/next-range! r 0.0 6.2831853)
            speed (prng/next-range! r 0.7 1.9)
            [ax az] (nth line i)
            [bx bz] (nth line (inc i))
            px (+ ax (* t (- bx ax)))
            pz (+ az (* t (- bz az)))
            dx (- bx ax) dz (- bz az)
            len (max 1e-6 (hypot dx dz))
            x (+ px (* side (* (- dz) (/ off len))))
            z (+ pz (* side (* dx (/ off len))))
            [y _] (surface seed segs x z)]
        (conj! out x) (conj! out y) (conj! out z)
        (conj! out head) (conj! out speed)))
    (let [v (persistent! out)
          a (farray (count v))]
      (dotimes [i (count v)] (fput! a i (nth v i)))
      a)))

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
        segs  (road-segments seed cx cz)
        props (chunk-props seed cx cz segs)
        buildings (chunk-buildings seed cx cz segs)
        peds  (chunk-peds seed cx cz segs)
        heights (farray (* n n))
        colors  (farray (* n n 3))]
    (dotimes [j n]                       ; j indexes z
      (dotimes [i n]                     ; i indexes x
        (let [x (+ x0 (* i step))
              z (+ z0 (* j step))
              [y road] (surface seed segs x z)
              idx (+ (* i n) j)
              ;; Vertex colour multiplies the tiled ground texture, which is
              ;; green. Simply darkening it for roads gives dark grass, not
              ;; asphalt -- so the road colour has to actively cancel the
              ;; texture's hue (green down, red and blue up) to land on neutral
              ;; grey. `road` blends between the two across the shoulder.
              dirt (noise/fbm2d (+ seed 977) (/ x 26.0) (/ z 26.0) 3)
              d    (* 0.55 (max 0.0 (- dirt 0.45)))
              gr   (+ 0.80 (* 0.30 dirt))
              tr   (* gr (+ 1.0 (* 0.55 d)))
              tg   (* gr (- 1.0 (* 0.05 d)))
              tb   (* gr (- 1.0 (* 0.40 d)))
              ;; Cities stand on concrete, not lawn. Same hue-cancelling trick
              ;; as the roads: the ground texture is green, so grey has to be
              ;; mixed toward by lifting red and blue and dropping green.
              u    (urbanness seed x z)
              ;; Pale concrete, not grey: the road colour below is dark asphalt,
              ;; and if urban ground lands on the same value the streets vanish
              ;; into the pavement and a city reads as one flat slab.
              tr   (+ tr (* u (- (* gr 1.12) tr)))
              tg   (+ tg (* u (- (* gr 0.78) tg)))
              tb   (+ tb (* u (- (* gr 1.24) tb)))
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
  "Somewhere on the road network of the origin chunk.

  Taken from an actual road polyline rather than from `hub`: the hub is only a
  road in country chunks, and in a city it is an arbitrary point that can land
  inside a block."
  [seed]
  (let [line (first (spokes seed 0 0))
        [x z] (nth line (quot (count line) 2))]
    [x (+ 1.2 (height-at seed x z)) z]))
