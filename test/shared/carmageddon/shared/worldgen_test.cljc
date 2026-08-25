(ns carmageddon.shared.worldgen-test
  "The properties these assert are the ones that make an infinite world possible
  at all. If chunks stop agreeing at their borders, terrain develops cliffs at
  every boundary and roads stop connecting -- and in multiplayer, two clients
  end up driving on different ground."
  (:require [clojure.test :refer [deftest is testing]]
            [carmageddon.shared.constants :as k]
            [carmageddon.shared.worldgen :as w]))

(def seed 20260823)

(defn- aget* [a i] #?(:clj (aget ^floats a i) :cljs (aget a i)))
(defn- alen* [a]   #?(:clj (alength ^floats a) :cljs (.-length a)))
(defn- dget* [a i] #?(:clj (aget ^doubles a i) :cljs (aget a i)))
(defn- dlen* [a]   #?(:clj (alength ^doubles a) :cljs (.-length a)))

;; --- the lattice ------------------------------------------------------------

(deftest a-node-is-a-function-of-its-own-coordinates
  (testing "so every chunk that can see a node computes the same point"
    (doseq [gx [-9 0 4 17], gz [-3 0 8 25]]
      (is (= (w/node seed gx gz) (w/node seed gx gz))))))

(deftest streets-are-identical-however-you-ask-for-them
  (testing "two overlapping queries return the same streets in the overlap"
    (let [a (w/streets-in-bounds seed 0.0 0.0 500.0 500.0)
          b (w/streets-in-bounds seed 250.0 250.0 750.0 750.0)
          ;; Both endpoints well inside both boxes: near the rim of a query the
          ;; two callers legitimately see different streets, because each only
          ;; reaches one lattice cell past what it was asked for.
          in-overlap (fn [s] (every? (fn [[x z]] (and (< 200.0 x 480.0)
                                                      (< 200.0 z 480.0)))
                                     (:points s)))
          sa (set (filter in-overlap a))
          sb (set (filter in-overlap b))]
      (is (seq sa) "expected streets in the overlap")
      (is (= sa sb) "the same street came out differently for the two callers"))))

(deftest arterials-exist-everywhere
  (testing "the network is connected even in empty wilderness"
    (doseq [cx [-40 -7 0 23 61], cz [-31 0 12 44]]
      (let [streets (w/chunk-lines seed cx cz)
            near    (w/streets-in-bounds seed
                                         (* cx k/chunk-size) (* cz k/chunk-size)
                                         (* (inc cx) k/chunk-size) (* (inc cz) k/chunk-size))]
        (is (seq near) (str "chunk " [cx cz] " had no streets at all"))
        ;; Nothing to assert about ownership beyond it being a subset.
        (is (every? (set near) streets))))))

(deftest arterials-run-straight
  (testing "an arterial's nodes barely leave the line, or a main road kinks"
    (doseq [gz [0 8 -16]]
      (doseq [gx (range -6 7)]
        (let [[_ z] (w/node seed gx gz)
              want  (* gz w/street-spacing)]
          (is (< (abs (- z want)) 3.0)
              (str "arterial line " gz " kinked by " (abs (- z want)) " m at " gx)))))))

(deftest cities-are-denser-than-countryside
  (let [density (fn [b]
                  (let [cs (for [cx (range -14 15), cz (range -14 15)
                                 :when (= b (w/biome seed cx cz))]
                             (count (w/chunk-lines seed cx cz)))]
                    (when (seq cs) (/ (reduce + cs) (double (count cs))))))
        city (density :city)
        country (density :country)]
    (is (and city country) "expected both biomes in range")
    (is (> city (* 2.0 country))
        (str "city " city " streets/chunk vs country " country))))

;; --- stitching --------------------------------------------------------------

(deftest terrain-is-continuous-across-chunk-borders
  (testing "a point on a shared edge gets the same height from either chunk"
    (doseq [[a b] [[[0 0] [1 0]] [[0 0] [0 1]] [[-1 3] [0 3]] [[4 -2] [4 -1]]]]
      (let [[ax az] a
            [bx bz] b
            fa (w/road-field seed ax az)
            fb (w/road-field seed bx bz)
            vertical? (not= ax bx)
            pts (for [t (range 0.05 1.0 0.1)]
                  (if vertical?
                    [(* (inc ax) k/chunk-size) (* (+ az t) k/chunk-size)]
                    [(* (+ ax t) k/chunk-size) (* (inc az) k/chunk-size)]))]
        (doseq [[x z] pts]
          (let [[ha ra] (w/surface seed fa x z)
                [hb rb] (w/surface seed fb x z)]
            (is (< (abs (- ha hb)) 1e-6)
                (str "seam step of " (abs (- ha hb)) " m between " a " and " b
                     " at " [x z]))
            (is (< (abs (- ra rb)) 1e-6) "and the road blend must match too")))))))

(deftest the-road-index-loses-nothing
  (testing "indexed lookup agrees with scanning every segment in the field"
    (let [cx 2 cz 1
          field (w/road-field seed cx cz)
          segs  (:segs field)
          n     (long (/ (dlen* segs) 8))
          ;; An independent, deliberately naive implementation of the same
          ;; question the uniform grid answers. If the grid ever drops a bucket
          ;; the two diverge, and the symptom in the game would be a strip of
          ;; road that quietly stops flattening the ground.
          brute (fn [x z]
                  (loop [i 0, best 0.0, wsum 0.0, wy 0.0]
                    (if (>= i n)
                      (if (pos? wsum) [best (/ wy wsum)] [0.0 0.0])
                      (let [o (* i 8)
                            x1 (dget* segs o) z1 (dget* segs (+ o 1)) y1 (dget* segs (+ o 2))
                            x2 (dget* segs (+ o 3)) z2 (dget* segs (+ o 4)) y2 (dget* segs (+ o 5))
                            half (dget* segs (+ o 6)) sh (dget* segs (+ o 7))
                            dx (- x2 x1) dz (- z2 z1)
                            l2 (+ (* dx dx) (* dz dz))
                            t (if (< l2 1e-9) 0.0
                                  (max 0.0 (min 1.0 (/ (+ (* (- x x1) dx) (* (- z z1) dz)) l2))))
                            px (+ x1 (* t dx)) pz (+ z1 (* t dz))
                            d (Math/sqrt (+ (* (- x px) (- x px)) (* (- z pz) (- z pz))))
                            r (cond (<= d half) 1.0
                                    (>= d (+ half sh)) 0.0
                                    :else (let [u (/ (- d half) sh)]
                                            (- 1.0 (* u u (- 3.0 (* 2.0 u))))))]
                        (if (pos? r)
                          (recur (inc i) (max best r) (+ wsum r) (+ wy (* r (+ y1 (* t (- y2 y1))))))
                          (recur (inc i) best wsum wy))))))]
      (is (pos? n) "the field should contain segments")
      (doseq [i (range 0 17), j (range 0 17)]
        (let [x (+ (* cx k/chunk-size) (* i 16.0))
              z (+ (* cz k/chunk-size) (* j 16.0))
              [br by] (brute x z)
              [sy sr] (w/surface seed field x z)
              want (if (pos? br)
                     (let [b (w/base-height seed x z)] (+ by (* (- b by) (- 1.0 br))))
                     (w/base-height seed x z))]
          (is (< (abs (- sr br)) 1e-9) (str "roadness mismatch at " [x z]))
          (is (< (abs (- sy want)) 1e-6) (str "height mismatch at " [x z])))))))

(deftest chunk-data-is-deterministic
  (let [a (w/chunk-data seed 3 -2)
        b (w/chunk-data seed 3 -2)]
    (is (= (alen* (:heights a)) (* k/chunk-verts k/chunk-verts)))
    (is (every? #(= (aget* (:heights a) %) (aget* (:heights b) %))
                (range (alen* (:heights a)))))
    (is (every? #(= (aget* (:colors a) %) (aget* (:colors b) %))
                (range (alen* (:colors a)))))))

(deftest chunk-heights-match-the-analytic-surface
  (testing "the sampled grid is the same function the collider will be built from"
    (let [cx 2 cz 1
          {:keys [heights verts]} (w/chunk-data seed cx cz)
          field (w/road-field seed cx cz)
          step (/ k/chunk-size (dec verts))]
      (doseq [i [0 7 16 32], j [0 5 21 32]]
        (let [x (+ (* cx k/chunk-size) (* i step))
              z (+ (* cz k/chunk-size) (* j step))
              expected (first (w/surface seed field x z))
              ;; x selects the row, z varies fastest -- the layout Rapier wants
              actual   (aget* heights (+ (* i verts) j))]
          (is (< (abs (- expected actual)) 1e-3)
              (str "grid/surface mismatch at " [i j])))))))

(def ^:private js-two-pi (* 2.0 Math/PI))

(defn- part-index-of
  "Index of a keyword in a parts vector, by value. Not `.indexOf` on an array
  of keywords: that compares by identity, which keywords do not guarantee."
  [parts kw]
  (first (keep-indexed (fn [i p] (when (= p kw) i)) parts)))

(defn- part-of
  "Index of a furniture part. Not `.indexOf` on an array of keywords: that
  compares by identity, which keywords do not guarantee."
  [kw]
  (first (keep-indexed (fn [i p] (when (= p kw) i)) w/furniture-parts)))

(defn- first-chunk-of [b]
  (first (for [cx (range 0 24), cz (range 0 6)
               :when (= b (w/biome seed cx cz))]
           [cx cz])))

(deftest roads-are-flat-and-terrain-is-not
  (testing "every biome's own roads are roads"
    (doseq [b [:country :city]]
      (let [[cx cz] (first-chunk-of b)
            field (w/road-field seed cx cz)
            pts   (:points (first (w/chunk-lines seed cx cz)))
            [x z] (nth pts (quot (count pts) 2))]
        (is (= 1.0 (second (w/surface seed field x z)))
            (str b " street midpoint was not flat")))))
  (testing "open ground away from any road is not road-like"
    (let [[cx cz] (first-chunk-of :country)
          field (w/road-field seed cx cz)
          ;; Walk the chunk and take the least road-like spot there is: with
          ;; streets 64 m apart, an arbitrary corner may well be on one.
          worst (apply min (for [i (range 33), j (range 33)]
                             (second (w/surface seed field
                                                (+ (* cx k/chunk-size) (* i 8.0))
                                                (+ (* cz k/chunk-size) (* j 8.0))))))]
      (is (< worst 0.05) "a country chunk should have open ground in it"))))

(deftest biome-is-stable-and-mixed
  (testing "a function of chunk coordinates only"
    (is (= (w/biome seed 3 -7) (w/biome seed 3 -7))))
  (testing "both biomes actually occur"
    (let [mix (frequencies (for [cx (range -12 13), cz (range -12 13)] (w/biome seed cx cz)))]
      (is (pos? (:city mix 0)))
      (is (pos? (:country mix 0))))))

(defn- densest-chunk
  "A chunk whose centre satisfies `pred` on (urbanness, industrialness)."
  [pred]
  (first (for [cx (range -30 40), cz (range -20 20)
               :let [x (* (+ cx 0.5) k/chunk-size)
                     z (* (+ cz 0.5) k/chunk-size)]
               :when (pred (w/urbanness seed x z) (w/industrialness seed x z))]
           [cx cz])))

(deftest lots-are-cut-from-blocks-not-scattered
  (let [[cx cz] (densest-chunk (fn [u _] (> u 0.85)))
        field (w/road-field seed cx cz)
        lots (w/chunk-lots seed cx cz)]
    (is (> (count lots) 40) "a downtown chunk should be full of plots")

    (testing "no plot sits on a carriageway"
      (let [on-road (count (filter (fn [{:keys [x z]}]
                                     (>= (second (w/surface seed field x z)) 0.9999))
                                   lots))]
        (is (zero? on-road) (str on-road " plots straddled a road"))))

    (testing "plots do not overlap: a shared boundary is drawn once, so
              neighbours meet exactly rather than fighting over the ground"
      (let [rs (mapv (fn [{:keys [x z hx hz]}]
                       [(- x hx) (- z hz) (+ x hx) (+ z hz)])
                     lots)
            clash (for [i (range (count rs)), j (range (inc i) (count rs))
                        :let [[ax0 az0 ax1 az1] (nth rs i)
                              [bx0 bz0 bx1 bz1] (nth rs j)]
                        ;; A hair of tolerance: shared edges touch by design.
                        :when (and (> (min ax1 bx1) (+ 0.01 (max ax0 bx0)))
                                   (> (min az1 bz1) (+ 0.01 (max az0 bz0))))]
                    [i j])]
        (is (empty? (take 5 clash))
            (str (count clash) " overlapping pairs, e.g. " (first clash)))))

    (testing "plots face the street rather than away from it"
      (let [fronts (for [{:keys [x z hx hz yaw]} lots
                         ;; local -Z is the frontage, local +Z the back yard
                         :let [sx (abs (Math/sin yaw)) ; 1 for east/west sides
                               dx (* (Math/sin yaw) (+ (if (> sx 0.5) hx hz) 6.0))
                               dz (* (Math/cos yaw) (+ (if (> sx 0.5) hx hz) 6.0))]]
                     [(second (w/surface seed field (- x dx) (- z dz)))
                      (second (w/surface seed field (+ x dx) (+ z dz)))])
            better (count (filter (fn [[front back]] (> front back)) fronts))]
        (is (> (/ better (double (count fronts))) 0.8)
            (str "only " better " of " (count fronts)
                 " plots had more road in front than behind"))))))

(deftest zoning-follows-density
  (let [zones-at (fn [pred]
                   (let [[cx cz] (densest-chunk pred)]
                     (set (map :zone (w/chunk-lots seed cx cz)))))
        downtown (zones-at (fn [u _] (> u 0.88)))
        country  (zones-at (fn [u _] (< u 0.10)))
        works    (zones-at (fn [u ind] (and (> ind 0.74) (< 0.3 u 0.7))))]
    (testing "downtown builds up, not out"
      (is (contains? downtown :office))
      (is (not (contains? downtown :barn)))
      (is (not (contains? downtown :factory)) "works belong at the edge of town"))
    (testing "open country is farmland with the odd barn"
      (is (contains? country :open))
      (is (not (contains? country :office)))
      (is (not (contains? country :apartment))))
    (testing "the industrial field actually places industry"
      (is (some works #{:factory :warehouse})))))

(deftest buildings-stand-on-their-plots
  (let [[cx cz] (densest-chunk (fn [u _] (> u 0.85)))
        field (w/road-field seed cx cz)
        b (w/chunk-buildings seed cx cz field)
        n (/ (alen* b) w/building-stride)]
    (is (pos? n) "a downtown chunk should have buildings")

    (testing "same seed, same street"
      (is (= (vec (map #(aget* b %) (range (alen* b))))
             (vec (map #(aget* (w/chunk-buildings seed cx cz field) %)
                       (range (alen* b)))))))

    (testing "no building stands in a road"
      (is (every? (fn [i]
                    (let [o (* i w/building-stride)]
                      (< (second (w/surface seed field (aget* b o) (aget* b (+ o 2))))
                         0.35)))
                  (range n))))

    (testing "every zone index names a real zone, and yaw is square to a street"
      (is (every? (fn [i]
                    (let [o (* i w/building-stride)
                          zi (aget* b (+ o 6))
                          yaw (aget* b (+ o 7))]
                      (and (<= 0 zi) (< zi (count w/building-zones))
                           ;; Yaw is one of the four side headings, so a facade
                           ;; is always parallel to the street it fronts.
                           (< (abs (- (abs (Math/sin yaw))
                                      (Math/round (abs (Math/sin yaw)))))
                              1e-6))))
                  (range n))))

    (testing "footprints and heights are within their zone's range"
      (is (every? (fn [i]
                    (let [o (* i w/building-stride)
                          {:keys [height]} (nth w/building-zones (int (aget* b (+ o 6))))
                          [h0 h1] height
                          h (aget* b (+ o 5))]
                      ;; Density scales height inside the range by 0.72..1.10.
                      (and (> (aget* b (+ o 3)) 1.9)
                           (> (aget* b (+ o 4)) 1.9)
                           (<= (* 0.71 h0) h (* 1.11 h1)))))
                  (range n))))

    (testing "a building meets the ground on its lowest corner"
      (is (every? (fn [i]
                    (let [o (* i w/building-stride)
                          x (aget* b o) z (aget* b (+ o 2))
                          hx (aget* b (+ o 3)) hz (aget* b (+ o 4))
                          lo (reduce min (for [px [(- x hx) x (+ x hx)]
                                               pz [(- z hz) z (+ z hz)]]
                                           (first (w/surface seed field px pz))))]
                      (< (abs (- (aget* b (+ o 1)) lo)) 0.35)))
                  (range n))))))

(deftest heights-stay-within-the-terrain-envelope
  (let [{:keys [heights]} (w/chunk-data seed -5 9)
        vs (map #(aget* heights %) (range (alen* heights)))]
    (is (every? #(< (abs %) (* 1.0 w/terrain-amp)) vs))
    (is (> (- (apply max vs) (apply min vs)) 0.5) "chunk should not be perfectly flat")))

(deftest spawn-is-on-a-road
  (let [{:keys [pos dir]} (w/spawn-point seed)
        [x _ z] pos
        [dx dz] dir
        [cx cz] (w/chunk-of x z)
        field (w/road-field seed cx cz)]
    (is (= 1.0 (second (w/surface seed field x z))) "the spawn itself")
    (is (< (abs (- 1.0 (Math/sqrt (+ (* dx dx) (* dz dz))))) 1e-9)
        "the street direction is a unit vector")
    (testing "and so is the road behind it, where the opponents queue up"
      (doseq [d [8.0 16.0 24.0]]
        (let [ox (- x (* dx d)) oz (- z (* dz d))
              [ocx ocz] (w/chunk-of ox oz)]
          (is (= 1.0 (second (w/surface seed (w/road-field seed ocx ocz) ox oz)))
              (str d " m back was not on the carriageway")))))))

;; --- clutter ----------------------------------------------------------------

(deftest props-are-deterministic-and-roadside
  (let [field (w/road-field seed 2 1)
        a (w/chunk-props seed 2 1 field)
        b (w/chunk-props seed 2 1 field)
        n (/ (alen* a) w/prop-stride)]
    (testing "same seed, same clutter"
      (is (= (vec (map #(aget* a %) (range (alen* a))))
             (vec (map #(aget* b %) (range (alen* b)))))))
    (testing "every candidate is placed -- acceptance must not depend on terrain,
              or two machines could disagree about how far the stream advanced"
      (is (= w/props-per-chunk n)))
    (testing "kinds are in range"
      (is (every? (fn [i] (let [kk (aget* a (+ 4 (* i w/prop-stride)))]
                            (and (<= 0 kk) (< kk (count w/prop-kinds)))))
                  (range n))))
    (testing "props sit on the surface, not floating or buried"
      (is (every? (fn [i]
                    (let [o (* i w/prop-stride)
                          x (aget* a o) y (aget* a (+ o 1)) z (aget* a (+ o 2))]
                      (< (abs (- y (first (w/surface seed field x z)))) 1e-3)))
                  (range n))))))

(deftest props-mostly-avoid-the-carriageway
  (testing "clutter lines the road rather than blocking it"
    (let [roadness (for [cx (range 0 4), cz (range 0 4)
                         :let [field (w/road-field seed cx cz)
                               a (w/chunk-props seed cx cz field)]
                         i (range (/ (alen* a) w/prop-stride))]
                     (second (w/surface seed field
                                        (aget* a (* i w/prop-stride))
                                        (aget* a (+ 2 (* i w/prop-stride))))))
          total (count roadness)
          on-road (count (filter #(> % 0.9) roadness))]
      (is (pos? total))
      (is (< (/ on-road (double total)) 0.15)
          (str on-road " of " total " props sat on the carriageway")))))

(deftest props-belong-to-exactly-one-chunk
  (testing "placed from streets this chunk owns, so neighbours cannot duplicate them"
    (doseq [[cx cz] [[0 0] [3 -2]]]
      (let [field (w/road-field seed cx cz)
            a (w/chunk-props seed cx cz field)
            lo-x (* cx k/chunk-size) lo-z (* cz k/chunk-size)]
        (doseq [i (range (/ (alen* a) w/prop-stride))]
          (let [o (* i w/prop-stride)
                x (aget* a o) z (aget* a (+ o 2))]
            ;; A prop may spill past its own edge -- a street straddling the
            ;; border is still owned by whichever chunk holds its midpoint --
            ;; but never as far as the neighbour's interior.
            (is (and (< (- lo-x 90) x (+ lo-x k/chunk-size 90))
                     (< (- lo-z 90) z (+ lo-z k/chunk-size 90)))
                (str "prop " i " of chunk " [cx cz] " strayed to " [x z]))))))))

;; --- junctions and street furniture ----------------------------------------

(deftest signals-never-let-both-groups-move
  (testing "green plus amber is exactly half the cycle, so the two axes cannot
            both be moving at any instant"
    (doseq [offset [0.0 3.7 11.2 23.9]]
      (doseq [ms (range 0 24000 137)]
        (let [t (/ ms 1000.0)
              a (w/signal-state t offset 0)
              b (w/signal-state t offset 1)]
          (is (or (= :red a) (= :red b))
              (str "at t=" t " offset=" offset " groups showed " a "/" b)))))))

(deftest signals-do-cycle
  (testing "a group is not stuck on one colour"
    (let [seen (set (for [ms (range 0 24000 100)] (w/signal-state (/ ms 1000.0) 0.0 0)))]
      (is (= #{:green :amber :red} seen)))))

(deftest junctions-need-three-arms
  (testing "a bend is not a junction and gets no furniture"
    (doseq [gx (range -6 7), gz (range -6 7)]
      (when-let [j (w/junction seed gx gz)]
        (is (>= (:degree j) 3) (str "junction at " [gx gz] " had degree " (:degree j)))
        (is (= (:pos j) (w/node seed gx gz)) "a junction stands on its own node")
        (is (contains? #{:signals :priority :uncontrolled} (:kind j)))))))

(deftest signals-only-where-a-real-road-is-crossed
  (testing "the bug this guards: asking whether *any* arm is an arterial puts a
            set of lights at every node along one, i.e. every 64 m"
    (let [rank {:local 0 :collector 1 :arterial 2}
          axis (fn [arms along-x?]
                 (let [r (for [{:keys [dir class]} arms
                               :let [[dx dz] dir]
                               :when (= along-x? (> (abs dx) (abs dz)))]
                           (rank class))]
                   (if (seq r) (apply max r) -1)))]
      (doseq [gx (range -10 11), gz (range -10 11)]
        (when-let [j (w/junction seed gx gz)]
          (when (= :signals (:kind j))
            (let [ax (axis (:arms j) true)
                  az (axis (:arms j) false)]
              (is (>= (min ax az) 1)
                  (str "signals at " [gx gz] " where the crossing road is only "
                       "class " (min ax az))))))))))

(deftest signalled-junctions-are-not-on-every-corner
  (testing "roughly one per city chunk, not one per block"
    (let [[cx cz] (first-chunk-of :city)
          kinds (frequencies (map :kind (w/chunk-junctions seed cx cz)))]
      (is (pos? (reduce + (vals kinds))) "a city chunk should have junctions")
      (is (<= (:signals kinds 0) 3)
          (str "too many signalled junctions in one chunk: " kinds)))))

(deftest furniture-is-well-formed
  (let [[cx cz] (first-chunk-of :city)
        field (w/road-field seed cx cz)
        a (w/chunk-furniture seed cx cz field)
        n (/ (alen* a) w/furniture-stride)]
    (is (pos? n) "a city chunk should have street furniture")
    (testing "same seed, same street"
      (is (= (vec (map #(aget* a %) (range (alen* a))))
             (vec (map #(aget* (w/chunk-furniture seed cx cz field) %) (range (alen* a)))))))
    (testing "every part index names a real part"
      (is (every? (fn [i]
                    (let [p (aget* a (+ 4 (* i w/furniture-stride)))]
                      (and (<= 0 p) (< p (count w/furniture-parts)))))
                  (range n))))
    (testing "poles stand on the ground rather than floating or buried"
      (is (every? (fn [i]
                    (let [o (* i w/furniture-stride)]
                      (or (not= 0.0 (aget* a (+ o 4)))   ; not a pole
                          (< (abs (- (aget* a (+ o 1))
                                     (first (w/surface seed field (aget* a o)
                                                       (aget* a (+ o 2))))))
                             1e-3))))
                  (range n))))
    (testing "nothing strays into a neighbour's interior"
      (let [lo-x (* cx k/chunk-size) lo-z (* cz k/chunk-size)]
        (is (every? (fn [i]
                      (let [o (* i w/furniture-stride)]
                        (and (< (- lo-x 70) (aget* a o) (+ lo-x k/chunk-size 70))
                             (< (- lo-z 70) (aget* a (+ o 2)) (+ lo-z k/chunk-size 70)))))
                    (range n)))))))

(deftest crossings-lie-on-the-carriageway
  (testing "a zebra painted on the pavement is worse than no zebra"
    (let [marking (part-of :marking)
          samples (for [cx (range 0 6), cz (range 0 6)
                        :let [field (w/road-field seed cx cz)
                              a (w/chunk-furniture seed cx cz field)]
                        i (range (/ (alen* a) w/furniture-stride))
                        :let [o (* i w/furniture-stride)]
                        :when (= (double marking) (double (aget* a (+ o 4))))]
                    (second (w/surface seed field (aget* a o) (aget* a (+ o 2)))))
          total (count samples)]
      (is (pos? total) "expected some crossings")
      (is (> (/ (count (filter #(> % 0.9) samples)) (double total)) 0.9)
          (str "only " (count (filter #(> % 0.9) samples)) " of " total
               " crossing stripes were on a road")))))

(deftest poles-stand-on-the-verge-not-in-the-road
  (testing "roadness is 1.0 only *inside* the carriageway; anything less is the
            verge, which is exactly where a lamp post belongs. Measuring against
            0.9 instead flags every correctly-placed pole on a wide arterial."
    (let [pole (part-of :pole)
          samples (for [cx (range 0 6), cz (range 0 6)
                        :let [field (w/road-field seed cx cz)
                              a (w/chunk-furniture seed cx cz field)]
                        i (range (/ (alen* a) w/furniture-stride))
                        :let [o (* i w/furniture-stride)]
                        :when (= (double pole) (double (aget* a (+ o 4))))]
                    (second (w/surface seed field (aget* a o) (aget* a (+ o 2)))))
          total (count samples)
          blocking (count (filter #(>= % 0.9999) samples))]
      (is (> total 500) "expected a decent sample of poles")
      (is (< (/ blocking (double total)) 0.01)
          (str blocking " of " total " poles stood in the carriageway")))))

(deftest a-plot-belongs-to-exactly-one-chunk
  (testing "cells straddle chunk borders, so both neighbours cut the same plots
            out of them -- ownership by centre is what stops the building being
            put up twice"
    (let [[bx bz] (densest-chunk (fn [u _] (> u 0.80)))
          claimed (for [cx [bx (inc bx)], cz [bz (inc bz)]
                        {:keys [x z]} (w/chunk-lots seed cx cz)]
                    [(Math/round (* 100.0 x)) (Math/round (* 100.0 z))])
          dupes (->> claimed frequencies (filter (fn [[_ n]] (> n 1))) (map first))]
      (is (> (count claimed) 100) "expected plenty of plots across four chunks")
      (is (empty? (take 3 dupes))
          (str (count dupes) " plots were claimed twice, e.g. " (first dupes))))))

;; --- building masses --------------------------------------------------------

(deftest building-parts-are-well-formed
  (let [[cx cz] (densest-chunk (fn [u _] (> u 0.80)))
        {:keys [buildings parts]} (w/chunk-structures seed cx cz (w/road-field seed cx cz))
        nb (/ (alen* buildings) w/building-stride)
        np (/ (alen* parts) w/building-part-stride)]
    (is (pos? nb))
    (is (> (/ np (double nb)) 3.0)
        (str "only " (/ np (double nb)) " volumes per building -- these are
              still boxes"))
    (testing "every part names a real shape and a real material"
      (is (every? (fn [i]
                    (let [o (* i w/building-part-stride)
                          prim (aget* parts (+ o 7))
                          mat  (aget* parts (+ o 8))
                          tint (aget* parts (+ o 9))]
                      (and (<= 0 prim) (< prim (count w/building-prims))
                           (or (= mat w/plain-mat)
                               (and (<= 0 mat) (< mat (count w/building-zones))))
                           (<= 0 tint 0xffffff)
                           ;; A float32 holds integers exactly only up to 2^24;
                           ;; a packed 24-bit colour is the largest that fits.
                           (= tint (Math/floor tint)))))
                  (range np))))
    (testing "no part is inside out or of zero size"
      (is (every? (fn [i]
                    (let [o (* i w/building-part-stride)]
                      (and (> (aget* parts (+ o 4)) 0.0)
                           (> (aget* parts (+ o 5)) 0.0)
                           (> (aget* parts (+ o 6)) 0.0))))
                  (range np))))))

(deftest a-shop-awning-hangs-over-the-street
  (testing "the local-to-world transform. A part placed toward local -Z has to
            come out on the street side of the building; rotating the wrong way
            puts every awning, porch and loading bay in the back yard, which is
            invisible in a screenshot of a city block."
    (let [awning 0xb2452f
          samples (for [cx (range 0 8), cz (range 0 8)
                        :let [field (w/road-field seed cx cz)
                              {:keys [buildings parts]} (w/chunk-structures seed cx cz field)]
                        i (range (/ (alen* parts) w/building-part-stride))
                        :let [o (* i w/building-part-stride)]
                        :when (= (double awning) (double (aget* parts (+ o 9))))
                        :let [px (aget* parts o) pz (aget* parts (+ o 2))
                              ;; Nearest building centre, which the awning belongs to.
                              nb (apply min-key
                                        (fn [j]
                                          (let [b (* j w/building-stride)]
                                            (+ (Math/pow (- (aget* buildings b) px) 2)
                                               (Math/pow (- (aget* buildings (+ b 2)) pz) 2))))
                                        (range (/ (alen* buildings) w/building-stride)))
                              b (* nb w/building-stride)]]
                    [(second (w/surface seed field px pz))
                     (second (w/surface seed field (aget* buildings b)
                                        (aget* buildings (+ b 2))))])
          total (count samples)
          nearer (count (filter (fn [[a b]] (>= a b)) samples))]
      (is (> total 30) "expected a decent number of shops")
      (is (> (/ nearer (double total)) 0.95)
          (str "only " nearer " of " total " awnings were on the street side")))))

(deftest silhouettes-differ-by-zone
  (testing "a house is not an office with a different texture"
    (let [prims-of (fn [pred]
                     (let [[cx cz] (densest-chunk pred)
                           {:keys [parts]} (w/chunk-structures seed cx cz
                                                               (w/road-field seed cx cz))]
                       (frequencies
                        (for [i (range (/ (alen* parts) w/building-part-stride))]
                          (nth w/building-prims
                               (int (aget* parts (+ 7 (* i w/building-part-stride)))))))))
          downtown (prims-of (fn [u _] (> u 0.88)))
          works    (prims-of (fn [u ind] (and (> ind 0.74) (< 0.3 u 0.7))))
          rural    (prims-of (fn [u _] (< 0.16 u 0.30)))]
      (is (pos? (:cylinder downtown 0)) "office masts")
      (is (pos? (:gable works 0)) "sawtooth and shed roofs")
      (is (pos? (:gable rural 0)) "pitched roofs on houses and barns"))))

;; --- rivers and bridges -----------------------------------------------------

(defn- wet-point
  "A point in the middle of a channel. Found rather than hardcoded: any change
  to the generator moves the rivers, and a stale coordinate would quietly turn
  every bridge test below into a test of an empty field."
  []
  (first (for [gx (range -80 80), gz (range -80 80)
               :let [x (* gx 400.0) z (* gz 400.0)]
               :when (> (w/river seed x z) 0.8)]
           [x z])))

(defn- bridge-chunk []
  (let [[wx wz] (wet-point)
        [c0x c0z] (w/chunk-of wx wz)]
    (first (for [dx (range -4 5), dz (range -4 5)
                 :let [cx (+ c0x dx) cz (+ c0z dz)]
                 :when (pos? (alen* (w/chunk-bridges seed cx cz)))]
             [cx cz]))))

(deftest rivers-are-narrow-and-regional
  (let [grid (for [i (range 120), j (range 120)]
               (w/river seed (* i 240.0) (* j 240.0)))
        channel (count (filter #(> % 0.5) grid))
        any     (count (filter pos? grid))
        n       (count grid)]
    (testing "measuring the contour in noise units instead of metres put a
              quarter of the world under water; the fix is dividing by the
              field's own gradient"
      (is (< 0.001 (/ channel (double n)) 0.10)
          (str (* 100.0 (/ channel (double n))) "% of the world is channel")))
    (is (< (/ any (double n)) 0.20) "and not much more is even damp")
    (is (pos? channel) "but there are rivers somewhere")))

(deftest rivers-cut-below-the-land
  (let [[wx wz] (wet-point)]
    (is (some? [wx wz]) "expected to find a channel")
    (let [wet-h (w/base-height seed wx wz)
          ;; A ring well outside the banks, at the same terrain scale.
          dry (apply max (for [a (range 0 8)
                               :let [th (* a (/ js-two-pi 8.0))
                                     x (+ wx (* 90.0 (Math/cos th)))
                                     z (+ wz (* 90.0 (Math/sin th)))]
                               :when (zero? (w/river seed x z))]
                           (w/base-height seed x z)))]
      (is (> (- dry wet-h) 5.0)
          (str "the channel is only " (- dry wet-h) " m below the bank")))))

(deftest a-bridge-leaves-its-valley-alone
  (testing "the whole structural point of W5: a span is excluded from the road
            field, so the ground under it is not flattened up to meet the deck"
    (let [[cx cz] (bridge-chunk)]
      (is (some? [cx cz]) "expected a chunk with a bridge in it")
      (let [field (w/road-field seed cx cz)
            spans (filter :bridge? (w/chunk-lines seed cx cz))]
        (is (seq spans))
        (doseq [{:keys [points ya yb]} spans]
          (let [[ax az] (first points)
                [bx bz] (peek points)
                mx (* 0.5 (+ ax bx)) mz (* 0.5 (+ az bz))
                deck (* 0.5 (+ ya yb))
                under (first (w/surface seed field mx mz))]
            (is (> (- deck under) 2.5)
                (str "only " (- deck under) " m of clearance -- the valley got "
                     "filled in"))))))))

(deftest a-deck-ends-where-the-road-does
  (testing "the approach is flattened terrain and the span is not, and they have
            to agree at the node or the car launches off a step.

            With expressways the agreement is at the node's *grade*: a river
            crossing has no lift and touches down, an elevated span sits exactly
            its own lift above the road it rides over. Asserting the deck always
            meets the ground stopped being true the moment roads could fly."
    (let [[cx cz] (bridge-chunk)
          field (w/road-field seed cx cz)]
      (doseq [{:keys [points ya yb lift-a lift-b]}
              (filter :bridge? (w/chunk-lines seed cx cz))]
        (let [[ax az] (first points)
              [bx bz] (peek points)]
          (is (< (abs (- ya (+ lift-a (first (w/surface seed field ax az))))) 0.05)
              "near end")
          (is (< (abs (- yb (+ lift-b (first (w/surface seed field bx bz))))) 0.05)
              "far end"))))))

(deftest bridge-parts-are-well-formed
  (let [[cx cz] (bridge-chunk)
        a (w/chunk-bridges seed cx cz)
        n (/ (alen* a) w/bridge-part-stride)
        ;; Coerced: on the JVM a float-array element boxes to Float, and
        ;; (contains? #{0.0 1.0} (float 1.0)) is false. In ClojureScript every
        ;; number is a double and the same test passes, so this only shows up
        ;; on one of the two platforms the generator has to run on.
        at (fn [i o] (double (aget* a (+ o (* i w/bridge-part-stride)))))]
    (is (pos? n))
    (testing "shapes and colours are in range"
      ;; Offsets 8/9/10, not 7/8/9: the stride grew by a pitch field when decks
      ;; stopped being flat slabs, and this is the test that noticed.
      (is (every? (fn [i] (and (<= 0 (at i 8)) (< (at i 8) (count w/bridge-prims))
                               (<= 0 (at i 9) 0xffffff)
                               (contains? #{0.0 1.0} (at i 10))
                               (> (at i 5) 0.0) (> (at i 6) 0.0) (> (at i 7) 0.0)))
                  (range n))))
    (testing "the deck carries the collider and the piers do not: a pier stands
              underneath where nothing can reach it"
      (let [box (part-index-of w/bridge-prims :box)
            cyl (part-index-of w/bridge-prims :cylinder)]
        (is (every? (fn [i] (or (not= (double cyl) (at i 8)) (zero? (at i 10))))
                    (range n))
            "a pier was marked solid")
        (is (some (fn [i] (and (= (double box) (at i 8)) (pos? (at i 10))))
                  (range n))
            "nothing solid to drive on")))))

(deftest nothing-is-placed-on-a-span
  (testing "`surface` under a bridge reports the riverbed, so anything lined up
            along one would sit in the water below the road"
    (let [[cx cz] (bridge-chunk)
          field (w/road-field seed cx cz)
          spans (filter :bridge? (w/chunk-lines seed cx cz))
          near-a-span? (fn [x z]
                         (some (fn [{:keys [points]}]
                                 (let [[ax az] (first points)
                                       [bx bz] (peek points)]
                                   (< (Math/hypot (- x (* 0.5 (+ ax bx)))
                                                  (- z (* 0.5 (+ az bz))))
                                      12.0)))
                               spans))
          arrs [[(w/chunk-props seed cx cz field) w/prop-stride 0 2]
                [(w/chunk-peds seed cx cz field) w/ped-stride 0 2]]]
      (doseq [[a st xi zi] arrs]
        (doseq [i (range (/ (alen* a) st))]
          (let [o (* i st)]
            (is (not (near-a-span? (aget* a (+ o xi)) (aget* a (+ o zi))))
                "something was placed on a bridge")))))))


(defn- deck-end-tops
  "World Y of the two ends of a deck part's top face.

  Reconstructed from the part's own transform rather than from the data it was
  built out of. Checking `ya` and `yb` proves the *heights* agree; it says
  nothing about where the slab actually ended up, which is where the bug was."
  [px py pz yaw pitch sy sz]
  (for [along [-1.0 1.0]]
    (let [ly (* 0.5 sy)
          lz (* along 0.5 sz)
          ;; RotX(-pitch) then RotY(yaw); only Y is needed.
          y' (- (* ly (Math/cos pitch)) (* lz (Math/sin (- pitch))))]
      (+ py y'))))

(deftest a-deck-lies-along-its-chord
  (testing "a flat slab at the average height floats over the road at the near
            end by half the fall of the span, and the car drives under the
            leading edge into the river"
    (let [[cx cz] (bridge-chunk)
          a (w/chunk-bridges seed cx cz)
          st w/bridge-part-stride
          n (/ (alen* a) st)
          at (fn [i o] (double (aget* a (+ o (* i st)))))
          box (part-index-of w/bridge-prims :box)
          deck 0x3c3c40
          tops (for [i (range n)
                     :when (and (= (double box) (at i 8)) (= (double deck) (at i 9)))
                     t (deck-end-tops (at i 0) (at i 1) (at i 2)
                                      (at i 3) (at i 4) (at i 6) (at i 7))]
                 t)
          spans (filter :bridge? (w/chunk-lines seed cx cz))
          lo (apply min (mapcat (juxt :ya :yb) spans))
          hi (apply max (mapcat (juxt :ya :yb) spans))]
      (is (seq tops) "expected deck parts")
      (is (< (abs (- (apply min tops) lo)) 0.15)
          (str "lowest deck end " (apply min tops) " vs lowest road end " lo))
      (is (< (abs (- (apply max tops) hi)) 0.15)
          (str "highest deck end " (apply max tops) " vs highest road end " hi))
      (testing "and the deck is pitched at all, rather than a stack of steps"
        (is (some (fn [i] (> (abs (at i 4)) 0.005)) (range n))
            "no deck part had any pitch")))))

;; --- elevated expressways ---------------------------------------------------

(defn- lifted-node
  "A lattice node where an expressway rides over the surface grid. Searched for
  rather than hardcoded, for the same reason `wet-point` is."
  []
  (first (for [gx (range -600 600 8), gz (range -600 600 8)
               :when (or (zero? (mod gx 32)) (zero? (mod gz 32)))
               :let [lx (w/node-lift seed gx gz true)
                     lz (w/node-lift seed gx gz false)]
               :when (> (max lx lz) 6.0)]
           [gx gz])))

(defn- around-lifted
  "Every street in the chunks around a lifted node."
  []
  (let [[gx gz] (lifted-node)
        [x z] (w/node seed gx gz)
        [cx cz] (w/chunk-of x z)]
    {:node [gx gz] :at [x z] :chunk [cx cz]
     :streets (vec (for [dx (range -2 3), dz (range -2 3)
                         s (w/chunk-lines seed (+ cx dx) (+ cz dz))]
                     s))}))

(deftest an-expressway-leaves-the-ground
  (let [{:keys [streets]} (around-lifted)
        lifted (filter #(or (pos? (:lift-a %)) (pos? (:lift-b %))) streets)]
    (is (seq lifted) "expected an elevated expressway nearby")
    (testing "a lifted street is a bridge, so it is excluded from the terrain cut
              and the street underneath stays a street"
      (is (every? :bridge? lifted)))))

(deftest a-flyover-passes-over-the-road-below
  (testing "the thing W5 could not do: at a node the expressway rides through,
            there is still a road on the ground and the deck is well above it"
    (let [{:keys [node at]} (around-lifted)
          [gx gz] node
          [x z] at
          [cx cz] (w/chunk-of x z)
          field (w/road-field seed cx cz)
          ;; The crossing street at this node is at grade, so the ground here
          ;; is still flattened to road level by it.
          under (w/surface seed field x z)
          lift (max (w/node-lift seed gx gz true) (w/node-lift seed gx gz false))]
      (is (> (second under) 0.5)
          (str "no road on the ground under the viaduct (roadness "
               (second under) ")"))
      (is (> lift 4.0) "and the deck is a storey or more above it"))))

(deftest no-signals-hang-in-mid-air
  (testing "a node an expressway rides over is not a crossroads: the traffic
            below passes under it and must not be signalled"
    (let [[gx gz] (lifted-node)]
      (when-let [j (w/junction seed gx gz)]
        (is (every? #(zero? (:lift %)) (:arms j))
            "a junction counted an elevated arm")))
    (testing "and the arms that are counted are the ones on the ground"
      (let [{:keys [streets]} (around-lifted)]
        (is (seq (filter #(and (pos? (:lift-a %)) (pos? (:lift-b %))) streets))
            "expected a fully elevated span")))))

(deftest ramps-are-driveable
  (testing "the lift follows density rather than switching on, so the climb is
            spread over however many blocks the edge of the city takes. Ramping
            over a narrow band of `urbanness` -- which is itself already a
            steepened remap -- put the whole climb on one street at 15%."
    (let [grades (for [{:keys [points ya yb lift-a lift-b]}
                       (:streets (around-lifted))
                       :when (or (pos? lift-a) (pos? lift-b))
                       :let [[ax az] (first points)
                             [bx bz] (peek points)
                             len (Math/hypot (- bx ax) (- bz az))]]
                   (* 100.0 (/ (abs (- yb ya)) len)))
          sorted (vec (sort grades))]
      (is (seq grades))
      (is (< (nth sorted (quot (count sorted) 2)) 5.0)
          (str "median grade " (nth sorted (quot (count sorted) 2)) "%"))
      (is (< (/ (count (filter #(> % 10.0) grades)) (double (count grades))) 0.10)
          "more than a tenth of the expressway is steeper than 1 in 10"))))

(deftest the-two-axes-ride-at-different-heights
  (testing "so that where two expressways cross you get a stack rather than two
            decks fighting over the same piece of air"
    (let [[gx gz] (lifted-node)]
      ;; Same node, both axes: whichever are non-zero must not be equal.
      (doseq [g [gx (+ gx 32)], h [gz (+ gz 32)]]
        (let [a (w/node-lift seed g h true)
              b (w/node-lift seed g h false)]
          (when (and (pos? a) (pos? b))
            (is (> (abs (- a b)) 3.0)
                "two expressways crossing at the same height")))))))
