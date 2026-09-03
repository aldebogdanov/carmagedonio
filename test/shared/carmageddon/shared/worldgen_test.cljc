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
          ;; The layout is spelled out here on purpose -- this is meant to be an
          ;; independent implementation, so a change to the segment array has to
          ;; be made deliberately in both places. Stride 9:
          ;; x1 z1 y1 x2 z2 y2 half shoulder paved.
          n     (long (/ (dlen* segs) 9))
          ;; An independent, deliberately naive implementation of the same
          ;; question the uniform grid answers. If the grid ever drops a bucket
          ;; the two diverge, and the symptom in the game would be a strip of
          ;; road that quietly stops flattening the ground.
          brute (fn [x z]
                  (loop [i 0, best 0.0, wsum 0.0, wy 0.0]
                    (if (>= i n)
                      (if (pos? wsum) [best (/ wy wsum)] [0.0 0.0])
                      (let [o (* i 9)
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

(deftest a-road-can-be-found-near-anywhere
  (testing "wherever you are, there is a carriageway within a few chunks"
    (doseq [[x z] [[0.0 0.0] [1500.0 -900.0] [-4200.0 3100.0] [640.0 640.0]]]
      (let [{:keys [pos dir]} (w/road-point-near seed x z)
            [px _ pz] pos
            [cx cz] (w/chunk-of px pz)]
        (is (some? pos) (str "no road near " x "," z))
        (is (= 1.0 (second (w/surface seed (w/road-field seed cx cz) px pz)))
            "the point returned is on the carriageway")
        (is (< (abs (- 1.0 (Math/sqrt (+ (* (first dir) (first dir))
                                         (* (second dir) (second dir))))))
               1e-9))
        ;; Rivals respawn here and queue up behind, so the road has to keep
        ;; going. This is the property that picking street *middles* buys.
        (doseq [d [8.0 16.0]]
          (let [ox (- px (* (first dir) d)) oz (- pz (* (second dir) d))
                [ocx ocz] (w/chunk-of ox oz)]
            (is (= 1.0 (second (w/surface seed (w/road-field seed ocx ocz) ox oz)))
                (str d " m back was not on the carriageway")))))))

  (testing "and it is a road near *there*, not near the origin"
    (let [{:keys [pos]} (w/road-point-near seed 3000.0 -2000.0)
          [px _ pz] pos]
      (is (< (Math/sqrt (+ (* (- px 3000.0) (- px 3000.0))
                           (* (- pz -2000.0) (- pz -2000.0))))
             (* 4 w/street-spacing))
          "should land within a couple of blocks of where it was asked"))))

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

(deftest every-district-gets-a-landmark
  (testing "a hundred districts, and every one of them has something in it"
    (let [lms (for [dx (range -5 5), dz (range -5 5)] (w/landmark seed dx dz))]
      (is (every? some? lms) "a district with nothing to navigate by")
      (is (every? #(contains? (set w/landmark-kinds) (:kind %)) lms))
      (testing "and they are not all the same thing"
        ;; The first version put standing stones in half of them, because half
        ;; the world is open country and open country had one answer.
        (let [freq (frequencies (map :kind lms))]
          (is (>= (count freq) 5) (str "only " (count freq) " kinds: " freq))
          (is (< (apply max (vals freq)) (* 0.45 (count lms)))
              (str "one kind dominates: " freq))))))

  (testing "each is inside its own district and nowhere near the water"
    (doseq [dx (range -3 3), dz (range -3 3)
            :let [{:keys [x z cell]} (w/landmark seed dx dz)
                  [cx cz] (w/chunk-of x z)
                  [ddx ddz] (w/district-of cx cz)]]
      (is (= [dx dz] [ddx ddz])
          (str "district " [dx dz] " put its landmark in " [ddx ddz]))
      (is (< (w/river seed x z) 0.5) "a landmark in the river")
      ;; The cell it claims is the cell it stands in.
      (is (= cell [(long (Math/floor (/ x w/street-spacing)))
                   (long (Math/floor (/ z w/street-spacing)))])))))

(deftest a-landmark-clears-its-block
  (testing "none of the cell's own lots survive into the chunk"
    ;; Compared against `cell-lots` for the claimed cell rather than against a
    ;; nominal 64 m square: nodes are displaced by up to 13 m, so a cell's real
    ;; rectangle is not its grid square and a neighbour's lot can legitimately
    ;; have its centre inside that square. What must not survive is a lot the
    ;; *landmark's own cell* produced -- otherwise the stadium is built through
    ;; a terrace, which is the whole thing this arrangement exists to avoid.
    (doseq [dx (range -3 3), dz (range -3 3)
            :let [{:keys [x z cell]} (w/landmark seed dx dz)
                  [cx cz] (w/chunk-of x z)
                  [gx gz] cell
                  own  (set (map (juxt :x :z) (w/cell-lots seed gx gz)))
                  ;; Lots are owned by the chunk their centre lands in and a
                  ;; cell can straddle two, so every neighbouring chunk is asked.
                  kept (for [c (range (dec cx) (+ 2 cx)), z' (range (dec cz) (+ 2 cz))
                             lot (w/chunk-lots seed c z')
                             :when (contains? own [(:x lot) (:z lot)])]
                         lot)]]
      (is (empty? kept)
          (str "district " [dx dz] " built " (count kept)
               " lots inside its landmark cell " cell)))))

(deftest landmark-parts-are-well-formed
  (let [built (for [dx (range -4 4), dz (range -4 4)
                    :let [{:keys [kind x z]} (w/landmark seed dx dz)
                          [cx cz] (w/chunk-of x z)]]
                [kind (w/chunk-landmarks seed cx cz)])
        at (fn [a i o] (double (aget* a (+ o (* i w/part-stride)))))]
    (is (seq built))
    (testing "the owning chunk builds it and nobody else does"
      (doseq [[kind a] built]
        (is (pos? (alen* a)) (str kind " generated nothing"))
        (is (zero? (mod (alen* a) w/part-stride)) "ragged parts array")))
    (testing "shapes are positive and the flags are in range"
      (doseq [[_ a] built
              :let [n (/ (alen* a) w/part-stride)]]
        (is (every? (fn [i] (and (< (at a i 8) (count w/part-prims))
                                 (<= 0 (at a i 9) 0xffffff)
                                 (contains? #{0.0 1.0 2.0} (at a i 10))
                                 (> (at a i 5) 0.0) (> (at a i 6) 0.0)
                                 (> (at a i 7) 0.0)))
                    (range n)))))
    (testing "and something in every one of them is solid enough to crash into"
      (doseq [[kind a] built
              :let [n (/ (alen* a) w/part-stride)]]
        (is (some (fn [i] (and (pos? (at a i 10)) (> (at a i 6) 3.0))) (range n))
            (str kind " is scenery you can drive through"))))))

(deftest bridge-parts-are-well-formed
  (let [[cx cz] (bridge-chunk)
        a (w/chunk-bridges seed cx cz)
        n (/ (alen* a) w/part-stride)
        ;; Coerced: on the JVM a float-array element boxes to Float, and
        ;; (contains? #{0.0 1.0} (float 1.0)) is false. In ClojureScript every
        ;; number is a double and the same test passes, so this only shows up
        ;; on one of the two platforms the generator has to run on.
        at (fn [i o] (double (aget* a (+ o (* i w/part-stride)))))]
    (is (pos? n))
    (testing "shapes and colours are in range"
      ;; Offsets 8/9/10, not 7/8/9: the stride grew by a pitch field when decks
      ;; stopped being flat slabs, and this is the test that noticed.
      (is (every? (fn [i] (and (<= 0 (at i 8)) (< (at i 8) (count w/part-prims))
                               (<= 0 (at i 9) 0xffffff)
                               (contains? #{0.0 1.0 2.0} (at i 10))
                               (> (at i 5) 0.0) (> (at i 6) 0.0) (> (at i 7) 0.0)))
                  (range n))))
    (testing "the parapets can be knocked out and the deck cannot"
      ;; A bridge you cannot leave the sides of is a corridor with a view, so
      ;; the sides have to be breakable -- and the deck has to not be, or the
      ;; road itself would come away under the wheels.
      (let [rails (filter #(= 2.0 (at % 10)) (range n))
            decks (filter #(= 1.0 (at % 10)) (range n))]
        (is (seq rails) "no breakable parapet panels")
        (is (seq decks) "no fixed deck")
        (is (> (count rails) (* 2 (count decks)))
            "parapets should come in panels, not one slab per deck segment")
        ;; Every panel is thin and short: that is what makes a hole in the side
        ;; rather than the whole side coming away at once.
        (is (every? (fn [i] (and (< (at i 5) 1.0) (< (at i 6) 1.5))) rails))))

    (testing "the deck carries the collider and the piers do not: a pier stands
              underneath where nothing can reach it"
      (let [box (part-index-of w/part-prims :box)
            cyl (part-index-of w/part-prims :cylinder)]
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
          st w/part-stride
          n (/ (alen* a) st)
          at (fn [i o] (double (aget* a (+ o (* i st)))))
          box (part-index-of w/part-prims :box)
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

;; --- farmland and flora -----------------------------------------------------

(deftest a-field-grows-one-thing
  (testing "the crop matches the cell the hedgerows are drawn around. Splitting
            cells into sub-parcels gave more variety and read as noise, because
            the colour changed in the middle of a hedged field."
    (doseq [gx [-9 0 4 21], gz [-3 0 8 17]]
      (let [x0 (* gx w/street-spacing) z0 (* gz w/street-spacing)
            inside (set (for [fx [0.1 0.4 0.6 0.9], fz [0.1 0.4 0.6 0.9]]
                          (w/crop-at seed (+ x0 (* fx w/street-spacing))
                                     (+ z0 (* fz w/street-spacing)))))]
        (is (= 1 (count inside))
            (str "cell " [gx gz] " grew " inside)))))
  (testing "and neighbouring fields are not all the same"
    (let [row (map #(w/crop-at seed (* % w/street-spacing) 0.0) (range 40))]
      (is (> (count (set row)) 3)))))

(deftest every-crop-is-planted-somewhere
  (let [mix (frequencies (for [i (range 45), j (range 45)]
                           (w/crop-at seed (* i 97.0) (* j 97.0))))]
    (is (= (set w/crop-names) (set (keys mix)))
        (str "missing from the world: "
             (remove mix w/crop-names)))
    (testing "and none of them takes over"
      (is (< (/ (apply max (vals mix)) (double (reduce + (vals mix)))) 0.35)))))

(defn- flora-of [cx cz]
  (w/chunk-flora seed cx cz (w/road-field seed cx cz)))

(deftest flora-parts-are-well-formed
  (let [[cx cz] (densest-chunk (fn [u _] (< u 0.08)))
        a (flora-of cx cz)
        n (/ (alen* a) w/part-stride)
        at (fn [i o] (double (aget* a (+ o (* i w/part-stride)))))]
    (is (> n 50) "open country should be growing something")
    (testing "shapes, colours and scales are in range"
      (is (every? (fn [i] (and (<= 0 (at i 8)) (< (at i 8) (count w/part-prims))
                               (<= 0 (at i 9) 0xffffff)
                               (contains? #{0.0 1.0} (at i 10))
                               (> (at i 5) 0.0) (> (at i 6) 0.0) (> (at i 7) 0.0)))
                  (range n))))
    (testing "a trunk stops a car and its branches do not"
      (let [cyl (part-index-of w/part-prims :cylinder)]
        (is (every? (fn [i] (= (= (double cyl) (at i 8)) (pos? (at i 10))))
                    (range n))
            "something other than a trunk was solid, or a trunk was not")))))

(deftest nothing-grows-on-the-road
  (let [[cx cz] (densest-chunk (fn [u _] (< u 0.10)))
        field (w/road-field seed cx cz)
        a (flora-of cx cz)
        n (/ (alen* a) w/part-stride)
        on-road (count (for [i (range n)
                             :let [o (* i w/part-stride)]
                             :when (>= (second (w/surface seed field (aget* a o)
                                                          (aget* a (+ o 2))))
                                       0.9999)]
                         1))]
    (is (pos? n))
    (is (zero? on-road) (str on-road " of " n " plants were in the carriageway"))))

(deftest a-tree-belongs-to-exactly-one-chunk
  (testing "candidate positions come from a global grid, so the grid gets walked
            from both sides of every border"
    (let [[bx bz] (densest-chunk (fn [u _] (< u 0.10)))
          cyl (part-index-of w/part-prims :cylinder)
          trunks (for [cx [bx (inc bx)], cz [bz (inc bz)]
                       :let [a (flora-of cx cz)]
                       i (range (/ (alen* a) w/part-stride))
                       :let [o (* i w/part-stride)]
                       :when (= (double cyl) (double (aget* a (+ o 8))))]
                   [(Math/round (* 100.0 (aget* a o)))
                    (Math/round (* 100.0 (aget* a (+ o 2))))])
          dupes (->> trunks frequencies (filter (fn [[_ c]] (> c 1))) (map first))]
      (is (> (count trunks) 50))
      (is (empty? (take 3 dupes))
          (str (count dupes) " trees were planted twice, e.g. " (first dupes))))))

(deftest the-city-is-not-a-forest
  (let [[cx cz] (densest-chunk (fn [u _] (> u 0.85)))]
    (is (zero? (alen* (flora-of cx cz)))
        "trees and hedgerows in the middle of downtown")))

(deftest woodland-is-denser-than-a-wheat-field
  (testing "tree cover follows the parcel rather than a density field, which is
            what keeps a tree line on a field boundary the way a real one is"
    (let [cyl (part-index-of w/part-prims :cylinder)
          count-in (fn [crop]
                     (let [pts (for [cx (range -34 -20), cz (range -24 -10)
                                     :let [a (flora-of cx cz)]
                                     i (range (/ (alen* a) w/part-stride))
                                     :let [o (* i w/part-stride)]
                                     :when (= (double cyl) (double (aget* a (+ o 8))))
                                     :when (= crop (w/crop-at seed (aget* a o)
                                                              (aget* a (+ o 2))))]
                                 1)]
                       (count pts)))
          wood (count-in :woodland)
          wheat (count-in :wheat)]
      (is (pos? wood) "expected woods")
      (is (> wood (* 4 wheat))
          (str "woodland " wood " trees vs wheat " wheat)))))

;; --- traffic, people and animals --------------------------------------------

(deftest a-street-can-be-driven-in-either-direction
  (testing "`street` is defined from the lower node outward, so a driver going
            the other way needs its polyline and chord reversed"
    (doseq [[a b] [[[0 0] [1 0]] [[3 -2] [3 -1]] [[-4 5] [-5 5]]]]
      (let [f (w/street-between seed a b)
            r (w/street-between seed b a)]
        (is (= (first (:points f)) (peek (:points r))) "ends swap")
        (is (= (peek (:points f)) (first (:points r))))
        (is (< (abs (- (:ya f) (:yb r))) 1e-9) "and so do the chord heights")
        (is (< (abs (- (:yb f) (:ya r))) 1e-9))))))

(deftest a-street-knows-where-it-is-in-the-lattice
  (testing "recovering this from the polyline does not work -- a node is
            displaced by up to 13 m, so an endpoint can sit in the next cell"
    (doseq [cx [-3 0 7], cz [-2 0 5]]
      (doseq [{:keys [gx gz along-x? points]} (w/chunk-lines seed cx cz)]
        (let [a (w/node seed gx gz)
              b (w/node seed (if along-x? (inc gx) gx) (if along-x? gz (inc gz)))]
          (is (= a (first points)))
          (is (= b (peek points))))))))

(deftest traffic-starts-on-a-real-street
  (let [[cx cz] (densest-chunk (fn [u _] (> u 0.80)))
        a (w/chunk-traffic seed cx cz)
        n (/ (alen* a) w/traffic-stride)]
    (is (> n 10) "a city chunk should have traffic on it")
    (testing "every car's two nodes are lattice neighbours with a street between"
      (is (every? (fn [i]
                    (let [o (* i w/traffic-stride)
                          f [(int (aget* a o)) (int (aget* a (+ o 1)))]
                          t [(int (aget* a (+ o 2))) (int (aget* a (+ o 3)))]
                          d (+ (abs (- (nth t 0) (nth f 0)))
                               (abs (- (nth t 1) (nth f 1))))]
                      (and (= 1 d)
                           (some #(= t (:to %)) (w/node-arms seed (nth f 0) (nth f 1))))))
                  (range n))))
    (testing "and starts somewhere along it at a sane speed"
      (is (every? (fn [i]
                    (let [o (* i w/traffic-stride)]
                      (and (<= 0.0 (aget* a (+ o 4)) 1.0)
                           (< 5.0 (aget* a (+ o 5)) 20.0))))
                  (range n))))))

(deftest traffic-density-follows-the-city
  (let [count-at (fn [pred]
                   (let [cs (take 4 (for [cx (range -30 40), cz (range -20 20)
                                          :let [x (* (+ cx 0.5) k/chunk-size)
                                                z (* (+ cz 0.5) k/chunk-size)]
                                          :when (pred (w/urbanness seed x z))]
                                      [cx cz]))]
                     (/ (reduce + (map (fn [[cx cz]]
                                         (/ (alen* (w/chunk-traffic seed cx cz))
                                            w/traffic-stride))
                                       cs))
                        (double (count cs)))))
        city (count-at #(> % 0.85))
        country (count-at #(< % 0.10))]
    (is (> city (* 4.0 country))
        (str "city " city " cars/chunk vs country " country))))

(deftest node-arms-name-their-neighbours
  (doseq [gx [-5 0 3], gz [-2 0 6]]
    (doseq [{:keys [to dir]} (w/node-arms seed gx gz)]
      (is (= 1 (+ (abs (- (nth to 0) gx)) (abs (- (nth to 1) gz))))
          "an arm goes to an orthogonal neighbour")
      (testing "and its direction points that way"
        (let [[nx nz] (w/node seed gx gz)
              [ox oz] (w/node seed (nth to 0) (nth to 1))
              [dx dz] dir]
          (is (pos? (+ (* dx (- ox nx)) (* dz (- oz nz))))
              "the unit direction faces the far node"))))))

(deftest people-walk-along-the-street-they-stand-on
  (testing "a crowd on arbitrary bearings reads as a scattering; walking the
            pavement is most of what makes it a crowd"
    (let [[cx cz] (densest-chunk (fn [u _] (> u 0.80)))
          field (w/road-field seed cx cz)
          a (w/chunk-peds seed cx cz field)
          n (/ (alen* a) w/ped-stride)
          lines (remove :bridge? (w/chunk-lines seed cx cz))
          ;; For each person, the direction of the nearest street.
          aligned (for [i (range n)
                        :let [o (* i w/ped-stride)]
                        :when (zero? (int (aget* a (+ o 5))))
                        :let [x (aget* a o) z (aget* a (+ o 2))
                              h (aget* a (+ o 3))
                              best (apply min-key
                                          (fn [{:keys [points]}]
                                            (let [[ax az] (first points)
                                                  [bx bz] (peek points)]
                                              (Math/hypot (- x (* 0.5 (+ ax bx)))
                                                          (- z (* 0.5 (+ az bz))))))
                                          lines)
                              [ax az] (first (:points best))
                              [bx bz] (peek (:points best))
                              len (Math/hypot (- bx ax) (- bz az))]]
                    ;; |cos| of the angle between the walk and the street.
                    (abs (+ (* (Math/cos h) (/ (- bx ax) len))
                            (* (Math/sin h) (/ (- bz az) len)))))]
      (is (seq aligned))
      (is (> (/ (count (filter #(> % 0.9) aligned)) (double (count aligned))) 0.85)
          "most people should be walking roughly along a street"))))

(deftest animals-belong-to-the-land-they-stand-on
  (let [kind-of (fn [i a] (nth w/ped-kinds (int (aget* a (+ 5 (* i w/ped-stride))))))
        gather (fn [pred]
                 (let [cs (take 6 (for [cx (range -34 -18), cz (range -24 -8)
                                        :let [x (* (+ cx 0.5) k/chunk-size)
                                              z (* (+ cz 0.5) k/chunk-size)]
                                        :when (pred (w/urbanness seed x z))]
                                    [cx cz]))]
                   (frequencies
                    (for [[cx cz] cs
                          :let [a (w/chunk-peds seed cx cz (w/road-field seed cx cz))]
                          i (range (/ (alen* a) w/ped-stride))]
                      (kind-of i a)))))
        farm (gather #(< % 0.10))
        city (gather #(> % 0.85))]
    (testing "livestock in the fields"
      (is (some farm [:sheep :cow]) (str "nothing grazing: " farm)))
    (testing "and none downtown"
      (is (nil? (:sheep city)))
      (is (nil? (:cow city)))
      (is (pos? (:person city 0))))))

(deftest a-herd-is-a-herd
  (testing "one sheep in a field is a mistake and six is a flock"
    (let [[cx cz] (densest-chunk (fn [u _] (< u 0.08)))
          a (w/chunk-peds seed cx cz (w/road-field seed cx cz))
          n (/ (alen* a) w/ped-stride)
          beasts (for [i (range n)
                       :let [o (* i w/ped-stride)]
                       :when (pos? (int (aget* a (+ o 5))))]
                   [(aget* a o) (aget* a (+ o 2))])]
      (when (seq beasts)
        ;; Every animal should have another within a herd's width of it.
        (is (every? (fn [[x z]]
                      (some (fn [[ox oz]]
                              (and (not= [x z] [ox oz])
                                   (< (Math/hypot (- x ox) (- z oz)) 24.0)))
                            beasts))
                    beasts)
            "an animal was standing on its own")))))

(deftest ped-kinds-and-traffic-are-deterministic
  (let [[cx cz] (densest-chunk (fn [u _] (< 0.2 u 0.5)))
        field (w/road-field seed cx cz)
        same? (fn [f] (let [a (f) b (f)]
                        (= (vec (map #(aget* a %) (range (alen* a))))
                           (vec (map #(aget* b %) (range (alen* b)))))))]
    (is (same? #(w/chunk-peds seed cx cz field)))
    (is (same? #(w/chunk-traffic seed cx cz)))
    (testing "every kind index names a real kind"
      (let [a (w/chunk-peds seed cx cz field)]
        (is (every? (fn [i]
                      (let [kk (aget* a (+ 5 (* i w/ped-stride)))]
                        (and (<= 0 kk) (< kk (count w/ped-kinds)))))
                    (range (/ (alen* a) w/ped-stride))))))))

;; --- standing on the ground the car drives on -------------------------------

(defn- sample-heightfield
  "Bilinear lookup into a chunk's heights, independently of the generator's own."
  [{:keys [heights verts origin size]} x z]
  (let [n verts
        step (/ size (dec n))
        [x0 z0] origin
        fx (/ (- x x0) step)
        fz (/ (- z z0) step)]
    (when (and (<= 0.0 fx (dec n)) (<= 0.0 fz (dec n)))
      (let [i0 (int (Math/floor fx)) j0 (int (Math/floor fz))
            i1 (min (dec n) (inc i0)) j1 (min (dec n) (inc j0))
            tx (- fx i0) tz (- fz j0)
            h (fn [i j] (aget* heights (+ (* i n) j)))
            a (+ (h i0 j0) (* (- (h i1 j0) (h i0 j0)) tx))
            b (+ (h i0 j1) (* (- (h i1 j1) (h i0 j1)) tx))]
        (+ a (* (- b a) tz))))))

(deftest things-stand-on-the-heightfield-not-beside-it
  (testing "the collider is built from the heightfield and the mesh is drawn
            from it, so an object placed on the analytic surface instead can sit
            up to 0.14 m from the ground the car can actually reach. Generating
            the ground first and standing everything on it is both cheaper and
            the correction."
    (let [[cx cz] (densest-chunk (fn [u _] (> u 0.80)))
          d (w/chunk-data seed cx cz)
          check (fn [label arr stride xi yi zi]
                  (let [errs (for [i (range (/ (alen* arr) stride))
                                   :let [o (* i stride)
                                         x (aget* arr (+ o xi))
                                         z (aget* arr (+ o zi))
                                         want (sample-heightfield d x z)]
                                   :when want]
                               (abs (- (aget* arr (+ o yi)) want)))]
                    (when (seq errs)
                      (is (< (apply max errs) 0.02)
                          (str label " sat " (apply max errs)
                               " m off the heightfield")))
                    (count errs)))]
      (is (pos? (check "props" (:props d) w/prop-stride 0 1 2)))
      (is (pos? (check "peds" (:peds d) w/ped-stride 0 1 2))))))

(deftest the-analytic-surface-and-the-heightfield-still-agree-closely
  (testing "they are not the same function -- a 33-vertex grid cannot follow a
            kerb -- but they must not have drifted apart"
    (let [[cx cz] (densest-chunk (fn [u _] (> u 0.80)))
          d (w/chunk-data seed cx cz)
          field (w/road-field seed cx cz)
          errs (for [i (range 2 31), j (range 2 31)
                     :let [x (+ (* cx k/chunk-size) (* i 8.0))
                           z (+ (* cz k/chunk-size) (* j 8.0))
                           grid (sample-heightfield d x z)]
                     :when grid]
                 (abs (- grid (first (w/surface seed field x z)))))]
      (is (seq errs))
      (is (< (/ (reduce + errs) (count errs)) 0.15) "mean")
      (is (< (apply max errs) 1.2) "worst case, at a kerb"))))

;; --- roads that carry on ----------------------------------------------------

(defn- edge-present
  "Which lattice edges exist in a box, as a set of [gx gz along-x?]."
  [x1 z1]
  (set (map (juxt :gx :gz :along-x?) (w/streets-in-bounds seed 0.0 0.0 x1 z1))))

(deftest streets-run-whole-stretches-rather-than-stubs
  (testing "a street either crosses the whole stretch between two junctions of
            a higher class or is not there at all -- a per-edge coin leaves
            sixty-four metres of tarmac between two fields"
    (let [present (edge-present 2400.0 2400.0)
          ;; A collector line is a multiple of four that is not a multiple of
          ;; eight; its runs are the eight edges between two arterials. A local
          ;; line is anything else, and its runs are four edges long.
          runs (for [line (range 1 33)
                     :let [collector? (and (zero? (mod line 4)) (pos? (mod line 8)))
                           span (if collector? 8 4)]
                     :when (pos? (mod line 8))
                     along-x? [true false]
                     run (range 0 (quot 32 span))
                     :let [edges (for [i (range (* run span) (* (inc run) span))]
                                   (contains? present
                                              (if along-x? [i line true] [line i false])))]]
                 [line run (count (filter true? edges)) span])]
      (is (seq runs))
      (doseq [[line run n span] runs]
        (is (or (zero? n) (= n span))
            (str "line " line " run " run " is " n " of " span " edges"))))))

(deftest country-roads-exist-at-all
  (testing "the country used to be arterials every 512 m and nothing else"
    (let [[cx cz] (densest-chunk (fn [u _] (< u 0.06)))
          lines (w/chunk-lines seed cx cz)
          classes (frequencies (map :class (w/streets-in-bounds
                                            seed
                                            (* (- cx 3) k/chunk-size)
                                            (* (- cz 3) k/chunk-size)
                                            (* (+ cx 4) k/chunk-size)
                                            (* (+ cz 4) k/chunk-size))))]
      (is (seq lines))
      (is (pos? (:arterial classes 0)))
      (is (pos? (:collector classes 0))
          (str "no through roads in open country: " classes)))))

;; --- crowds -----------------------------------------------------------------

(def ^:private social-kinds #{:suit :shopper :fan :drinker :streetwalker})

(deftest crowds-gather-rather-than-scatter
  (let [[cx cz] (densest-chunk (fn [u _] (> u 0.85)))
        a (w/chunk-peds seed cx cz (w/road-field seed cx cz))
        n (/ (alen* a) w/ped-stride)
        social (vec (for [i (range n)
                          :let [o (* i w/ped-stride)
                                kind (nth w/ped-kinds (int (aget* a (+ o 5))))]
                          :when (social-kinds kind)]
                      [kind (aget* a o) (aget* a (+ o 2))]))]
    (testing "downtown has people who are somewhere for a reason"
      (is (seq social)))
    (testing "and they are standing with each other, not near each other"
      (doseq [[kind x z] social]
        (is (some (fn [[k2 x2 z2]]
                    (and (= kind k2)
                         (not (and (== x x2) (== z z2)))
                         (< (Math/hypot (- x x2) (- z z2)) 12.0)))
                  social)
            (str "a lone " kind " at " [x z]))))))

(deftest a-landmark-decides-what-gathers-near-it
  (testing "supporters turn up at the ground, not two districts away"
    (let [lm (first (for [dx (range -7 8), dz (range -7 8)
                          :let [l (w/landmark seed dx dz)]
                          :when (= :stadium (:kind l))]
                      l))
          _ (is lm "no stadium anywhere in 15x15 districts")
          [cx cz] (w/chunk-of (:x lm) (:z lm))
          mix (frequencies
               (for [ox [-1 0 1], oz [-1 0 1]
                     :let [a (w/chunk-peds seed (+ cx ox) (+ cz oz)
                                           (w/road-field seed (+ cx ox) (+ cz oz)))]
                     i (range (/ (alen* a) w/ped-stride))]
                 (nth w/ped-kinds (int (aget* a (+ 5 (* i w/ped-stride)))))))]
      (is (pos? (:fan mix 0)) (str "nobody supporting anybody: " mix)))))
