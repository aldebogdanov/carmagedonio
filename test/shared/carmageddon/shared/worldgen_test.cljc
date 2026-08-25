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

(deftest buildings-only-in-cities-and-clear-of-streets
  (let [[ccx ccz] (first-chunk-of :country)]
    (is (zero? (alen* (w/chunk-buildings seed ccx ccz (w/road-field seed ccx ccz))))
        "country chunks have no buildings"))
  (let [[cx cz] (first-chunk-of :city)
        field (w/road-field seed cx cz)
        b (w/chunk-buildings seed cx cz field)
        n (/ (alen* b) w/building-stride)]
    (is (pos? n) "city chunks should have buildings")
    (testing "no building sits on a street"
      (is (every? (fn [i]
                    (let [o (* i w/building-stride)]
                      (< (second (w/surface seed field (aget* b o) (aget* b (+ o 2)))) 0.2)))
                  (range n))))
    (testing "footprints and heights are sane"
      (is (every? (fn [i]
                    (let [o (* i w/building-stride)]
                      (and (< 3.0 (aget* b (+ o 3)) 11.0)
                           (< 3.0 (aget* b (+ o 4)) 11.0)
                           (< 8.0 (aget* b (+ o 5)) 43.0))))
                  (range n))))))

(deftest heights-stay-within-the-terrain-envelope
  (let [{:keys [heights]} (w/chunk-data seed -5 9)
        vs (map #(aget* heights %) (range (alen* heights)))]
    (is (every? #(< (abs %) (* 1.0 w/terrain-amp)) vs))
    (is (> (- (apply max vs) (apply min vs)) 0.5) "chunk should not be perfectly flat")))

(deftest spawn-is-on-a-road
  (let [[x _ z] (w/spawn-point seed)
        [cx cz] (w/chunk-of x z)
        field (w/road-field seed cx cz)]
    (is (= 1.0 (second (w/surface seed field x z))))))

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
