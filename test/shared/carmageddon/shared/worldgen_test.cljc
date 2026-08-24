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

(deftest portals-agree-from-both-sides
  (testing "each shared edge yields one point, computed independently"
    (doseq [cx (range -2 3), cz (range -2 3)]
      (is (= (w/portal seed cx cz :east) (w/portal seed (inc cx) cz :west))
          (str "east/west seam at " [cx cz]))
      (is (= (w/portal seed cx cz :south) (w/portal seed cx (inc cz) :north))
          (str "north/south seam at " [cx cz])))))

(deftest portals-lie-on-the-boundary
  (doseq [cx [-3 0 5], cz [-1 0 7]]
    (let [[ex _] (w/portal seed cx cz :east)
          [_ sz] (w/portal seed cx cz :south)]
      (is (= ex (* (inc cx) k/chunk-size)))
      (is (= sz (* (inc cz) k/chunk-size))))))

(deftest terrain-is-continuous-across-chunk-borders
  (testing "a point on a shared edge gets the same height from either chunk"
    (doseq [[a b] [[[0 0] [1 0]] [[0 0] [0 1]] [[-1 3] [0 3]] [[4 -2] [4 -1]]]]
      (let [[ax az] a
            [bx bz] b
            sa (w/road-segments seed ax az)
            sb (w/road-segments seed bx bz)
            vertical? (not= ax bx)
            ;; walk along the seam the two chunks share
            pts (for [t (range 0.05 1.0 0.1)]
                  (if vertical?
                    [(* (inc ax) k/chunk-size) (* (+ az t) k/chunk-size)]
                    [(* (+ ax t) k/chunk-size) (* (inc az) k/chunk-size)]))]
        (doseq [[x z] pts]
          (let [[ha _] (w/surface seed sa x z)
                [hb _] (w/surface seed sb x z)]
            (is (< (abs (- ha hb)) 1e-9)
                (str "seam step of " (abs (- ha hb)) " m between " a " and " b
                     " at " [x z]))))))))

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
          segs (w/road-segments seed cx cz)
          step (/ k/chunk-size (dec verts))]
      (doseq [i [0 7 16 32], j [0 5 21 32]]
        (let [x (+ (* cx k/chunk-size) (* i step))
              z (+ (* cz k/chunk-size) (* j step))
              expected (first (w/surface seed segs x z))
              ;; x selects the row, z varies fastest -- the layout Rapier wants
              actual   (aget* heights (+ (* i verts) j))]
          (is (< (abs (- expected actual)) 1e-3)
              (str "grid/surface mismatch at " [i j])))))))

(defn- first-chunk-of [b]
  (first (for [cx (range 0 24), cz (range 0 6)
               :when (= b (w/biome seed cx cz))]
           [cx cz])))

(deftest roads-are-flat-and-terrain-is-not
  (testing "a country hub sits on fully road-like ground"
    (let [[cx cz] (first-chunk-of :country)
          segs (w/road-segments seed cx cz)
          [hx hz] (w/hub seed cx cz)]
      (is (= 1.0 (second (w/surface seed segs hx hz))))))
  (testing "every biome's own roads are roads"
    (doseq [b [:country :city]]
      (let [[cx cz] (first-chunk-of b)
            segs (w/road-segments seed cx cz)
            line (first (w/spokes seed cx cz))
            [x z] (nth line (quot (count line) 2))]
        (is (= 1.0 (second (w/surface seed segs x z)))
            (str b " road midpoint was not flat")))))
  (testing "open ground away from any road is not road-like"
    (let [[cx cz] (first-chunk-of :country)
          segs (w/road-segments seed cx cz)
          [_ r] (w/surface seed segs (+ 4.0 (* cx k/chunk-size)) (+ 4.0 (* cz k/chunk-size)))]
      (is (< r 0.5)))))

(deftest biome-is-stable-and-mixed
  (testing "a function of chunk coordinates only"
    (is (= (w/biome seed 3 -7) (w/biome seed 3 -7))))
  (testing "both biomes actually occur"
    (let [mix (frequencies (for [cx (range -12 13), cz (range -12 13)] (w/biome seed cx cz)))]
      (is (pos? (:city mix 0)))
      (is (pos? (:country mix 0))))))

(deftest portals-still-stitch-across-a-biome-boundary
  (testing "biome must not change what neighbours agree on"
    (let [pairs (for [cx (range -8 8), cz (range -4 4)
                      :when (not= (w/biome seed cx cz) (w/biome seed (inc cx) cz))]
                  [cx cz])]
      (is (seq pairs) "expected at least one biome boundary in range")
      (doseq [[cx cz] pairs]
        (is (= (w/portal seed cx cz :east) (w/portal seed (inc cx) cz :west)))))))

(deftest buildings-only-in-cities-and-clear-of-streets
  (let [[ccx ccz] (first-chunk-of :country)]
    (is (zero? (alen* (w/chunk-buildings seed ccx ccz (w/road-segments seed ccx ccz))))
        "country chunks have no buildings"))
  (let [[cx cz] (first-chunk-of :city)
        segs (w/road-segments seed cx cz)
        b (w/chunk-buildings seed cx cz segs)
        n (/ (alen* b) w/building-stride)]
    (is (pos? n) "city chunks should have buildings")
    (testing "no building sits on a street"
      (is (every? (fn [i]
                    (let [o (* i w/building-stride)]
                      (< (second (w/surface seed segs (aget* b o) (aget* b (+ o 2)))) 0.2)))
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
  (testing "whatever biome the origin chunk turns out to be"
    (let [[x _ z] (w/spawn-point seed)
          segs (w/road-segments seed 0 0)]
      (is (= 1.0 (second (w/surface seed segs x z)))))))

(deftest props-are-deterministic-and-roadside
  (let [segs (w/road-segments seed 2 1)
        a (w/chunk-props seed 2 1 segs)
        b (w/chunk-props seed 2 1 segs)
        n (/ (alen* a) w/prop-stride)]
    (testing "same seed, same clutter"
      (is (= (vec (map #(aget* a %) (range (alen* a))))
             (vec (map #(aget* b %) (range (alen* b)))))))
    (testing "every candidate is placed -- acceptance must not depend on terrain,
              or two machines could disagree about how far the stream advanced"
      (is (= w/props-per-chunk n)))
    (testing "kinds are in range"
      (is (every? (fn [i] (let [k (aget* a (+ 4 (* i w/prop-stride)))]
                            (and (<= 0 k) (< k (count w/prop-kinds)))))
                  (range n))))
    (testing "props sit on the surface, not floating or buried"
      (is (every? (fn [i]
                    (let [o (* i w/prop-stride)
                          x (aget* a o) y (aget* a (+ o 1)) z (aget* a (+ o 2))]
                      (< (abs (- y (first (w/surface seed segs x z)))) 1e-3)))
                  (range n))))))

(deftest props-mostly-avoid-the-carriageway
  (testing "clutter lines the road rather than blocking it"
    (let [roadness (for [cx (range 0 4), cz (range 0 4)
                         :let [segs (w/road-segments seed cx cz)
                               a (w/chunk-props seed cx cz segs)]
                         i (range (/ (alen* a) w/prop-stride))]
                     (second (w/surface seed segs
                                        (aget* a (* i w/prop-stride))
                                        (aget* a (+ 2 (* i w/prop-stride))))))
          total (count roadness)
          on-road (count (filter #(> % 0.9) roadness))]
      (is (pos? total))
      (is (< (/ on-road (double total)) 0.15)
          (str on-road " of " total " props sat on the carriageway")))))

(deftest props-belong-to-exactly-one-chunk
  (testing "placed from this chunk's own spokes, so neighbours cannot duplicate them"
    (doseq [[cx cz] [[0 0] [3 -2]]]
      (let [segs (w/road-segments seed cx cz)
            a (w/chunk-props seed cx cz segs)
            lo-x (* cx k/chunk-size) lo-z (* cz k/chunk-size)]
        (doseq [i (range (/ (alen* a) w/prop-stride))]
          (let [o (* i w/prop-stride)
                x (aget* a o) z (aget* a (+ o 2))]
            ;; a prop may spill slightly past its own edge near a portal, but
            ;; never as far as the neighbour's interior
            (is (and (< (- lo-x 60) x (+ lo-x k/chunk-size 60))
                     (< (- lo-z 60) z (+ lo-z k/chunk-size 60)))
                (str "prop " i " of chunk " [cx cz] " strayed to " [x z]))))))))
