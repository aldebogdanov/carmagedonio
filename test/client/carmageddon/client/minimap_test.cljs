(ns carmageddon.client.minimap-test
  "The map's one piece of geometry, and it was wrong in both places that used
  it: every arrowhead pointed exactly backwards."
  (:require [carmageddon.client.minimap :as minimap]
            [clojure.test :refer [deftest is testing]]))

(defn- tip
  "Where the tip of an arrow drawn at (0, -1) ends up after `ctx.rotate`.

  Canvas rotation takes (x, y) to (x cos a - y sin a, x sin a + y cos a), so a
  tip at (0, -1) lands on (sin a, -cos a). Screen y points down."
  [a]
  [(js/Math.sin a) (- (js/Math.cos a))])

(defn- close? [a b] (< (js/Math.abs (- a b)) 1e-9))

(deftest an-arrow-points-where-the-car-points
  (testing "north is -Z and the map is drawn north-up, so world +X is screen
            right and world +Z is screen *down* -- the canvas y axis runs the
            opposite way from the world z axis"
    (doseq [[label fx fz] [["north (-Z)"  0.0 -1.0]
                           ["east  (+X)"  1.0  0.0]
                           ["south (+Z)"  0.0  1.0]
                           ["west  (-X)" -1.0  0.0]]]
      (let [[tx ty] (tip (minimap/heading-of fx fz))]
        (is (close? tx fx) (str label ": screen x"))
        (is (close? ty fz) (str label ": screen y"))))))

(deftest the-bearing-grows-clockwise-from-north
  (testing "the same convention the compass label uses, which was the only
            place in this namespace that already had it right"
    (is (close? 0.0 (minimap/heading-of 0.0 -1.0)))
    (is (close? (/ js/Math.PI 2) (minimap/heading-of 1.0 0.0)))
    (is (close? js/Math.PI (js/Math.abs (minimap/heading-of 0.0 1.0))))))
