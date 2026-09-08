(ns carmageddon.client.minimap-test
  "The map's one piece of geometry, and it was wrong in both places that used
  it: every arrowhead pointed exactly backwards."
  (:require [carmageddon.client.minimap :as minimap]
            [carmageddon.shared.worldgen :as worldgen]
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


(deftest every-landmark-has-a-blip-that-says-what-it-is
  ;; The colour table had nine entries, from when there were nine kinds. It was
  ;; never grown, so twenty-one of the thirty fell through to the default and
  ;; drew as identical white diamonds -- the map answered "there is a landmark
  ;; here" and nothing else, which is most of the way to answering nothing.
  ;;
  ;; Nothing here draws anything. It asserts the two lookups a blip does are
  ;; total over the catalogue, which is the property that quietly stopped
  ;; holding.
  (testing "every kind belongs to a family"
    (is (empty? (remove minimap/family-of worldgen/landmark-kinds))
        (str "no family: " (vec (remove minimap/family-of
                                        worldgen/landmark-kinds)))))

  (testing "every kind has a pictogram of its own"
    (let [missing (remove #(minimap/glyph-for %) worldgen/landmark-kinds)]
      (is (empty? missing) (str "no pictogram: " (vec missing)))))

  (testing "and no two kinds share one"
    ;; Two landmarks drawn identically is the same bug in a smaller size.
    (let [gs (map #(minimap/glyph-for %) worldgen/landmark-kinds)]
      (is (= (count gs) (count (distinct gs))))))

  (testing "families are named and coloured, and none is empty"
    (doseq [[fam {:keys [colour label kinds]}] minimap/landmark-families]
      (is (string? colour) (str fam " has no colour"))
      (is (seq label) (str fam " has no name"))
      (is (seq kinds) (str fam " is empty"))
      (is (every? (set worldgen/landmark-kinds) kinds)
          (str fam " lists a landmark that does not exist"))))

  (testing "and every kind is in exactly one of them"
    (let [listed (mapcat :kinds (vals minimap/landmark-families))]
      (is (= (count listed) (count (distinct listed)))
          "a landmark in two families")
      (is (= (set listed) (set worldgen/landmark-kinds))))))
