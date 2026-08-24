(ns carmageddon.shared.noise-test
  "Golden values generated on the JVM and asserted on both runtimes. Terrain
  height comes out of these functions, so a divergence here would put two
  clients on physically different ground."
  (:require [clojure.test :refer [deftest is testing]]
            [carmageddon.shared.noise :as n]))

(defn- r6 [v] (/ (double (long (* v 1e6))) 1e6))

(deftest value-noise-golden
  (is (= [0.172706 0.209968 0.352686 0.589441 0.262671]
         (mapv #(r6 (n/value2d 7 % 0.5)) [0.0 0.25 0.5 1.5 2.75])))
  (testing "negative coordinates are not a special case"
    (is (= 0.462271 (r6 (n/value2d 7 -3.25 -8.75))))))

(deftest fbm-golden
  (is (= [0.345362 0.491261 0.541389]
         (mapv #(r6 (n/fbm2d 7 % 1.25 4)) [0.0 1.0 2.5])))
  (is (= 0.536804 (r6 (n/ridged 7 1.5 2.5 3)))))

(deftest stays-in-unit-interval
  (let [vs (for [x (range 0 40), y (range 0 40)]
             (n/fbm2d 3 (* 0.37 x) (* 0.37 y) 5))]
    (is (every? #(and (<= 0.0 %) (< % 1.0)) vs))))

(deftest continuous-across-lattice-boundaries
  (testing "no seam where integer cells meet"
    (doseq [c [1.0 2.0 -1.0 -4.0]]
      (let [a (n/value2d 11 (- c 0.0001) 0.3)
            b (n/value2d 11 (+ c 0.0001) 0.3)]
        (is (< (abs (- a b)) 1e-3) (str "seam at x=" c))))))

(deftest lattice-points-are-order-independent
  (testing "value depends only on (seed, x, y), never on call order"
    (let [a (n/value2d 5 3.25 -2.5)]
      (dotimes [_ 20] (n/value2d 5 (rand) (rand)))
      (is (= a (n/value2d 5 3.25 -2.5))))))
