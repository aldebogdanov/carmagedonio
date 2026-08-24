(ns carmageddon.shared.interp-test
  (:require [clojure.test :refer [deftest is testing]]
            [carmageddon.shared.interp :as i]))

(defn- st [x] {:pos [x 0.0 0.0] :quat [0.0 0.0 0.0 1.0] :damage 0.0})

(defn- filled
  "Snapshots at 0, 100, 200, 300 ms with x = 0, 10, 20, 30."
  []
  (reduce (fn [b n] (i/insert b (* n 100) (st (* n 10.0))))
          (i/buffer) (range 4)))

(deftest renders-in-the-past
  (testing "at now=400 with a 100ms delay we draw the 300ms sample"
    (is (= [30.0 0.0 0.0] (:pos (i/sample-at (filled) 400 100)))))
  (testing "and halfway between two samples we get halfway between them"
    (is (= [15.0 0.0 0.0] (:pos (i/sample-at (filled) 250 100))))))

(deftest holds-rather-than-extrapolates
  (testing "past the end it stops instead of sailing on"
    (let [p (:pos (i/sample-at (filled) 5000 100))]
      (is (= [30.0 0.0 0.0] p) "held at the last known state, not extrapolated"))))

(deftest before-the-buffer-starts-it-holds-the-first
  (is (= [0.0 0.0 0.0] (:pos (i/sample-at (filled) 0 100)))))

(deftest empty-buffer-yields-nothing
  (is (nil? (i/sample-at (i/buffer) 1000 100))))

(deftest out-of-order-arrivals-are-placed-not-dropped
  (let [b (-> (i/buffer)
              (i/insert 0 (st 0.0))
              (i/insert 200 (st 20.0))
              (i/insert 100 (st 10.0)))]   ; late packet
    (is (= [0 100 200] (mapv :t b)))
    (is (= [10.0 0.0 0.0] (:pos (i/sample-at b 200 100))))))

(deftest the-buffer-does-not-grow-without-bound
  (let [b (reduce (fn [b n] (i/insert b (* n 40) (st (double n)))) (i/buffer) (range 500))]
    (is (<= (count b) 24))
    (testing "and it keeps the newest, not the oldest"
      (is (= 499.0 (first (:pos (:state (peek b)))))))))

(deftest quaternions-take-the-shorter-arc
  (testing "without the sign check a proxy spins the long way between near-identical rotations"
    (let [a {:pos [0.0 0.0 0.0] :quat [0.0 0.0 0.0 1.0] :damage 0.0}
          b {:pos [0.0 0.0 0.0] :quat [0.0 0.0 0.0 -1.0] :damage 0.0}
          buf (-> (i/buffer) (i/insert 0 a) (i/insert 100 b))
          q (:quat (i/sample-at buf 150 100))]
      ;; halfway between q and -q is q itself, not a tumble through 180 degrees
      (is (< (abs (- 1.0 (abs (nth q 3)))) 1e-6)))))

(deftest interpolated-quaternions-stay-unit-length
  (let [buf (-> (i/buffer)
                (i/insert 0   {:pos [0.0 0.0 0.0] :quat [0.0 0.0 0.0 1.0] :damage 0.0})
                (i/insert 100 {:pos [0.0 0.0 0.0] :quat [0.0 0.7071 0.0 0.7071] :damage 0.0}))]
    (doseq [t [110 120 150 180 199]]
      (let [q (:quat (i/sample-at buf t 100))
            len (Math/sqrt (reduce + (map * q q)))]
        (is (< (abs (- 1.0 len)) 1e-6) (str "not unit length at " t))))))

(deftest staleness-is-detected
  (is (i/stale? (i/buffer) 1000 500))
  (is (not (i/stale? (filled) 350 500)))
  (is (i/stale? (filled) 5000 500)))
