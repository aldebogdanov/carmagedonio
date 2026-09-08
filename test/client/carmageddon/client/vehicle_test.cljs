(ns carmageddon.client.vehicle-test
  "Sign and handedness, which is the one kind of question this project has
  learned not to reason about in the abstract."
  (:require [cljs.test :refer-macros [deftest is testing]]
            [carmageddon.client.vehicle :as vehicle]))

(defn- rotate
  "Apply a quaternion to a vector, the long way round. Written out here rather
  than borrowed from the vehicle so the test cannot agree with the code by
  sharing its mistake."
  [q [x y z]]
  (let [qx (.-x q) qy (.-y q) qz (.-z q) qw (.-w q)
        ;; t = 2 * (q_vec x v)
        tx (* 2.0 (- (* qy z) (* qz y)))
        ty (* 2.0 (- (* qz x) (* qx z)))
        tz (* 2.0 (- (* qx y) (* qy x)))]
    [(+ x (* qw tx) (- (* qy tz) (* qz ty)))
     (+ y (* qw ty) (- (* qz tx) (* qx tz)))
     (+ z (* qw tz) (- (* qx ty) (* qy tx)))]))

(deftest righting-a-car-leaves-it-facing-the-same-way
  ;; The whole point of righting a car in place is that you carry on from where
  ;; you were. Put it down facing backwards and it is a worse outcome than the
  ;; roll, because now you are pointing at the wall you just hit.
  (testing "a chassis rotated by the result points along the heading it was given"
    (doseq [th (range 0.0 6.28 0.37)]
      ;; The chassis convention: local forward is -Z, so a car yawed by th
      ;; points along (-sin th, 0, -cos th).
      (let [fx (- (js/Math.sin th))
            fz (- (js/Math.cos th))
            q  (vehicle/upright-quaternion fx fz)
            [ax ay az] (rotate q [0.0 0.0 -1.0])]
        (is (< (js/Math.abs (- ax fx)) 1e-9) (str "x at yaw " th))
        (is (< (js/Math.abs ay) 1e-9) (str "the heading tipped at yaw " th))
        (is (< (js/Math.abs (- az fz)) 1e-9) (str "z at yaw " th)))))

  (testing "and it is level however far over the car had rolled"
    ;; The input comes off a car lying on its roof, so its heading is not
    ;; horizontal and its length is not one. Only the yaw of it may survive.
    (doseq [[fx fz] [[0.3 0.05] [-2.0 0.0] [0.0 -0.4] [1e-4 1e-4]]]
      (let [q (vehicle/upright-quaternion fx fz)
            [_ uy _] (rotate q [0.0 1.0 0.0])]
        (is (< (js/Math.abs (- uy 1.0)) 1e-9)
            (str "not level for heading " [fx fz])))))

  (testing "a car with no heading left at all still gets a valid rotation"
    ;; atan2(0, 0) is 0 rather than NaN, and a car standing exactly on its nose
    ;; has to be put down facing *somewhere*.
    (let [q (vehicle/upright-quaternion 0.0 0.0)]
      (is (every? #(== % %) [(.-x q) (.-y q) (.-z q) (.-w q)])))))
