(ns carmageddon.client.camera-test
  "Camera behaviour, tested through the public surface only.

  The interesting parts of a camera are all sign and handedness questions --
  which side of the car it ends up on, which way it swings round a seam -- and
  those are exactly the questions this project has learned not to reason about
  in the abstract. So the tests place a car, run the camera, and measure where
  it landed."
  (:require ["three" :as three]
            [carmageddon.client.camera :as camera]
            [cljs.test :refer-macros [deftest is testing]]))

(defn- rig []
  (let [^js c (three/PerspectiveCamera. 70 1 0.3 2000)]
    [c (camera/create! c)]))

(defn- yaw-quat
  "A quaternion whose `heading` is `psi`.

  Local forward is -Z, so rotating by theta about +Y points the car at
  (-sin theta, -cos theta) and `heading` reports atan2 of that, which is
  theta + pi. Inverting gives theta = psi - pi."
  [psi]
  (let [th (- psi js/Math.PI)]
    [0.0 (js/Math.sin (* 0.5 th)) 0.0 (js/Math.cos (* 0.5 th))]))

(defn- step!
  "One camera update for a car at (px,0.5,pz) with heading `psi`."
  ([cs psi] (step! cs psi 0.0 0.0 0.0 (/ 1.0 60.0) nil))
  ([cs psi px pz speed dt cast]
   (let [[qx qy qz qw] (yaw-quat psi)]
     (camera/update! cs px 0.5 pz qx qy qz qw speed dt cast))))

(defn- pos [^js c] [(.-x (.-position c)) (.-y (.-position c)) (.-z (.-position c))])

(defn- dist [[ax ay az] [bx by bz]]
  (js/Math.hypot (- ax bx) (- ay by) (- az bz)))

(deftest chase-sits-behind-the-car
  (testing "identity heading: car points down -Z, camera is at +Z"
    (let [[c cs] (rig)]
      (step! cs js/Math.PI)                 ; heading pi == forward (0,-1)
      (let [[x y z] (pos c)]
        (is (< (js/Math.abs x) 0.01) "no lateral offset")
        (is (< 3.5 y 5.0) "raised off the road")
        (is (< 9.0 z 11.0) "ten metres behind"))))

  (testing "quarter turn: the camera follows the car round, not the world"
    (let [[c cs] (rig)]
      ;; heading -pi/2 is forward (-1, 0), so behind the car is +X.
      (step! cs (- (/ js/Math.PI 2)))
      (let [[x _ z] (pos c)]
        (is (< 9.0 x 11.0) "behind is now +X")
        (is (< (js/Math.abs z) 0.01) "and no longer +Z")))))

(deftest first-frame-snaps
  (testing "the camera does not slide in from wherever it was constructed"
    (let [[c cs] (rig)]
      (step! cs js/Math.PI 500.0 -300.0 0.0 (/ 1.0 60.0) nil)
      (let [[x _ z] (pos c)]
        (is (< (js/Math.abs (- x 500.0)) 0.01))
        (is (< (js/Math.abs (- z -290.0)) 0.01) "ten behind a car at z=-300")))))

(deftest speed-pulls-the-camera-back
  (testing "distance and field of view both open up with speed"
    (let [[^js c1 cs1] (rig)
          [^js c2 cs2] (rig)]
      (step! cs1 js/Math.PI 0.0 0.0 0.0 (/ 1.0 60.0) nil)
      (step! cs2 js/Math.PI 0.0 0.0 60.0 (/ 1.0 60.0) nil)
      (is (> (nth (pos c2) 2) (+ 1.0 (nth (pos c1) 2))) "further back")
      (is (> (.-fov c2) (+ 5.0 (.-fov c1))) "wider lens"))))

(deftest look-back-flips-the-view
  (let [[c cs] (rig)]
    (step! cs js/Math.PI)
    (let [behind (pos c)]
      (vreset! (:look-back cs) true)
      ;; Snap so the flip is not half-completed when measured.
      (vreset! (:primed cs) false)
      (step! cs js/Math.PI)
      (let [[x _ z] (pos c)]
        (is (< z -9.0) "now in front of the car")
        (is (> (dist behind [x 0.0 z]) 15.0) "and a long way from where it was"))
      (vreset! (:look-back cs) false))))

(deftest yaw-smoothing-takes-the-short-way-round
  (testing "crossing the -pi/pi seam does not swing the camera through a circle"
    (let [[c cs] (rig)]
      (step! cs 3.0)                              ; settle just below +pi
      (let [travel (loop [i 0, p (pos c), total 0.0]
                     (if (= i 90)
                       total
                       (do (step! cs -3.0)        ; 0.283 rad away across the seam
                           (let [p' (pos c)]
                             (recur (inc i) p' (+ total (dist p p')))))))]
        ;; Short way: an arc of 0.283 rad at ~10 m is under 3 m. The long way
        ;; round is 6.0 rad, i.e. nearly 60 m of travel.
        (is (< travel 5.0) (str "camera travelled " (.toFixed travel 2) " m"))))))

(deftest smoothing-is-frame-rate-independent
  (testing "one second of catch-up lands in the same place at 60 and 120 fps"
    (let [[c60 cs60] (rig)
          [c120 cs120] (rig)]
      (step! cs60 0.0)
      (step! cs120 0.0)
      (dotimes [_ 60] (step! cs60 2.0 0.0 0.0 0.0 (/ 1.0 60.0) nil))
      (dotimes [_ 120] (step! cs120 2.0 0.0 0.0 0.0 (/ 1.0 120.0) nil))
      (is (< (dist (pos c60) (pos c120)) 0.01)
          "a fixed per-frame lerp would put these metres apart"))))

(deftest occluders-pull-the-camera-in
  (testing "something solid three metres out ends the camera short of it"
    (let [[c cs] (rig)
          cast (fn [_ _ _ _ _ _ _] 3.0)]
      (step! cs js/Math.PI 0.0 0.0 0.0 (/ 1.0 60.0) cast)
      ;; unobstruct! measures from just above the car's origin.
      (is (< 2.4 (dist (pos c) [0.0 1.1 0.0]) 2.7))))

  (testing "an unobstructed view is left alone"
    (let [[c cs] (rig)
          cast (fn [_ _ _ _ _ _ _] nil)]
      (step! cs js/Math.PI 0.0 0.0 0.0 (/ 1.0 60.0) cast)
      (is (< 9.0 (nth (pos c) 2) 11.0)))))

(deftest top-view-looks-straight-down
  (let [[^js c cs] (rig)]
    (vreset! (:mode cs) :top)
    (step! cs 1.0 40.0 -20.0 0.0 (/ 1.0 60.0) nil)
    (let [[x y z] (pos c)]
      (is (< (js/Math.abs (- x 40.0)) 0.01))
      (is (< (js/Math.abs (- z -20.0)) 0.01))
      (is (> y 40.0) "well above the car")
      ;; North up the screen, which needs a -Z up vector when looking down.
      (is (< (.-z (.-up c)) -0.9)))))

(deftest hood-view-rides-with-the-car
  (let [[c cs] (rig)]
    (vreset! (:mode cs) :hood)
    (step! cs js/Math.PI 12.0 7.0 0.0 (/ 1.0 60.0) nil)
    (is (< (dist (pos c) [12.0 0.5 7.0]) 1.5) "essentially at the car")))

(deftest mode-ring-cycles-and-wraps
  (let [[_ cs] (rig)]
    (is (= :chase (camera/mode cs)))
    (is (= (vec (rest camera/ring))
           (mapv (fn [_] (camera/cycle-mode! cs)) (rest camera/ring))))
    (is (= :chase (camera/cycle-mode! cs)) "wraps back to the start")
    (is (every? camera/labels camera/ring) "every mode has a HUD label")))
