(ns carmageddon.client.ai-test
  "The driver, tested as a controller: given a situation, what does it ask for?

  No physics here on purpose. Everything below is a question about the input
  map, and the one bug this file exists for was invisible in the physics --
  the car behaved exactly as a car does when the throttle is held open."
  (:require [cljs.test :refer-macros [deftest is testing]]
            [carmageddon.client.ai :as ai]))

(defn- ahead
  "A driver at the origin pointing along -Z, with a target `d` metres in front
  and `speed` on the clock."
  [ctl speed d opts]
  (ai/drive-toward ctl
                   {:x 0.0 :z 0.0 :forward [0.0 0.0 -1.0] :speed speed}
                   [0.0 0.0 (- d)]
                   opts))

(deftest a-driver-asks-for-what-its-car-can-give
  ;; The tractor's top speed is 12 m/s and the controller's ceiling was 26 for
  ;; everybody. Cruising flat out, the error term stayed positive, so the
  ;; throttle never came off for the whole run: full torque into agricultural
  ;; tyres, wheelspin, and a rival that slewed round corners it never lifted
  ;; for. It read as a physics fault and was a driver who did not know what it
  ;; was driving.
  (testing "at its own top speed on a straight, a slow car stops accelerating"
    (let [ctl (ai/controller)
          out (ahead ctl 12.0 200.0 {:top 12.0})]
      (is (zero? (:throttle out))
          (str "throttle " (:throttle out) " at the top speed it was given"))))

  (testing "and the same car below its top speed still asks for more"
    (let [ctl (ai/controller)
          out (ahead ctl 5.0 200.0 {:top 12.0})]
      (is (pos? (:throttle out)))))

  (testing "a fast car is unaffected -- its ceiling is the controller's"
    (let [ctl (ai/controller)
          slow (ahead ctl 12.0 200.0 {:top 40.0})]
      (is (pos? (:throttle slow))
          "a 40 m/s car at 12 m/s should still be accelerating")))

  (testing "and with no top speed given, nothing changes for anybody"
    (let [a (ahead (ai/controller) 12.0 200.0 nil)
          b (ahead (ai/controller) 12.0 200.0 {:top 40.0})]
      (is (= (:throttle a) (:throttle b)))))

  (testing "a committed run is capped by the car too"
    ;; `commit?` skips the corner logic and asks for everything. Everything
    ;; still has to mean what the car has.
    (let [ctl (ai/controller)
          out (ahead ctl 12.0 30.0 {:commit? true :top 12.0})]
      (is (zero? (:throttle out))
          "a ramming run at the car's top speed still had the throttle open"))))

(deftest a-driver-lifts-for-a-corner
  ;; The other half of the same knob: the speed asked for falls away with the
  ;; angle to the target, and that has to scale with the car as well.
  (testing "a target off to the side is worth less throttle than one ahead"
    (let [straight (ai/drive-toward (ai/controller)
                                    {:x 0.0 :z 0.0 :forward [0.0 0.0 -1.0]
                                     :speed 6.0}
                                    [0.0 0.0 -200.0] {:top 12.0})
          corner   (ai/drive-toward (ai/controller)
                                    {:x 0.0 :z 0.0 :forward [0.0 0.0 -1.0]
                                     :speed 6.0}
                                    [200.0 0.0 0.0] {:top 12.0})]
      (is (> (:throttle straight) (:throttle corner)))
      (is (not (zero? (:steer corner))) "and it turns towards it"))))
