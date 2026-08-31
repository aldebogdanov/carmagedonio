(ns carmageddon.shared.wire-test
  "The wire is the one place client and server exchange raw bytes, so the bytes
  themselves are asserted -- not just that a round-trip survives. A codec that
  round-trips correctly on each platform but disagrees between them would look
  perfectly healthy in isolation and fail only when the two actually talk."
  (:require [clojure.test :refer [deftest is testing]]
            [carmageddon.shared.wire :as w]))

(defn- u
  "Bytes as unsigned ints; the JVM's are signed and JS's are not."
  [ba]
  #?(:clj  (mapv #(bit-and % 0xFF) (seq ba))
     :cljs (vec (js/Array.from ba))))

(def sample-car
  {:id 7 :pos [141.5 0.73 -128.25]
   :quat [0.0 0.3826834 0.0 0.9238795]
   :vel [12.5 -0.25 -30.0] :damage 0.42})

(deftest state-bytes-are-identical-on-both-platforms
  (is (= [3 0 0 4 210 1 0 7 67 13 128 0 63 58 225 72 195 0 64 0 0 0 48 251 0 0
          118 65 13 85 255 188 224 2 107 0]
         (u (w/encode-state 1234 [sample-car])))))

(deftest welcome-and-delta-bytes-are-identical
  (is (= [2 0 3 192 0 0 0 0 1] (u (w/encode-welcome 3 -1073741824))))
  (is (= [4 255 255 255 254 0 0 0 5 1 0 13]
         (u (w/encode-delta {:cx -2 :cz 5 :kind :ped :index 13})))))

(deftest a-car-is-thirty-bytes
  (testing "size is a protocol promise, not an accident"
    (is (= 30 w/car-bytes))
    (is (= (+ 6 30) (w/byte-length (w/encode-state 0 [sample-car]))))
    (is (= (+ 6 (* 4 30)) (w/byte-length (w/encode-state 0 (repeat 4 sample-car)))))))

(deftest state-round-trips-within-quantisation
  (let [{:keys [type tick cars]} (w/decode (w/encode-state 99 [sample-car]))
        c (first cars)]
    (is (= :state type))
    (is (= 99 tick))
    (is (= 7 (:id c)))
    (testing "position is f32: an infinite world needs the range"
      (is (= [141.5 0.73 -128.25] (mapv #(/ (Math/round (* % 100.0)) 100.0) (:pos c)))))
    (testing "rotation and velocity are i16: nobody can see the difference"
      (is (every? #(< (abs %) 1e-4) (map - (:quat c) (:quat sample-car))))
      (is (every? #(< (abs %) 0.01) (map - (:vel c) (:vel sample-car)))))
    (is (< (abs (- 0.42 (:damage c))) 0.005))))

(deftest the-vehicle-kind-rides-in-the-spare-byte
  (testing "a car frame is still thirty bytes, and it now says what it is"
    ;; The last byte was reserved padding. Spending it on the catalogue index
    ;; is what stops every remote player being drawn as the reference saloon.
    (let [frame (w/encode-state 1 [(assoc sample-car :kind 4)])
          c (first (:cars (w/decode frame)))]
      (is (= (+ 6 30) (w/byte-length frame)))
      (is (= 4 (:kind c)))))
  (testing "and a frame from something that does not set it decodes as zero"
    (is (= 0 (:kind (first (:cars (w/decode (w/encode-state 1 [sample-car])))))))))

(deftest several-cars-survive-together
  (let [cars (mapv #(assoc sample-car :id % :pos [(* % 10.0) 1.0 (* % -5.0)]) (range 6))
        decoded (:cars (w/decode (w/encode-state 5 cars)))]
    (is (= 6 (count decoded)))
    (is (= (mapv :id cars) (mapv :id decoded)))
    (is (= (mapv :pos cars) (mapv :pos decoded)))))

(deftest velocity-is-clamped-not-wrapped
  (testing "a wrapped velocity would send a proxy flying backwards"
    (let [c (assoc sample-car :vel [999.0 0.0 -999.0])
          [vx _ vz] (:vel (first (:cars (w/decode (w/encode-state 0 [c])))))]
      (is (pos? vx))
      (is (neg? vz))
      (is (< (abs vx) 121.0)))))

(deftest hello-and-bye-round-trip
  (is (= :hello (:type (w/decode (w/encode-hello)))))
  (is (= {:type :bye :player-id 42} (w/decode (w/encode-bye 42)))))

(deftest empty-and-unreadable-frames
  (testing "a state with no cars is valid -- it is what a lone player sends"
    (is (= {:type :state :tick 0 :cars []} (w/decode (w/encode-state 0 [])))))
  (testing "an empty frame is not a crash"
    (is (nil? (w/decode #?(:clj (byte-array 0) :cljs (js/Uint8Array. 0))))))
  (testing "an unknown message type is ignored rather than throwing"
    (is (nil? (w/decode #?(:clj (byte-array [(byte 99) (byte 0)])
                           :cljs (js/Uint8Array. #js [99 0])))))))
