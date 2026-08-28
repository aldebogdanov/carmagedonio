(ns carmageddon.shared.wire
  "Binary protocol between client and server.

  Binary rather than transit or EDN because of the rate: a car snapshot goes out
  25 times a second per player, and at that frequency the framing of a textual
  format costs more than the payload it carries. A car is 30 bytes here; the
  same data as EDN is closer to 200.

  Values are quantised, not rounded off for fun: positions stay f32 because a
  car crossing an infinite world needs the range, while velocity and rotation
  become i16 because nobody can see the difference in a proxy that is already
  being rendered a tenth of a second in the past.

  Encoding is identical on both platforms -- the JVM writes with ByteBuffer, the
  browser with DataView, and `wire_test.cljc` asserts they agree byte for byte."
  (:require [carmageddon.shared.constants :as k]))

;; --- message types ----------------------------------------------------------

(def msg-hello   1)   ; client -> server: I want in
(def msg-welcome 2)   ; server -> client: you are player N, world seed is S
(def msg-state   3)   ; both ways: car snapshots
(def msg-delta   4)   ; both ways: something in the world was destroyed
(def msg-bye     5)   ; server -> client: player N left

(def car-bytes 30)

;; Quantisation scales. Velocity beyond this is clamped rather than wrapped --
;; a wrapped velocity would send a proxy flying in the opposite direction.
(def ^:private vel-scale 273.0)      ; +/- 120 m/s across an i16
(def ^:private quat-scale 32767.0)

;; --- portable buffer --------------------------------------------------------

(defn- alloc [n]
  #?(:clj  (java.nio.ByteBuffer/allocate n)
     :cljs (js/DataView. (js/ArrayBuffer. n))))

(defn- put-u8! [b o v]
  #?(:clj  (.put ^java.nio.ByteBuffer b (int o) (unchecked-byte v))
     :cljs (.setUint8 b o v)))
(defn- get-u8 [b o]
  #?(:clj  (bit-and (.get ^java.nio.ByteBuffer b (int o)) 0xFF)
     :cljs (.getUint8 b o)))

(defn- put-u16! [b o v]
  #?(:clj  (.putShort ^java.nio.ByteBuffer b (int o) (unchecked-short v))
     :cljs (.setUint16 b o v)))
(defn- get-u16 [b o]
  #?(:clj  (bit-and (.getShort ^java.nio.ByteBuffer b (int o)) 0xFFFF)
     :cljs (.getUint16 b o)))

(defn- put-i16! [b o v]
  #?(:clj  (.putShort ^java.nio.ByteBuffer b (int o) (unchecked-short v))
     :cljs (.setInt16 b o v)))
(defn- get-i16 [b o]
  #?(:clj  (.getShort ^java.nio.ByteBuffer b (int o))
     :cljs (.getInt16 b o)))

(defn- put-i32! [b o v]
  #?(:clj  (.putInt ^java.nio.ByteBuffer b (int o) (unchecked-int v))
     :cljs (.setInt32 b o v)))
(defn- get-i32 [b o]
  #?(:clj  (.getInt ^java.nio.ByteBuffer b (int o))
     :cljs (.getInt32 b o)))

(defn- put-f32! [b o v]
  #?(:clj  (.putFloat ^java.nio.ByteBuffer b (int o) (float v))
     :cljs (.setFloat32 b o v)))
(defn- get-f32 [b o]
  #?(:clj  (.getFloat ^java.nio.ByteBuffer b (int o))
     :cljs (.getFloat32 b o)))

(defn bytes-of [b]
  #?(:clj  (.array ^java.nio.ByteBuffer b)
     :cljs (js/Uint8Array. (.-buffer b))))

(defn view-of [ba]
  #?(:clj  (java.nio.ByteBuffer/wrap ba)
     :cljs (js/DataView. (if (instance? js/ArrayBuffer ba) ba (.-buffer ba)))))

(defn byte-length [ba]
  #?(:clj (alength ^bytes ba) :cljs (.-byteLength ba)))

(defn- clamp [v lo hi] (max lo (min hi v)))

(defn- js-round [v]
  #?(:clj (Math/round (double v)) :cljs (js/Math.round v)))

;; --- car snapshot -----------------------------------------------------------

(defn- put-car! [b o {:keys [id pos quat vel damage]}]
  (let [[px py pz] pos
        [qx qy qz qw] quat
        [vx vy vz] vel]
    (put-u16! b o id)
    (put-f32! b (+ o 2) px)
    (put-f32! b (+ o 6) py)
    (put-f32! b (+ o 10) pz)
    (put-i16! b (+ o 14) (js-round (* quat-scale (clamp qx -1.0 1.0))))
    (put-i16! b (+ o 16) (js-round (* quat-scale (clamp qy -1.0 1.0))))
    (put-i16! b (+ o 18) (js-round (* quat-scale (clamp qz -1.0 1.0))))
    (put-i16! b (+ o 20) (js-round (* quat-scale (clamp qw -1.0 1.0))))
    (put-i16! b (+ o 22) (js-round (* vel-scale (clamp vx -120.0 120.0))))
    (put-i16! b (+ o 24) (js-round (* vel-scale (clamp vy -120.0 120.0))))
    (put-i16! b (+ o 26) (js-round (* vel-scale (clamp vz -120.0 120.0))))
    (put-u8!  b (+ o 28) (js-round (* 255.0 (clamp (or damage 0.0) 0.0 1.0))))
    (put-u8!  b (+ o 29) 0)))

(defn- get-car [b o]
  {:id   (get-u16 b o)
   :pos  [(get-f32 b (+ o 2)) (get-f32 b (+ o 6)) (get-f32 b (+ o 10))]
   :quat [(/ (get-i16 b (+ o 14)) quat-scale)
          (/ (get-i16 b (+ o 16)) quat-scale)
          (/ (get-i16 b (+ o 18)) quat-scale)
          (/ (get-i16 b (+ o 20)) quat-scale)]
   :vel  [(/ (get-i16 b (+ o 22)) vel-scale)
          (/ (get-i16 b (+ o 24)) vel-scale)
          (/ (get-i16 b (+ o 26)) vel-scale)]
   :damage (/ (get-u8 b (+ o 28)) 255.0)})

;; --- messages ---------------------------------------------------------------

(defn encode-state
  "[type u8][tick u32][count u8][car * count]"
  [tick cars]
  (let [n (count cars)
        b (alloc (+ 6 (* n car-bytes)))]
    (put-u8! b 0 msg-state)
    (put-i32! b 1 tick)
    (put-u8! b 5 n)
    (dotimes [i n]
      (put-car! b (+ 6 (* i car-bytes)) (nth cars i)))
    (bytes-of b)))

(defn encode-welcome [player-id seed]
  (let [b (alloc 9)]
    (put-u8! b 0 msg-welcome)
    (put-u16! b 1 player-id)
    (put-i32! b 3 seed)
    (put-u16! b 7 k/protocol-version)
    (bytes-of b)))

(defn encode-hello []
  (let [b (alloc 3)]
    (put-u8! b 0 msg-hello)
    (put-u16! b 1 k/protocol-version)
    (bytes-of b)))

(defn encode-bye [player-id]
  (let [b (alloc 3)]
    (put-u8! b 0 msg-bye)
    (put-u16! b 1 player-id)
    (bytes-of b)))

(def delta-kinds {:prop 0 :ped 1 :car 2})
(def delta-kind-of (into {} (map (fn [[k v]] [v k]) delta-kinds)))

(defn encode-delta
  "Something in the shared world was destroyed. Twelve bytes, and the only
  world-state traffic there is -- everything else both sides derive from the
  seed."
  [{:keys [cx cz kind index]}]
  (let [b (alloc 12)]
    (put-u8! b 0 msg-delta)
    (put-i32! b 1 cx)
    (put-i32! b 5 cz)
    (put-u8! b 9 (delta-kinds kind))
    (put-u16! b 10 index)
    (bytes-of b)))

(defn decode
  "Decode one frame. Returns a map with :type, or nil if it is unreadable."
  [ba]
  (let [len (byte-length ba)]
    (when (pos? len)
      (let [b (view-of ba)
            t (get-u8 b 0)]
        (condp = t
          msg-hello   {:type :hello :protocol (get-u16 b 1)}
          msg-welcome {:type :welcome :player-id (get-u16 b 1)
                       :seed (get-i32 b 3) :protocol (get-u16 b 7)}
          msg-bye     {:type :bye :player-id (get-u16 b 1)}
          msg-delta   {:type :delta :cx (get-i32 b 1) :cz (get-i32 b 5)
                       :kind (delta-kind-of (get-u8 b 9)) :index (get-u16 b 10)}
          msg-state   (let [tick (get-i32 b 1)
                            n    (get-u8 b 5)]
                        (when (= len (+ 6 (* n car-bytes)))
                          {:type :state :tick tick
                           :cars (mapv #(get-car b (+ 6 (* % car-bytes))) (range n))}))
          nil)))))
