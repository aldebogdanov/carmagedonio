(ns carmageddon.client.powerups
  "Crates worth driving over, and what happens for the next few seconds.

  The world was already full of smashable crates. These are a different thing:
  they sit *on* the carriageway rather than beside it, they are collected by
  driving through them rather than by hitting them, and they are gone
  afterwards -- recorded in the overlay exactly like a smashed prop, so they do
  not come back when the chunk does.

  Every effect but the repair is a timer. That is deliberate: a permanent
  upgrade turns a run into a shopping trip, and the interesting decision is
  whether the ten seconds you are holding right now is worth spending on the
  crowd in front of you or saving for the rival behind.

  Boosts are multipliers on the vehicle rather than edits to its tuning. The
  reference car's tuning atom is shared with the measurement harness, and a
  nitro that leaked into it would quietly change what every published number
  means."
  (:require ["three" :as three]
            [carmageddon.client.figures :as fig]
            [carmageddon.client.fire :as fire]
            [carmageddon.client.overlay :as overlay]
            [carmageddon.client.vehicle :as vehicle]
            [carmageddon.shared.worldgen :as worldgen]))

(def kinds
  "One entry per `worldgen/pickup-kinds`, in that order."
  [{:name :repair :colour 0x4fbf6a :label "REPAIR"   :secs 0.0}
   {:name :nitro  :colour 0xf05a28 :label "NITRO"    :secs 9.0}
   {:name :grip   :colour 0x39a0d8 :label "GRIP"     :secs 12.0}
   {:name :armour :colour 0xb9bec6 :label "ARMOUR"   :secs 14.0}
   {:name :flame  :colour 0xf2a63a :label "FIRETRAIL" :secs 8.0}
   {:name :shock  :colour 0xd8d84a :label "SHOCK"    :secs 8.0}])

(def ^:private reach 3.2)       ; metres: how close is "drove through it"
(def ^:private slots 260)

;; Fire trail: a short-lived puddle every few ticks. Any faster and the pool
;; cap is spent in two seconds of driving.
(def ^:private trail-every 9)
(def ^:private trail-r 2.6)
(def ^:private trail-life 3.6)

;; Shock: arcs to whatever is nearest, on a beat rather than continuously.
(def ^:private shock-every 24)
(def ^:private shock-reach 14.0)

(defn create [^js scene ov]
  (atom {:scene scene
         :overlay ov
         :pool (fig/pool scene (three/BoxGeometry. 1 1 1)
                         (three/MeshPhongMaterial. #js {:emissive 0x000000
                                                        :shininess 40
                                                        :flatShading true})
                         slots {:cast? true})
         :m4 (three/Matrix4.)
         :chunks {}          ; [cx cz] -> [{:x :y :z :kind :idx :slot}]
         :active {}          ; kind -> seconds remaining
         :taken 0}))

(defn taken-in [ps key] (overlay/destroyed (:overlay @ps) key :pickups))

(defn add-chunk! [ps key arr]
  (when (and arr (pos? (.-length arr)))
    (let [gone (taken-in ps key)
          st worldgen/pickup-stride
          n (/ (.-length arr) st)
          pool (:pool @ps)
          made (vec (for [idx (range n)
                          :when (not (contains? gone idx))
                          :let [o (* idx st)
                                kind (int (aget arr (+ o 3)))
                                slot (fig/claim! pool)]]
                      (do (fig/set-colour! pool slot (:colour (nth kinds kind)))
                          {:x (aget arr (+ o 0)) :y (aget arr (+ o 1))
                           :z (aget arr (+ o 2)) :kind kind :idx idx :slot slot})))]
      (swap! ps assoc-in [:chunks key] made)
      made)))

(defn remove-chunk! [ps key]
  (let [pool (:pool @ps)]
    (doseq [{:keys [slot]} (get (:chunks @ps) key)] (fig/release! pool slot))
    (swap! ps update :chunks dissoc key)))

(defn- take-index! [ps key idx]
  (overlay/record! (:overlay @ps) key :pickups idx)
  (when-let [p (first (filter #(= idx (:idx %)) (get (:chunks @ps) key)))]
    (fig/release! (:pool @ps) (:slot p))
    (swap! ps (fn [st]
                (-> st
                    (update-in [:chunks key] (fn [v] (vec (remove #(= idx (:idx %)) v))))
                    (update :taken inc))))
    p))

(defn take-index-remote!
  "Somebody else drove over it. Recorded even when the chunk is not loaded
  here, so it stays gone when it arrives."
  [ps key idx]
  (take-index! ps key idx)
  nil)

(defn collect!
  "Whatever the car at (x, z) is standing on. Returns `{:kind :delta}` or nil.

  A distance check rather than a sensor collider: there are a handful of these
  per chunk, the answer is wanted once a tick, and a collider would mean a
  broad-phase entry and a contact event for something that is not solid."
  [ps x z]
  (let [r2 (* reach reach)
        ;; Found first, taken second. Taking it inside the sequence was a bug
        ;; waiting for a crowded street: `for` is lazy and chunked, so `first`
        ;; realises up to thirty-two elements -- and every one of them would
        ;; have been collected, consuming crates the car never reached.
        hit (first
             (for [[key ps'] (:chunks @ps)
                   p ps'
                   :let [dx (- (:x p) x) dz (- (:z p) z)]
                   :when (< (+ (* dx dx) (* dz dz)) r2)]
               [key p]))]
    (when hit
      (let [[key p] hit]
        (take-index! ps key (:idx p))
        {:kind (:kind p)
         :delta {:cx (first key) :cz (second key) :index (:idx p)}}))))

;; --- what holding one does -------------------------------------------------

(def ^:private index-of-name (zipmap (map :name kinds) (range)))

(defn- effect! [veh kind on?]
  (case (:name (nth kinds kind))
    :nitro  (vehicle/set-boost! veh vehicle/boost-engine (if on? 1.75 1.0))
    :grip   (vehicle/set-boost! veh vehicle/boost-grip (if on? 1.30 1.0))
    :armour (vehicle/set-boost! veh vehicle/boost-armour (if on? 3.2 1.0))
    nil))

(defn apply!
  "Take the effect of `kind`. Returns the label to flash on the dashboard."
  [ps veh kind]
  (let [{:keys [name secs label]} (nth kinds kind)]
    (if (= :repair name)
      ;; The only instant one, and the only one worth grabbing when you are
      ;; already holding something else.
      (vehicle/repair! veh 0.45)
      (do (swap! ps assoc-in [:active name] secs)
          (effect! veh kind true)))
    label))

(defn tick!
  "Run down the timers and do whatever the held effects do this tick.

  `emit` is called with `[:fire x y z]` or `[:shock x y z]` -- powerups do not
  reach into the fire system or the crowd themselves, because what a shock
  hits is a gameplay question and this namespace is about what is held."
  [ps veh tick dt emit]
  (let [act (:active @ps)]
    (when (seq act)
      (doseq [[k secs] act]
        (let [left (- secs dt)]
          (if (pos? left)
            (swap! ps assoc-in [:active k] left)
            (do (swap! ps update :active dissoc k)
                (effect! veh (index-of-name k) false)
                ;; Belt and braces: whatever expired, put everything back. A
                ;; missed reset here is a permanent nitro.
                (when (empty? (:active @ps)) (vehicle/clear-boosts! veh))))))
      (let [[x y z] (vehicle/chassis-position veh)]
        (when (and (contains? (:active @ps) :flame) (zero? (mod tick trail-every)))
          (emit [:fire x (- y 0.5) z trail-r trail-life]))
        (when (and (contains? (:active @ps) :shock) (zero? (mod tick shock-every)))
          (emit [:shock x y z shock-reach]))))))

(defn active [ps] (:active @ps))

(defn label-for [k]
  (:label (first (filter #(= k (:name %)) kinds))))

(defn sync!
  "Spin them. A pickup that sits still on a grey road is a pickup nobody sees."
  [ps now-s]
  (let [{:keys [pool ^js m4 chunks]} @ps]
    (doseq [[_ ps'] chunks
            {:keys [x y z slot kind]} ps']
      (let [a (+ (* 1.6 now-s) (* 0.7 kind))
            bob (* 0.18 (js/Math.sin (+ (* 2.2 now-s) kind)))
            c (js/Math.cos a) s (js/Math.sin a)
            ^js e (.-elements m4)]
        ;; Yaw by hand rather than through an Object3D: one pickup is nine
        ;; stores and a matrix write, and there is no other transform involved.
        (aset e 0 (* 0.8 c)) (aset e 1 0.0) (aset e 2 (* 0.8 (- s))) (aset e 3 0.0)
        (aset e 4 0.0) (aset e 5 0.8) (aset e 6 0.0) (aset e 7 0.0)
        (aset e 8 (* 0.8 s)) (aset e 9 0.0) (aset e 10 (* 0.8 c)) (aset e 11 0.0)
        (aset e 12 x) (aset e 13 (+ y bob)) (aset e 14 z) (aset e 15 1.0)
        (fig/set-matrix! pool slot m4)))
    (fig/flush! pool)))

(defn stats [ps]
  {:live (reduce + (map count (vals (:chunks @ps))))
   :taken (:taken @ps)
   :active (count (:active @ps))})
