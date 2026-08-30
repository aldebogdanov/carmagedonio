(ns carmageddon.client.traffic
  "Civilian cars, driving the street graph.

  Traffic is kinematic rather than simulated. Four raycast vehicles already cost
  a fair slice of the tick, and a hundred more would cost the frame -- so a
  civilian car is a box that is *told* where it is, following the same lattice
  the roads were generated from. It still has a collider, so the player can hit
  it, and the moment it is hit hard enough it stops being told anything and
  becomes ordinary dynamic debris. That switch is the whole model: driving is
  kinematic, being destroyed is physics.

  Navigation is a walk, not a route. An infinite world has no destination to
  plan toward, so a driver arriving at a node simply asks what leaves it and
  picks one -- preferring to go straight on, because a city where every car
  turned at random reads as chaos rather than traffic.

  What a car knows about its street is cached until it reaches the next node.
  Rebuilding a street polyline per car per tick is the one thing here that would
  genuinely be too slow."
  (:require ["@dimforge/rapier3d-compat" :as RAPIER]
            ["three" :as three]
            [carmageddon.client.overlay :as overlay]
            [carmageddon.shared.worldgen :as worldgen]))

(def ^:private half [0.82 0.58 1.85])
(def ^:private ride 0.62)          ; body centre above the road surface
(def ^:private lane 0.45)          ; share of the carriageway half-width to sit off
(def ^:private stop-line 0.86)     ; how far along a street a red light holds a car
(def ^:private follow-gap 7.5)     ; metres to leave to the car in front
(def ^:private straight-bonus 6.0) ; weighting for carrying straight on at a node

(def ^:private colours
  [0x9fa4ab 0x3d4b63 0x8c3a34 0xd8d2c4 0x2f4f3a 0x6b5a3e 0x1f2833 0xb08a3c])

(deftype Car [body collider handle key idx colour
              ^:mutable from ^:mutable to ^:mutable t ^:mutable speed
              ^:mutable leg ^:mutable alive? ^:mutable rnd ^:mutable ekey]
  ;; A record would allocate a new one of these per car per tick. Traffic is the
  ;; one place in the client where state is genuinely mutated in place, and
  ;; deftype fields are munged by the ClojureScript compiler rather than left to
  ;; Closure to guess at -- which a plain #js object is not.
  Object
  (toString [_] (str "Car " idx " " from "->" to)))

(defn create [world scene seed ov]
  (atom {:world world :scene scene :seed seed :overlay ov
         :geometry (three/BoxGeometry. (* 2 (nth half 0)) (* 2 (nth half 1))
                                       (* 2 (nth half 2)))
         :material (three/MeshPhongMaterial. #js {:color 0xffffff :shininess 24
                                                  :flatShading true})
         :scratch (three/Object3D.)
         :colour (three/Color.)
         ;; One placement object for the whole simulation. `place!` writes into
         ;; it rather than returning, so driving allocates nothing per tick.
         :place #js {:x 0.0 :y 0.0 :z 0.0 :h 0.0}
         :chunks {}          ; [cx cz] -> {:mesh :cars}
         :by-collider {}     ; handle -> car
         :wrecked 0}))

;; --- geometry along a street ------------------------------------------------

(deftype Leg [^js segs total half ya yb signals? offset phase])

(defn- edge-key
  "An integer naming the street between two lattice nodes.

  Computed once when a car changes street rather than per tick. Building this
  key by `str`-ing the two coordinate vectors -- which is what it was -- costs
  two vector prints per car per tick, and with a couple of hundred cars that
  alone was most of the traffic budget."
  [[ax az] [bx bz]]
  (bit-or 0 (+ (* 7919 (+ (* 131 ax) az)) (+ (* 131 bx) bz))))

(defn- leg
  "Cache everything a car needs about the street it is on: its polyline as a
  flat array, its length, and what waits at the far node.

  Flat and typed because `place!` walks it for every car on every tick, and a
  vector of maps means a keyword lookup per segment per car per tick."
  [seed from to]
  (let [st (worldgen/street-between seed from to)
        pts (:points st)
        n (dec (count pts))
        segs (js/Float64Array. (* 5 n))]
    (dotimes [i n]
      (let [[ax az] (nth pts i)
            [bx bz] (nth pts (inc i))
            o (* 5 i)]
        (aset segs (+ o 0) ax)
        (aset segs (+ o 1) az)
        (aset segs (+ o 2) (- bx ax))
        (aset segs (+ o 3) (- bz az))
        (aset segs (+ o 4) (js/Math.hypot (- bx ax) (- bz az)))))
    (let [total (loop [i 0, acc 0.0]
                  (if (>= i n) acc (recur (inc i) (+ acc (aget segs (+ (* 5 i) 4))))))
          j (worldgen/junction seed (nth to 0) (nth to 1))
          last-o (* 5 (dec n))]
      (->Leg segs (max 1.0 total) (:half st) (:ya st) (:yb st)
             (= :signals (:kind j))
             (:offset j 0.0)
             ;; Opposite approaches share a signal group, so which axis the last
             ;; stretch runs along is all a driver needs to read the lights.
             (if (> (js/Math.abs (aget segs (+ last-o 2)))
                    (js/Math.abs (aget segs (+ last-o 3))))
               0 1)))))

(defn- place!
  "Write the world position and heading `t` of the way along a leg into `out`,
  offset into its lane. Writes rather than returns: this runs per car per tick
  and the object would otherwise be garbage every time."
  [^js out ^Leg lg t]
  (let [segs (.-segs lg)
        n (/ (.-length segs) 5)
        want (* t (.-total lg))]
    (loop [i 0, acc 0.0]
      (let [o (* 5 i)
            len (aget segs (+ o 4))
            last? (>= i (dec n))]
        (if (or last? (<= want (+ acc len)))
          (let [ax (aget segs o) az (aget segs (+ o 1))
                dx (aget segs (+ o 2)) dz (aget segs (+ o 3))
                u (if (pos? len) (max 0.0 (min 1.0 (/ (- want acc) len))) 0.0)
                nx (/ dx (max 1e-6 len)) nz (/ dz (max 1e-6 len))
                ;; Keep right. Without it oncoming cars share a centre line.
                off (* (.-half lg) lane)]
            (set! (.-x out) (+ ax (* dx u) (* (- nz) off)))
            (set! (.-y out) (+ (.-ya lg) (* (- (.-yb lg) (.-ya lg)) t) ride))
            (set! (.-z out) (+ az (* dz u) (* nx off)))
            (set! (.-h out) (js/Math.atan2 nx nz))
            out)
          (recur (inc i) (+ acc len)))))))

(defn- next-node
  "Where to go from `at`, having come from `prev`.

  Turning back is a last resort, and carrying straight on is weighted heavily:
  a driver that picks uniformly at random looks like it is lost, not like it is
  going somewhere."
  [seed prev at rnd]
  (let [arms (worldgen/node-arms seed (nth at 0) (nth at 1))
        [px pz] prev
        heading [(- (nth at 0) px) (- (nth at 1) pz)]
        weight (fn [a]
                 (let [[tx tz] (:to a)
                       d [(- tx (nth at 0)) (- tz (nth at 1))]]
                   (cond (= (:to a) prev) 0.05
                         (= d heading) straight-bonus
                         :else 1.0)))
        ws (mapv weight arms)
        total (reduce + ws)]
    (when (seq arms)
      (let [pick (* rnd total)]
        (loop [i 0, acc 0.0]
          (if (or (= i (dec (count arms))) (< pick (+ acc (nth ws i))))
            (:to (nth arms i))
            (recur (inc i) (+ acc (nth ws i)))))))))

;; --- spawning ---------------------------------------------------------------

(defn- spawn-one! [ts key idx from to t0 speed]
  (let [{:keys [^js world seed]} @ts
        lg (leg seed from to)
        p (place! #js {:x 0.0 :y 0.0 :z 0.0 :h 0.0} lg t0)
        [hx hy hz] half
        ^js body (.createRigidBody
                  world
                  (-> (.kinematicPositionBased RAPIER/RigidBodyDesc)
                      (.setTranslation (.-x p) (.-y p) (.-z p))))
        ^js collider (.createCollider
                      world
                      (-> (.cuboid RAPIER/ColliderDesc hx hy hz)
                          (.setDensity 300.0)
                          (.setFriction 0.8)
                          (.setRestitution 0.1)
                          (.setActiveEvents (.-CONTACT_FORCE_EVENTS RAPIER/ActiveEvents))
                          (.setContactForceEventThreshold 1500.0))
                      body)]
    (->Car body collider (.-handle collider) key idx
           (nth colours (mod (+ idx (nth from 0) (nth from 1)) (count colours)))
           from to t0 speed lg true
           (js/Math.abs (js/Math.sin (+ (* 12.9898 idx) (* 0.017 (nth from 0)))))
           (edge-key from to))))

(defn add-chunk! [ts key arr]
  (when (and arr (pos? (.-length arr)))
    (let [{:keys [^js scene ^js geometry ^js material overlay]} @ts
          gone (overlay/destroyed overlay key :cars)
          st worldgen/traffic-stride
          n (/ (.-length arr) st)
          ;; A car smashed earlier stays smashed when its chunk comes back,
          ;; exactly as a crate does.
          cars (into-array
                (for [i (range n)
                      :when (not (contains? gone i))
                      :let [o (* i st)]]
                  (spawn-one! ts key i
                              [(int (aget arr (+ o 0))) (int (aget arr (+ o 1)))]
                              [(int (aget arr (+ o 2))) (int (aget arr (+ o 3)))]
                              (aget arr (+ o 4)) (aget arr (+ o 5)))))
          ^js mesh (three/InstancedMesh. geometry material (max 1 (alength cars)))]
      (set! (.-frustumCulled mesh) false)
      (set! (.-castShadow mesh) true)
      (set! (.-receiveShadow mesh) true)
      (.add scene mesh)
      (swap! ts (fn [s]
                  (-> s
                      (assoc-in [:chunks key] {:mesh mesh :cars cars})
                      (update :by-collider into
                              (map (fn [c] [(.-handle c) c]) cars)))))
      cars)))

(defn remove-chunk! [ts key]
  (let [{:keys [^js world ^js scene chunks]} @ts]
    (when-let [{:keys [^js mesh cars]} (get chunks key)]
      (.remove scene mesh)
      (.dispose mesh)
      (doseq [c0 cars] (let [^Car c c0] (.removeRigidBody world ^js (.-body c))))
      (swap! ts (fn [s]
                  (-> s
                      (update :chunks dissoc key)
                      (update :by-collider
                              #(apply dissoc % (map (fn [^Car c] (.-handle c)) cars))))))) ))

;; --- driving ----------------------------------------------------------------

(defn- hold-for-lights?
  [{:keys [signals? offset phase]} t now-s]
  (and signals? (> t stop-line)
       (not= :green (worldgen/signal-state now-s offset phase))))

(defn drive!
  "Advance every living car one tick.

  Cars are bucketed by the street they are on before anything moves, so keeping
  a gap to the car in front costs one pass rather than comparing every car with
  every other. On a busy grid that is the difference between a few hundred
  comparisons and a few tens of thousands."
  [ts dt now-ms]
  (let [{:keys [seed chunks ^js place]} @ts
        now-s (/ now-ms 1000.0)
        ahead (js/Map.)]
    ;; Who is on which street, and how far along.
    (doseq [[_ {:keys [cars]}] chunks, c0 cars]
      (let [^Car c c0]
        (when (.-alive? c)
          (let [k (.-ekey c)
                v (or (.get ahead k) (let [a (array)] (.set ahead k a) a))]
            (.push v c)))))
    (doseq [[_ {:keys [cars]}] chunks, c0 cars
            :let [^Car c c0]
            :when (.-alive? c)]
      (let [^Leg lg (.-leg c)
            total (.-total lg)
            t' (+ (.-t c) (/ (* (.-speed c) dt) total))
            ;; The car in front.
            peers (.get ahead (.-ekey c))
            gap (/ follow-gap total)
            t' (loop [i 0, cap t']
                 (if (>= i (alength peers))
                   cap
                   (let [^Car o (aget peers i)]
                     (recur (inc i)
                            (if (and (not (identical? o c)) (> (.-t o) (.-t c)))
                              (min cap (- (.-t o) gap))
                              cap)))))
            ;; And the lights.
            t' (if (and (.-signals? lg) (> t' stop-line)
                        (not= :green (worldgen/signal-state now-s (.-offset lg)
                                                            (.-phase lg))))
                 (min t' stop-line)
                 t')]
        (if (>= t' 1.0)
          ;; Arrived. Pick the next street and carry the overshoot into it.
          (let [nxt (next-node seed (.-from c) (.-to c) (.-rnd c))]
            (if nxt
              (do (set! (.-leg c) (leg seed (.-to c) nxt))
                  (set! (.-from c) (.-to c))
                  (set! (.-to c) nxt)
                  (set! (.-ekey c) (edge-key (.-from c) (.-to c)))
                  (set! (.-t c) (min 0.9 (- t' 1.0)))
                  ;; Cheap deterministic churn so a car does not take the same
                  ;; turn at every junction for ever.
                  (set! (.-rnd c) (mod (+ (* 1.61803 (.-rnd c)) 0.31831) 1.0)))
              (set! (.-t c) 1.0)))
          (set! (.-t c) (max 0.0 t')))
        (let [p (place! place (.-leg c) (.-t c))
              ^js body (.-body c)]
          (.setNextKinematicTranslation body p)
          (.setNextKinematicRotation body
                                     #js {:x 0.0 :y (js/Math.sin (* 0.5 (.-h p)))
                                          :z 0.0 :w (js/Math.cos (* 0.5 (.-h p)))}))))))

(defn sync!
  "Copy every car's transform onto its chunk's instanced mesh. Wrecks are read
  from the body like anything else -- once a car is debris the physics is the
  only thing that knows where it is."
  [ts]
  (let [{:keys [chunks ^js scratch ^js colour]} @ts]
    (doseq [[_ {:keys [^js mesh cars]}] chunks]
      (dotimes [i (alength cars)]
        (let [^Car c (aget cars i)
              ^js body (.-body c)
              t (.translation body)
              r (.rotation body)]
          (.set (.-position scratch) (.-x t) (.-y t) (.-z t))
          (.set (.-quaternion scratch) (.-x r) (.-y r) (.-z r) (.-w r))
          (.set (.-scale scratch) 1 1 1)
          (.updateMatrix scratch)
          (.setMatrixAt mesh i (.-matrix scratch))
          (.setHex colour (.-colour c))
          (.setColorAt mesh i colour)))
      (set! (.-needsUpdate (.-instanceMatrix mesh)) true)
      (when-let [ic (.-instanceColor mesh)] (set! (.-needsUpdate ic) true)))))

(defn traffic? [ts handle] (contains? (:by-collider @ts) handle))

(defn wreck!
  "Stop driving a car and let physics have it. Returns the delta describing what
  was wrecked, or nil if this was not the hit that did it."
  [ts handle impulse]
  (when-let [^Car c (get (:by-collider @ts) handle)]
    (when (.-alive? c)
      (set! (.-alive? c) false)
      (overlay/record! (:overlay @ts) (.-key c) :cars (.-idx c))
      (let [^js body (.-body c)]
        (.setBodyType body (.-Dynamic RAPIER/RigidBodyType) true)
        (.applyImpulse body
                       #js {:x (* 90.0 (nth impulse 0))
                            :y (+ 900.0 (js/Math.abs (* 40.0 (nth impulse 1))))
                            :z (* 90.0 (nth impulse 2))}
                       true))
      (swap! ts update :wrecked inc)
      {:cx (first (.-key c)) :cz (second (.-key c)) :index (.-idx c)})))

(defn wreck-index!
  "Wreck car `idx` of chunk `key` because someone else did. Recorded even when
  that chunk is not loaded here, so it stays wrecked when it arrives."
  [ts key idx]
  (overlay/record! (:overlay @ts) key :cars idx)
  (when-let [{:keys [cars]} (get (:chunks @ts) key)]
    (doseq [c0 cars]
      (let [^Car c c0]
        (when (and (= idx (.-idx c)) (.-alive? c))
          (wreck! ts (.-handle c) [0.0 0.0 0.0]))))))

(defn stats [ts]
  (let [all (mapcat (fn [[_ v]] (seq (:cars v))) (:chunks @ts))]
    {:cars (count all)
     :driving (count (filter (fn [^Car c] (.-alive? c)) all))
     :wrecked (:wrecked @ts)}))
