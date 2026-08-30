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
            [carmageddon.client.figures :as fig]
            [carmageddon.client.overlay :as overlay]
            [carmageddon.shared.worldgen :as worldgen]))

(def ^:private lane 0.45)          ; share of the carriageway half-width to sit off
(def ^:private stop-line 0.86)     ; how far along a street a red light holds a car
(def ^:private follow-gap 7.5)     ; metres to leave to the car in front
(def ^:private straight-bonus 6.0) ; weighting for carrying straight on at a node

(def ^:private colours
  [0x9fa4ab 0x3d4b63 0x8c3a34 0xd8d2c4 0x2f4f3a 0x6b5a3e 0x1f2833 0xb08a3c])

(def ^:private glass 0x1d2733)
(def ^:private rubber 0x1b1b1e)

;; Local +Z is forward here, not -Z: `place!` yaws by atan2(nx, nz), which maps
;; local +Z onto the direction of travel. It never mattered while a car was a
;; symmetrical box; it matters the moment one has a windscreen.
;;
;; `parts` are the shapes bolted to the body, `wheel` where the four go. The
;; collider is still one cuboid -- what the player hits is the shape of the
;; vehicle, not the shape of its cab.
(def ^:private types
  [{:name :saloon
    :half [0.82 0.55 1.95] :ride 0.60
    :wheel {:r 0.32 :w 0.22 :track 0.80 :base 1.32}
    :parts [{:at [0.0 0.46 -0.10] :size [1.42 0.50 1.90] :tint :glass}]}
   {:name :hatch
    :half [0.76 0.52 1.62] :ride 0.56
    :wheel {:r 0.29 :w 0.20 :track 0.74 :base 1.10}
    :parts [{:at [0.0 0.44 -0.06] :size [1.34 0.48 1.60] :tint :glass}]}
   {:name :van
    :half [0.88 0.86 2.35] :ride 0.90
    :wheel {:r 0.35 :w 0.24 :track 0.84 :base 1.60}
    :parts [{:at [0.0 0.30 1.70] :size [1.66 1.00 0.70] :tint :glass}
            {:at [0.0 0.94 -0.30] :size [1.70 0.14 3.60] :tint :body}]}
   {:name :pickup
    :half [0.86 0.60 2.25] :ride 0.66
    :wheel {:r 0.36 :w 0.26 :track 0.84 :base 1.55}
    :parts [{:at [0.0 0.52 0.70] :size [1.52 0.62 1.50] :tint :glass}
            {:at [-0.82 0.34 -0.85] :size [0.10 0.44 2.00] :tint :body}
            {:at [0.82 0.34 -0.85] :size [0.10 0.44 2.00] :tint :body}]}
   {:name :lorry
    :half [1.05 1.20 3.30] :ride 1.28
    :wheel {:r 0.46 :w 0.32 :track 0.98 :base 2.20}
    :parts [{:at [0.0 0.60 2.40] :size [2.00 1.30 1.40] :tint :glass}
            {:at [0.0 0.40 -0.90] :size [2.16 2.20 4.60] :tint :body}]}])

;; Most of what is on a road is a car. One vehicle in six being a van and one in
;; twelve a lorry is roughly a city street; drawing the type uniformly makes
;; every junction look like a depot.
(def ^:private type-mix [0 0 1 0 1 0 3 2 0 1 0 4])

(deftype Car [body collider handle key idx colour type ti meshes slots
              ^:mutable from ^:mutable to ^:mutable t ^:mutable speed
              ^:mutable leg ^:mutable alive? ^:mutable rnd ^:mutable ekey
              ^:mutable dist]
  ;; A record would allocate a new one of these per car per tick. Traffic is the
  ;; one place in the client where state is genuinely mutated in place, and
  ;; deftype fields are munged by the ClojureScript compiler rather than left to
  ;; Closure to guess at -- which a plain #js object is not.
  Object
  (toString [_] (str "Car " idx " " from "->" to)))

(def ^:private box-slots 2600)     ; bodies and bodywork
(def ^:private wheel-slots 3200)

(defn- type-rig
  "One vehicle as a rig: hull, bodywork, four wheels.

  The wheels are `spin?` parts, which turn continuously rather than leaning
  back and forth the way a pedestrian's leg does. Everything else is rigid and
  its transform is built once, here, rather than sixty times a second."
  [{:keys [half parts wheel]}]
  (let [[hx hy hz] half
        {wr :r ww :w track :track base :base} wheel
        y (+ (- hy) (* 0.35 wr))]
    (fig/rig
     (concat
      [{:shape :box :at [0.0 0.0 0.0] :size [(* 2 hx) (* 2 hy) (* 2 hz)]}]
      (for [{:keys [at size]} parts] {:shape :box :at at :size size})
      (for [j (range 4)]
        {:shape :wheel :spin? true
         :at [(if (even? j) (- track) track) y (if (< j 2) base (- base))]
         :size [ww (* 2 wr) (* 2 wr)]})))))

(defn create [world scene seed ov]
  (let [material (three/MeshPhongMaterial. #js {:color 0xffffff :shininess 26
                                                :flatShading true})
        ;; Two pools for the whole road network. It was one InstancedMesh per
        ;; chunk before, which was already cheap -- the reason to change is that
        ;; a car is now six pieces, and per-chunk meshes would have made that
        ;; six meshes a chunk.
        pools {:box (fig/pool scene (three/BoxGeometry. 1 1 1) material
                              box-slots {:receive? true})
               ;; Axis along X, so the rotation the rig applies about X spins
               ;; the wheel rather than tipping it over.
               :wheel (fig/pool scene
                                (doto (three/CylinderGeometry. 0.5 0.5 1 10)
                                  (.rotateZ (/ js/Math.PI 2)))
                                material wheel-slots {})}
        rigs (mapv type-rig types)]
    (atom {:world world :scene scene :seed seed :overlay ov
         :pools pools
         :rigs rigs
         :rig-meshes (mapv (fn [r] (into-array (map #(:mesh (get pools (:shape %)))
                                                    (:parts r))))
                           rigs)
         :body-m (three/Matrix4.)
         :local-m (three/Matrix4.)
         :out-m (three/Matrix4.)
         :qpos (three/Vector3.)
         :quat (three/Quaternion.)
         :one (three/Vector3. 1 1 1)
         ;; One placement object for the whole simulation. `place!` writes into
         ;; it rather than returning, so driving allocates nothing per tick.
         :place #js {:x 0.0 :y 0.0 :z 0.0 :h 0.0}
         :chunks {}          ; [cx cz] -> {:cars}
         :by-collider {}     ; handle -> car
         :wrecked 0})))

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
  offset into its lane, `ride` metres above the carriageway. Writes rather than
  returns: this runs per car per tick and the object would otherwise be garbage
  every time."
  [^js out ^Leg lg t ride]
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

(defn- claim-slots!
  "One box slot for the body, one for each piece of bodywork, four wheels."
  [pools type]
  (let [nb (inc (count (:parts type)))
        slots (js/Int32Array. (+ nb 4))]
    (dotimes [i nb] (aset slots i (fig/claim! (:box pools))))
    (dotimes [i 4] (aset slots (+ nb i) (fig/claim! (:wheel pools))))
    slots))

(defn- spawn-one! [ts key idx from to t0 speed]
  (let [{:keys [^js world seed pools]} @ts
        ;; Which vehicle, drawn from a fixed mix rather than uniformly: a road
        ;; where a fifth of the traffic is lorries is a depot, not a street.
        ti (nth type-mix (mod (+ idx (* 3 (nth from 0)) (* 5 (nth from 1)))
                              (count type-mix)))
        type (nth types ti)
        lg (leg seed from to)
        p (place! #js {:x 0.0 :y 0.0 :z 0.0 :h 0.0} lg t0 (:ride type))
        [hx hy hz] (:half type)
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
                      body)
        colour (nth colours (mod (+ idx (nth from 0) (nth from 1)) (count colours)))
        slots (claim-slots! pools type)
        meshes (nth (:rig-meshes @ts) ti)]
    ;; Paint is written once. A car's colour does not change until it is a wreck.
    (let [nb (inc (count (:parts type)))]
      (fig/set-colour! (:box pools) (aget slots 0) colour)
      (dotimes [i (count (:parts type))]
        (fig/set-colour! (:box pools) (aget slots (inc i))
                         (case (:tint (nth (:parts type) i))
                           :glass glass
                           colour)))
      (dotimes [i 4] (fig/set-colour! (:wheel pools) (aget slots (+ nb i)) rubber)))
    (->Car body collider (.-handle collider) key idx colour type ti meshes slots
           from to t0 speed lg true
           (js/Math.abs (js/Math.sin (+ (* 12.9898 idx) (* 0.017 (nth from 0)))))
           (edge-key from to) 0.0)))

(defn add-chunk! [ts key arr]
  (when (and arr (pos? (.-length arr)))
    (let [{:keys [overlay]} @ts
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
                              (aget arr (+ o 4)) (aget arr (+ o 5)))))]
      (swap! ts (fn [s]
                  (-> s
                      (assoc-in [:chunks key] {:cars cars})
                      (update :by-collider into
                              (map (fn [c] [(.-handle c) c]) cars)))))
      cars)))

(defn remove-chunk! [ts key]
  (let [{:keys [^js world chunks pools]} @ts]
    (when-let [{:keys [cars]} (get chunks key)]
      (doseq [c0 cars]
        (let [^Car c c0
              nb (inc (count (:parts (.-type c))))
              ^js slots (.-slots c)]
          (dotimes [i nb] (fig/release! (:box pools) (aget slots i)))
          (dotimes [i 4] (fig/release! (:wheel pools) (aget slots (+ nb i))))
          (.removeRigidBody world ^js (.-body c))))
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
        ;; Distance travelled, kept because the wheels are drawn turning and
        ;; `t` restarts at every junction.
        (set! (.-dist c) (+ (.-dist c) (* (.-speed c) dt)))
        (let [p (place! place (.-leg c) (.-t c) (:ride (.-type c)))
              ^js body (.-body c)]
          (.setNextKinematicTranslation body p)
          (.setNextKinematicRotation body
                                     #js {:x 0.0 :y (js/Math.sin (* 0.5 (.-h p)))
                                          :z 0.0 :w (js/Math.cos (* 0.5 (.-h p)))}))))))

(defn sync!
  "Place every car's body, bodywork and wheels.

  Wrecks are read from the body like anything else -- once a car is debris the
  physics is the only thing that knows where it is, and its wheels stop turning
  because it has stopped covering ground."
  [ts]
  (let [{:keys [chunks pools rigs ^js body-m ^js local-m ^js out-m
                ^js qpos ^js quat ^js one]} @ts]
    (doseq [[_ {:keys [cars]}] chunks]
      (dotimes [i (alength cars)]
        (let [^Car c (aget cars i)
              rig (nth rigs (.-ti c))]
          (fig/body-matrix! body-m qpos quat one ^js (.-body c))
          ;; The wheels are told how far the car has come, not how long it has
          ;; been going: a car held at a red light stands with its wheels still.
          (fig/place-rig! rig (.-meshes c) (.-slots c) body-m local-m out-m
                          (/ (.-dist c) (:r (:wheel (.-type c))))))))
    (fig/flush! (:box pools))
    (fig/flush! (:wheel pools))))

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
     :wrecked (:wrecked @ts)
     :parts (+ (fig/used (:box (:pools @ts))) (fig/used (:wheel (:pools @ts))))}))
