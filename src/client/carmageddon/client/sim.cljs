(ns carmageddon.client.sim
  "Fixed-step physics simulation. Knows nothing about rendering.

  Entity transforms live in flat Float32Arrays rather than Clojure maps: this is
  the hot path, and persistent data structures here would allocate on the order
  of 60 * entities objects per second and stutter under GC. Immutable data is
  for config and world metadata, not per-tick state.

  `prev` and `curr` hold the last two simulated transforms so the renderer can
  interpolate between them. Render rate and sim rate are deliberately unrelated.

  The player is a raycast vehicle: a single box chassis plus four wheel rays
  with spring/damper suspension. This is the standard robust approach and gives
  weight transfer and rollovers for free -- pushing a chassis box along the
  ground instead does not work, because a 1.2 t block under realistic friction
  simply resists any force an engine could plausibly apply."
  (:require ["@dimforge/rapier3d-compat" :as RAPIER]
            [carmageddon.client.cars :as cars]
            [carmageddon.client.vehicle :as vehicle]
            [carmageddon.shared.worldgen :as worldgen]
            [carmageddon.shared.constants :as k]))

(def ^:const stride 7)              ; x y z  qx qy qz qw
(def ^:const wheel-stride 3)        ; suspension-length  steer  spin
(def ^:const max-entities 256)
(def ^:const max-vehicles 8)

(defn init!
  "Load the Rapier wasm module. Returns a promise."
  []
  (.init RAPIER))

;; --- vehicle configuration --------------------------------------------------
;;
;; What a vehicle *is* lives in `carmageddon.client.cars`: box, mass, wheels,
;; gearing, bodywork. This namespace only knows that a vehicle has a layout and
;; a tuning atom, which is what keeps a tractor and a muscle car running through
;; exactly the same tyre model.

(def chassis-half
  "The reference car's box. Remote proxies are drawn with it until the wire
  carries a vehicle kind."
  (cars/half cars/default-kind))

;; --- construction -----------------------------------------------------------

(defn- vec3 [x y z] #js {:x x :y y :z z})

(defn spawn-box!
  "Add a dynamic box. Half-extents in metres. Returns the entity index."
  [sim [x y z] [hx hy hz] {:keys [density friction can-sleep events? yaw]
                           :or   {density 200.0 friction 0.8 can-sleep true
                                  events? false yaw 0.0}}]
  (let [{:keys [^js world bodies halves]} @sim
        i        (count halves)
        ^js body (.createRigidBody
                  world
                  (-> (.dynamic RAPIER/RigidBodyDesc)
                      (.setTranslation x y z)
                      (.setRotation #js {:x 0.0 :y (js/Math.sin (* 0.5 yaw))
                                         :z 0.0 :w (js/Math.cos (* 0.5 yaw))})
                      (.setCanSleep can-sleep)
                      (.setLinearDamping 0.05)
                      (.setAngularDamping 0.4)))]
    (.createCollider world
                     (cond-> (-> (.cuboid RAPIER/ColliderDesc hx hy hz)
                                 (.setDensity density)
                                 (.setFriction friction)
                                 (.setRestitution 0.1))
                       events? (-> (.setActiveEvents (.-CONTACT_FORCE_EVENTS RAPIER/ActiveEvents))
                                   (.setContactForceEventThreshold 1000.0)))
                     body)
    (aset bodies i body)
    (swap! sim update :halves conj [hx hy hz])
    i))

(defn- build-vehicle!
  "Attach a vehicle to the chassis at entity index `i`. Index 0 is the player;
  the rest are opponents, and they are identical in every other respect -- same
  tyre model, same tuning, same damage. An opponent that handled differently
  would be a different game."
  [sim i kind]
  (let [{:keys [world bodies ^js by-collider]} @sim
        ^js body (aget bodies i)
        v (vehicle/create world body (cars/layout kind) (cars/tuning kind))]
    (swap! sim update :vehicles (fnil conj []) v)
    ;; Contact events arrive as collider handles. Resolving one back to a
    ;; vehicle used to mean comparing against the player's handle and nothing
    ;; else, which is why an opponent could be driven into a wall all day
    ;; without ever being damaged.
    (.set by-collider (.-handle (.collider body 0)) (dec (count (:vehicles @sim))))
    (swap! sim update :kinds conj kind)
    v))

(defn kind-of
  "Which catalogue entry vehicle `i` is. Presentation needs this to know what
  shape to draw; the simulation never asks."
  [sim i]
  (nth (:kinds @sim) i))

(defn vehicle-of-collider
  "Which vehicle that collider belongs to, or nil for scenery."
  [sim handle]
  (let [^js m (:by-collider @sim)
        v (.get m handle)]
    (when-not (undefined? v) v)))

(defn- spawn-lift
  "How far above the nominal spawn height a vehicle's chassis centre has to
  start. The spawn is quoted for the reference car; a truck dropped at that
  height starts with its axles underground and is fired into the air by the
  solver on the first tick."
  [kind]
  (let [[_ hy _] (cars/half kind)
        {:keys [rear-radius radius]} (:wheels (cars/spec kind))]
    (max 0.0 (- (+ hy (or rear-radius radius)) 0.65))))

(defn vehicles [sim] (:vehicles @sim))
(defn opponent-count [sim] (max 0 (dec (count (:vehicles @sim)))))

(defn create!
  "Build the physics world, the player vehicle, and optional debris.

  There is no ground here any more: terrain arrives as streamed heightfield
  colliders from `carmageddon.client.chunks`, so the world starts empty and the
  caller loads the chunks around the spawn before the first tick.

  Smashable scenery comes from `carmageddon.client.props`, spawned per chunk.
  `:flat?` gives the measurement harness a bare slab instead, because a harness
  that measured braking on procedurally rolling terrain would be measuring the
  hill, not the tyres."
  ([] (create! {}))
  ([{:keys [flat? seed opponents kind rival-kinds]
     :or   {seed 0 opponents 0 kind cars/default-kind}}]
   (let [[gx gy gz] k/gravity
         ^js world (RAPIER/World. (vec3 gx gy gz))
         spawn      (when-not flat? (worldgen/spawn-point seed))
         [sx sy sz] (if flat? [0.0 1.2 0.0] (:pos spawn))
         [fdx fdz]  (if flat? [0.0 -1.0] (:dir spawn))
         ;; Local forward is -Z, so this is the yaw that points the car down the
         ;; street. Without it the field starts broadside to the road, which
         ;; only became obvious once there were buildings to be broadside into.
         spawn-yaw  (js/Math.atan2 (- fdx) (- fdz))
         sim  (atom {:world  world
                     :seed   seed
                     :bodies (make-array max-entities)
                     :halves []
                     :prev   (js/Float32Array. (* max-entities stride))
                     :curr   (js/Float32Array. (* max-entities stride))
                     ;; Wheel state for every vehicle, not just the player:
                     ;; opponents with static wheels read as sliding boxes.
                     :wheels (js/Float32Array. (* max-vehicles 4 wheel-stride))
                     :spawn  [sx sy sz]
                     :events (RAPIER/EventQueue. true)
                     :on-impact nil
                     ;; collider handle -> vehicle index
                     :by-collider (js/Map.)
                     :player 0
                     :vehicles []
                     :kinds  []
                     :tick   0})
         ;; Rivals cycle through the catalogue rather than picking at random:
         ;; three cars of the same kind is a duller field than three different
         ;; ones, and a random draw produces that a fifth of the time.
         rivals (or rival-kinds
                    (mapv #(nth cars/kinds (mod (inc %) (count cars/kinds)))
                          (range opponents)))]
     (set! (.-timestep world) k/dt)
     (when flat?
       ;; Twenty kilometres of it. The harness holds full throttle for a minute
       ;; to find a top speed, and on the 800 m plate this used to have was off
       ;; the edge and in free fall long before then -- which the measurement
       ;; then reported as the top speed, because a tumbling car has plenty of
       ;; velocity along its own nose.
       (.createCollider world (-> (.cuboid RAPIER/ColliderDesc 20000.0 0.5 20000.0)
                                  (.setTranslation 0.0 -0.5 0.0)
                                  (.setFriction 1.0))))
     ;; Player must be entity 0.
     (spawn-box! sim [sx (+ sy (spawn-lift kind)) sz] (cars/half kind)
                 {:density (cars/density kind) :can-sleep false :events? true
                  :yaw spawn-yaw})
     (build-vehicle! sim 0 kind)
     ;; Opponents queue up along the carriageway behind the player, two abreast.
     ;; They used to be scattered round a 9 m circle, which was fine when the
     ;; only scenery was the odd crate but drops half the field inside a
     ;; building now that blocks are built out to the pavement.
     (dotimes [i opponents]
       (let [rk   (nth rivals i)
             ;; A lorry is seven metres long. Spacing the queue by a fixed
             ;; eight put the truck's nose through the car in front of it.
             back (reduce + 4.0 (map #(+ 3.0 (* 2.0 (nth (cars/half (nth rivals %)) 2)))
                                     (range (inc i))))
             lat  (if (even? i) 2.4 -2.4)
             ox (+ sx (* (- fdx) back) (* (- fdz) lat))
             oz (+ sz (* (- fdz) back) (* fdx lat))
             oy (+ (if flat? 1.2 (+ 1.2 (worldgen/height-at seed ox oz)))
                   (spawn-lift rk))]
         (spawn-box! sim [ox oy oz] (cars/half rk)
                     {:density (cars/density rk) :can-sleep false :events? true
                      :yaw spawn-yaw})
         (build-vehicle! sim (inc i) rk)))
     sim)))

(defn entity-count [sim] (count (:halves @sim)))

;; --- stepping ---------------------------------------------------------------

(defn- capture!
  "Roll curr -> prev, then read every body transform plus the wheel state that
  the renderer needs."
  [sim]
  (let [{:keys [bodies prev curr wheels vehicles halves]} @sim
        n (count halves)]
    (.set prev curr)
    (dotimes [i n]
      (let [^js b (aget bodies i)
            t (.translation b)
            r (.rotation b)
            o (* i stride)]
        (aset curr (+ o 0) (.-x t))
        (aset curr (+ o 1) (.-y t))
        (aset curr (+ o 2) (.-z t))
        (aset curr (+ o 3) (.-x r))
        (aset curr (+ o 4) (.-y r))
        (aset curr (+ o 5) (.-z r))
        (aset curr (+ o 6) (.-w r))))
    (dotimes [v (count vehicles)]
      (let [{:keys [susp steer spin layout]} (nth vehicles v)
            steered (:steered layout)
            base (* v 4 wheel-stride)]
        (dotimes [i 4]
          (let [o (+ base (* i wheel-stride))]
            (aset wheels (+ o 0) (aget susp i))
            (aset wheels (+ o 1) (if (steered i) (aget steer 0) 0.0))
            (aset wheels (+ o 2) (aget spin i))))))))

(defn- reset-player! [sim]
  (let [^js body (aget (:bodies @sim) 0)
        [x y z] (:spawn @sim)]
    (.setTranslation body (vec3 x y z) true)
    (.setRotation body #js {:x 0.0 :y 0.0 :z 0.0 :w 1.0} true)
    (.setLinvel body (vec3 0 0 0) true)
    (.setAngvel body (vec3 0 0 0) true)
    (.resetForces body true)
    (.resetTorques body true)
    (vehicle/reset-state! (first (:vehicles @sim)))))

(defn step!
  "Advance exactly one fixed tick.

  `cmd` drives the player. `opponent-cmds`, if given, drives the rest in order;
  they are ordinary Commands, indistinguishable from keyboard input."
  ([sim cmd] (step! sim cmd nil))
  ([sim cmd opponent-cmds]
  (capture! sim)
  (if (:reset cmd)
    (reset-player! sim)
    ;; Wheel rays must be cast against the world as it stands *before* the
    ;; solver runs, and the forces they add are consumed by that step, so this
    ;; ordering is required rather than incidental.
    (vehicle/update! (first (:vehicles @sim)) cmd k/dt))
  (let [vs (:vehicles @sim)]
    (dotimes [i (min (count opponent-cmds) (dec (count vs)))]
      (vehicle/update! (nth vs (inc i)) (nth opponent-cmds i) k/dt)))
  (let [{:keys [^js world ^js events on-impact]} @sim]
    (.step world events)
    ;; Contact-force events are drained immediately after the step that produced
    ;; them: the queue auto-drains, so anything left unread is lost.
    (when on-impact
      (.drainContactForceEvents
       events
       (fn [^js ev]
         (on-impact (.collider1 ev) (.collider2 ev) (.totalForceMagnitude ev))))))
  (swap! sim assoc :tick (:tick cmd))))

(defn player-vehicle [sim] (first (:vehicles @sim)))

(defn vehicle-body ^js [sim i] (aget (:bodies @sim) i))

(defn place-vehicle!
  "Put vehicle `i` down at `[x y z]` facing `yaw`, stationary.

  Damage is deliberately kept: a rival that has been leashed back to the player
  is the same car that was just being shot at, not a new one."
  [sim i [x y z] yaw]
  (let [^js body (aget (:bodies @sim) i)]
    (.setTranslation body (vec3 x y z) true)
    (.setRotation body #js {:x 0.0 :y (js/Math.sin (* 0.5 yaw))
                            :z 0.0 :w (js/Math.cos (* 0.5 yaw))} true)
    (.setLinvel body (vec3 0 0 0) true)
    (.setAngvel body (vec3 0 0 0) true)
    (.resetForces body true)
    (.resetTorques body true)
    (vehicle/clear-motion! (nth (:vehicles @sim) i))))

(defn player-speed
  "Signed metres per second along the vehicle's forward axis."
  [sim]
  (vehicle/forward-speed (player-vehicle sim)))

(defn chassis-body ^js [sim] (aget (:bodies @sim) (:player @sim)))

(defn cast-ray
  "Distance to the first solid thing along a ray, or nil.

  `dx dy dz` must be a unit vector: Rapier reports a time of impact, which is
  only a distance when the direction has unit length. The player's own body is
  excluded, so a camera using this does not collide with the car it follows."
  [sim ox oy oz dx dy dz max-dist]
  (let [^js world (:world @sim)
        ^js body  (chassis-body sim)
        ray       (RAPIER/Ray. #js {:x ox :y oy :z oz} #js {:x dx :y dy :z dz})
        ^js hit   (.castRay world ray max-dist true
                            js/undefined js/undefined js/undefined body)]
    (when hit (.-timeOfImpact hit))))

(defn- v3 [^js v] [(.-x v) (.-y v) (.-z v)])

(defn forward-vector
  "Unit vector the car is pointing along, in world space."
  [sim]
  (let [^js b (chassis-body sim)
        q     (.rotation b)
        qx (.-x q) qy (.-y q) qz (.-z q) qw (.-w q)]
    ;; local (0,0,-1) rotated by q
    [(* -2.0 (+ (* qx qz) (* qw qy)))
     (* -2.0 (- (* qy qz) (* qw qx)))
     (- (* 2.0 (+ (* qx qx) (* qy qy))) 1.0)]))

(defn telemetry
  "Everything the measurement harness and the tuning overlay need. Allocates a
  map, so this is called at HUD rate or from the testbed -- never per tick."
  [sim]
  (let [{:keys [susp fz fx fy slip-a slip-r contact steer] :as v} (player-vehicle sim)
        ^js b (chassis-body sim)]
    {:pos     (v3 (.translation b))
     :vel     (v3 (.linvel b))
     :angvel  (v3 (.angvel b))
     :forward (forward-vector sim)
     :speed   (vehicle/forward-speed v)
     :steer   (aget steer 0)
     :suspension  (vec (array-seq susp))
     :load        (vec (array-seq fz))
     :force-long  (vec (array-seq fx))
     :force-lat   (vec (array-seq fy))
     :slip-angle  (vec (array-seq slip-a))
     :slip-ratio  (vec (array-seq slip-r))
     :contact     (mapv pos? (array-seq contact))}))

(defn damage [sim] (vehicle/damage (player-vehicle sim)))

(defn player-x [sim] (let [^js b (chassis-body sim)] (.-x (.translation b))))
(defn player-z [sim] (let [^js b (chassis-body sim)] (.-z (.translation b))))

(defn sideslip-deg
  "Angle between where the car is pointing and where it is actually going.
  Near zero is gripping; large and steady is a drift; large and growing is a
  spin. Takes a telemetry map so callers pay for it once."
  [{[vx _ vz] :vel [fx _ fz] :forward}]
  (let [sp (js/Math.hypot vx vz)]
    (if (< sp 1.0)
      0.0
      (* (/ 180.0 js/Math.PI)
         (js/Math.atan2 (- (* fx vz) (* fz vx))
                        (+ (* fx vx) (* fz vz)))))))

(defn wheels-on-ground [sim]
  (vehicle/wheels-on-ground (player-vehicle sim)))
