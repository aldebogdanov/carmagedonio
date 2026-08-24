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
;; M1 is where these get tuned until driving is fun. They are gathered here,
;; named, and deliberately not spread through the code.

(def chassis-half [0.9 0.30 1.9])
(def ^:private chassis-density 292.0)   ; ~1200 kg for the box above

;; Forward is -Z, so the front axle sits at negative z.
(def wheel-connections
  [[-0.85 -0.15 -1.35]     ; 0 front left
   [ 0.85 -0.15 -1.35]     ; 1 front right
   [-0.85 -0.15  1.35]     ; 2 rear left
   [ 0.85 -0.15  1.35]])   ; 3 rear right

(def wheel-radius 0.35)

(def layout
  {:connections wheel-connections
   :radius      wheel-radius
   :steered     #{0 1}
   :driven      #{2 3}})

(def tuning
  "Live-adjustable so the testbed can sweep it and a tuning overlay can drive it
  from the browser. Read fresh every tick, so a change takes effect immediately.

  All SI. The magic-formula B/C/E constants shape the tyre curves: B is
  stiffness (how fast force builds with slip), C the peak's shape, E how sharply
  force falls away past the peak. Lower `grip` for a loose surface."
  (atom
   {;; suspension
    :suspension-rest      0.32
    :spring-rate          34000.0   ; N/m -- ~8.5 cm static sag at 1200 kg
    :damper-compression   3000.0    ; N.s/m
    :damper-rebound       4200.0
    :max-load             20000.0   ; N, clamp against solver spikes
    :nominal-load         2950.0    ; N, static load on one corner

    ;; tyre
    :grip                 1.60      ; peak mu; sweeps show slides sustain best here
    ;; <1.0 makes the rear let go first. Measured: it tightens turn-in but
    ;; shortens slides and costs acceleration, because the rear wheels are the
    ;; driven ones. Left neutral; it is a lever, not a default.
    :grip-rear-bias       1.0
    :load-sensitivity     0.25      ; grip lost as a tyre is loaded past nominal
    :lat-B  9.0   :lat-C  1.60  :lat-E  0.92   ; peak near 7.5 deg slip angle
    :long-B 11.0  :long-C 1.65  :long-E 0.90   ; peak near 12% slip ratio

    ;; drivetrain
    :engine-torque        1200.0    ; N.m per driven wheel
    :brake-torque         1800.0    ; N.m per wheel -- enough to lock
    :handbrake-torque     2600.0
    :wheel-inertia        1.4       ; kg.m^2
    :rolling-resistance   0.6

    ;; steering
    :max-steer            0.55      ; radians at standstill
    :steer-speed-falloff  0.016     ; authority lost per m/s
    :steer-rate           6.0}))    ; radians/second of input travel

;; --- construction -----------------------------------------------------------

(defn- vec3 [x y z] #js {:x x :y y :z z})

(defn spawn-box!
  "Add a dynamic box. Half-extents in metres. Returns the entity index."
  [sim [x y z] [hx hy hz] {:keys [density friction can-sleep events?]
                           :or   {density 200.0 friction 0.8 can-sleep true
                                  events? false}}]
  (let [{:keys [^js world bodies halves]} @sim
        i        (count halves)
        ^js body (.createRigidBody
                  world
                  (-> (.dynamic RAPIER/RigidBodyDesc)
                      (.setTranslation x y z)
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
  [sim i]
  (let [{:keys [world bodies]} @sim
        v (vehicle/create world (aget bodies i) layout tuning)]
    (swap! sim update :vehicles (fnil conj []) v)
    v))

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
  ([{:keys [flat? seed opponents] :or {seed 0 opponents 0}}]
   (let [[gx gy gz] k/gravity
         ^js world (RAPIER/World. (vec3 gx gy gz))
         [sx sy sz] (if flat? [0.0 1.2 0.0] (worldgen/spawn-point seed))
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
                     :player 0
                     :vehicles []
                     :tick   0})]
     (set! (.-timestep world) k/dt)
     (when flat?
       (.createCollider world (-> (.cuboid RAPIER/ColliderDesc 400.0 0.5 400.0)
                                  (.setTranslation 0.0 -0.5 0.0)
                                  (.setFriction 1.0))))
     ;; Player must be entity 0.
     (spawn-box! sim [sx sy sz] chassis-half
                 {:density chassis-density :can-sleep false :events? true})
     (build-vehicle! sim 0)
     ;; Opponents, spread along the road behind the player.
     (dotimes [i opponents]
       (let [a  (* (+ i 1) 2.3)
             ox (+ sx (* 9.0 (js/Math.cos a)))
             oz (+ sz (* 9.0 (js/Math.sin a)))
             oy (if flat? 1.2 (+ 1.2 (worldgen/height-at seed ox oz)))]
         (spawn-box! sim [ox oy oz] chassis-half
                     {:density chassis-density :can-sleep false :events? true})
         (build-vehicle! sim (inc i))))
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
    (let [steered (:steered layout)]
      (dotimes [v (count vehicles)]
        (let [{:keys [susp steer spin]} (nth vehicles v)
              base (* v 4 wheel-stride)]
          (dotimes [i 4]
            (let [o (+ base (* i wheel-stride))]
              (aset wheels (+ o 0) (aget susp i))
              (aset wheels (+ o 1) (if (steered i) (aget steer 0) 0.0))
              (aset wheels (+ o 2) (aget spin i)))))))))

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

(defn player-speed
  "Signed metres per second along the vehicle's forward axis."
  [sim]
  (vehicle/forward-speed (player-vehicle sim)))

(defn chassis-body ^js [sim] (aget (:bodies @sim) (:player @sim)))

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

(defn chassis-collider-handle [sim]
  (let [^js b (chassis-body sim)]
    (.-handle (.collider b 0))))

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
