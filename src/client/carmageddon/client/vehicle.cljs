(ns carmageddon.client.vehicle
  "Raycast vehicle with a slip-based tyre model.

  Replaces Rapier's built-in DynamicRayCastVehicleController, which measurement
  showed cannot produce a controllable slide: its friction is a hard clamp, so
  grip is either fully present or fully gone. A real tyre's force rises to a
  peak at a small slip and then *falls away*, and it is that falloff region --
  past the peak, still producing force, but less of it -- where drifting lives.

  It also fixes a second problem: in the built-in controller, braking force was
  not limited by grip at all, so the car stopped from 100 km/h in the same
  distance on ice as on tarmac.

  Model, per wheel, per tick:
    1. cast a ray down from the chassis connection point
    2. spring/damper along the contact normal gives the vertical load Fz
    3. slip ratio (longitudinal) and slip angle (lateral) from the contact
       patch velocity
    4. a Magic-Formula-shaped curve turns each slip into a force
    5. a friction ellipse makes the two share one budget of mu * Fz
    6. the tyre's reaction torque feeds back into wheel spin, so wheels can
       lock under braking and light up under power

  Everything is in SI units. Chassis-local -Z is forward, +Y is up."
  (:require ["@dimforge/rapier3d-compat" :as RAPIER]
            [carmageddon.shared.constants :as k]))

;; Scratch vectors. The sim is single-threaded and vehicles step one at a time,
;; so module-level scratch is safe and keeps the tick allocation-free.
(defn- v3 [] (js/Float64Array. 3))
(def ^:private s-fwd (v3))
(def ^:private s-right (v3))
(def ^:private s-up (v3))
(def ^:private s-origin (v3))
(def ^:private s-contact (v3))
(def ^:private s-vel (v3))
(def ^:private s-normal (v3))

(defn- qrot!
  "Rotate (x,y,z) by quaternion q into `out`."
  [^js out ^js q x y z]
  (let [qx (.-x q) qy (.-y q) qz (.-z q) qw (.-w q)
        tx (* 2.0 (- (* qy z) (* qz y)))
        ty (* 2.0 (- (* qz x) (* qx z)))
        tz (* 2.0 (- (* qx y) (* qy x)))]
    (aset out 0 (+ x (* qw tx) (- (* qy tz) (* qz ty))))
    (aset out 1 (+ y (* qw ty) (- (* qz tx) (* qx tz))))
    (aset out 2 (+ z (* qw tz) (- (* qx ty) (* qy tx))))
    out))

(defn- dot3 [^js a ^js b]
  (+ (* (aget a 0) (aget b 0)) (* (aget a 1) (aget b 1)) (* (aget a 2) (aget b 2))))

(defn- project-onto-plane!
  "Remove the component of `v` along `n`, then renormalise. Keeps the tyre basis
  flat against whatever surface was actually hit, so slopes behave."
  [^js v ^js n]
  (let [d (dot3 v n)
        x (- (aget v 0) (* d (aget n 0)))
        y (- (aget v 1) (* d (aget n 1)))
        z (- (aget v 2) (* d (aget n 2)))
        len (js/Math.hypot x y z)]
    (if (< len 1e-6)
      v
      (doto v (aset 0 (/ x len)) (aset 1 (/ y len)) (aset 2 (/ z len))))))

(defn- magic
  "Pacejka-shaped normalised force curve. Rises to ~1.0 at the peak slip, then
  decays -- that decay is the whole point."
  [s B C E]
  (let [bs (* B s)]
    (js/Math.sin (* C (js/Math.atan (- bs (* E (- bs (js/Math.atan bs)))))))))

(defn- hit-distance [^js hit]
  (let [t (.-timeOfImpact hit)]
    (if (undefined? t) (.-toi hit) t)))

;; --- construction -----------------------------------------------------------

(defn create
  "Attach a vehicle to an existing chassis rigid body."
  [world ^js body layout tuning]
  {:world     world
   :body      body
   :layout    layout          ; {:connections [[x y z] x4] :radius r :steered #{} :driven #{}}
   :tuning    tuning          ; atom
   :omega     (js/Float64Array. 4)
   :susp      (doto (js/Float64Array. 4) (.fill (:suspension-rest @tuning)))
   :susp-prev (doto (js/Float64Array. 4) (.fill (:suspension-rest @tuning)))
   :fz        (js/Float64Array. 4)
   :fx        (js/Float64Array. 4)
   :fy        (js/Float64Array. 4)
   :slip-a    (js/Float64Array. 4)
   :slip-r    (js/Float64Array. 4)
   :contact   (js/Uint8Array. 4)
   :spin      (js/Float64Array. 4)
   :steer     (js/Float64Array. 1)
   ;; 0 = pristine, 1 = wrecked. Held here rather than in the sim so the vehicle
   ;; model is self-contained and an AI or remote car carries its own.
   :damage    (js/Float64Array. 1)})

(defn damage [{:keys [^js damage]}] (aget damage 0))

(defn add-damage!
  "Accumulate impact damage, saturating at 1."
  [{:keys [^js damage]} amount]
  (aset damage 0 (min 1.0 (+ (aget damage 0) amount))))

(defn forward-speed
  "Signed metres per second along the chassis' forward axis."
  [{:keys [^js body]}]
  (let [^js lv (.linvel body)]
    (qrot! s-fwd (.rotation body) 0.0 0.0 -1.0)
    (+ (* (.-x lv) (aget s-fwd 0))
       (* (.-y lv) (aget s-fwd 1))
       (* (.-z lv) (aget s-fwd 2)))))

(def ^:private creep 0.8)          ; m/s below which the car counts as stopped
(def ^:private reverse-torque 0.55) ; reverse is geared lower than first
(def ^:private reverse-max 11.0)    ; and runs out of gearing sooner, m/s

(defn- gear-for
  "Resolve the two pedals into a signed drive and a brake.

  The car has a reverse gear, chosen the way an automatic chooses one: from how
  fast it is already going, not from a separate control. Above walking pace the
  brake pedal brakes; at rest it drives backwards; and once rolling backwards
  the two pedals swap, so the throttle is what stops you.

  This did not exist. The comment where reverse was supposed to happen claimed
  that `the tyre model runs the wheels backwards under sustained braking from a
  standstill`, but brake torque is clamped at zero angular velocity and never
  takes it past -- so holding the brake against a wall did nothing at all, for
  the player and for the AI backing out of whatever it had driven into."
  [veh {:keys [throttle brake] :as cmd}]
  (let [v    (forward-speed veh)
        top  (:top-speed @(:tuning veh) 62.0)
        ;; Gearing. Drive torque fades out as the vehicle approaches what its
        ;; drivetrain is geared for, cubically so that it is barely felt until
        ;; the last third. Without it a lorry and a hot rod converge on the same
        ;; terminal speed, because past the tyres the only things resisting are
        ;; rolling resistance and body damping and neither knows what it is
        ;; pushing.
        gear (let [r (/ (js/Math.abs v) top)]
               (max 0.0 (- 1.0 (* r r r))))
        throttle (* throttle gear)]
    (cond
      ;; Rolling forwards: everything as it was.
      (> v creep)     (assoc cmd :drive throttle :brake brake)
      ;; Rolling backwards: the pedals have swapped.
      (< v (- creep)) (assoc cmd :drive (if (< (- v) reverse-max)
                                          (* (- reverse-torque) brake)
                                          0.0)
                                 :brake throttle)
      ;; Stopped: whichever pedal is down decides which way to go.
      :else           (assoc cmd :drive (- throttle (* reverse-torque brake))
                                 :brake 0.0))))

;; --- per-wheel step ---------------------------------------------------------

(defn- step-wheel!
  [{:keys [^js world ^js body layout tuning omega susp susp-prev fz fx fy
           slip-a slip-r contact spin steer ^js damage]}
   i dt {:keys [drive brake handbrake]}]
  (let [{:keys [connections radii steered driven handbraked]} layout
        radius (nth radii i)
        {:keys [suspension-rest spring-rate damper-compression damper-rebound
                max-load nominal-load grip grip-rear-bias load-sensitivity
                lat-B lat-C lat-E long-B long-C long-E
                engine-torque brake-torque handbrake-torque
                wheel-inertia rolling-resistance]} @tuning
        [cx cy cz] (nth connections i)
        ;; A wrecked car should be a worse car: less power to the wheels and
        ;; less adhesion. Both saturate well short of undriveable, because a
        ;; car you cannot move is not fun, it is just over.
        dmg    (aget damage 0)
        engine-torque (* engine-torque (- 1.0 (* 0.55 dmg)))
        grip          (* grip (- 1.0 (* 0.30 dmg)))
        ^js q  (.rotation body)
        ^js p  (.translation body)
        theta  (if (steered i) (aget steer 0) 0.0)
        ;; Ray from the suspension's top mount, straight down in chassis space.
        _      (qrot! s-origin q cx cy cz)
        _      (qrot! s-up q 0.0 1.0 0.0)
        ox (+ (.-x p) (aget s-origin 0))
        oy (+ (.-y p) (aget s-origin 1))
        oz (+ (.-z p) (aget s-origin 2))
        max-ray (+ suspension-rest radius)
        ray (RAPIER/Ray. #js {:x ox :y oy :z oz}
                         #js {:x (- (aget s-up 0)) :y (- (aget s-up 1)) :z (- (aget s-up 2))})
        ^js hit (.castRayAndGetNormal world ray max-ray true
                                      js/undefined js/undefined js/undefined body)]
    (aset susp-prev i (aget susp i))
    (if (nil? hit)
      ;; Airborne: the wheel droops to full extension and carries no load. It
      ;; keeps spinning, so landing under power lights it up.
      (do
        (aset contact i 0)
        (aset susp i suspension-rest)
        (aset fz i 0.0) (aset fx i 0.0) (aset fy i 0.0)
        (aset slip-a i 0.0) (aset slip-r i 0.0)
        (let [drive (if (driven i) (* engine-torque drive) 0.0)
              w     (+ (aget omega i) (/ (* drive dt) wheel-inertia))
              bt    (+ (* brake-torque brake)
                       (if (and handbrake (handbraked i)) handbrake-torque 0.0))
              dw    (/ (* bt dt) wheel-inertia)
              w'    (cond
                      (<= (js/Math.abs w) dw) 0.0
                      (pos? w) (- w dw)
                      :else (+ w dw))]
          (aset omega i (* w' (- 1.0 (* rolling-resistance dt))))
          (aset spin i (+ (aget spin i) (* (aget omega i) dt)))))

      (let [d       (hit-distance hit)
            len     (max 0.0 (min suspension-rest (- d radius)))
            _       (aset susp i len)
            _       (aset contact i 1)
            ^js hn  (.-normal hit)
            _       (doto s-normal (aset 0 (.-x hn)) (aset 1 (.-y hn)) (aset 2 (.-z hn)))
            ;; --- suspension -------------------------------------------------
            compression (- suspension-rest len)
            susp-vel    (/ (- (aget susp-prev i) len) dt)   ; +ve while compressing
            damper      (* (if (pos? susp-vel) damper-compression damper-rebound) susp-vel)
            load        (max 0.0 (min max-load (+ (* spring-rate compression) damper)))
            _           (aset fz i load)
            ;; --- contact patch velocity -------------------------------------
            cpx (+ ox (* (- (+ len radius)) (aget s-up 0)))
            cpy (+ oy (* (- (+ len radius)) (aget s-up 1)))
            cpz (+ oz (* (- (+ len radius)) (aget s-up 2)))
            ^js com (.worldCom body)
            ^js lv  (.linvel body)
            ^js av  (.angvel body)
            rx (- cpx (.-x com)) ry (- cpy (.-y com)) rz (- cpz (.-z com))
            vx (+ (.-x lv) (- (* (.-y av) rz) (* (.-z av) ry)))
            vy (+ (.-y lv) (- (* (.-z av) rx) (* (.-x av) rz)))
            vz (+ (.-z lv) (- (* (.-x av) ry) (* (.-y av) rx)))
            _  (doto s-vel (aset 0 vx) (aset 1 vy) (aset 2 vz))
            ;; --- tyre basis, flattened onto the surface ---------------------
            _ (qrot! s-fwd q (- (js/Math.sin theta)) 0.0 (- (js/Math.cos theta)))
            _ (qrot! s-right q (js/Math.cos theta) 0.0 (- (js/Math.sin theta)))
            _ (project-onto-plane! s-fwd s-normal)
            _ (project-onto-plane! s-right s-normal)
            v-fwd (dot3 s-vel s-fwd)
            v-lat (dot3 s-vel s-right)
            ;; The +1.0 floor keeps slip finite at a standstill; without it both
            ;; quantities blow up as the car stops and the tyres oscillate.
            vref  (+ (js/Math.abs v-fwd) 1.0)
            alpha (js/Math.atan2 v-lat vref)
            kappa (/ (- (* (aget omega i) radius) v-fwd) vref)
            _ (aset slip-a i alpha)
            _ (aset slip-r i kappa)
            ;; --- forces -----------------------------------------------------
            ;; Load sensitivity: a tyre's grip coefficient falls as it is pushed
            ;; harder, which is what makes the loaded outside wheel in a corner
            ;; less effective than its share of the weight suggests.
            ;; Rear bias below 1.0 makes the back axle let go first, which is
            ;; what turns a corner into an oversteering slide instead of a
            ;; nose-first push. It is the single most effective knob for how
            ;; drifty the car feels.
            axle (if (driven i) grip-rear-bias 1.0)
            mu   (* grip axle (- 1.0 (* load-sensitivity
                                        (- (/ load (max 1.0 nominal-load)) 1.0))))
            mu   (max 0.25 mu)
            cap  (* mu load)
            fx0  (* cap (magic kappa long-B long-C long-E))
            fy0  (* -1.0 cap (magic alpha lat-B lat-C lat-E))
            mag  (js/Math.hypot fx0 fy0)
            scl  (if (> mag cap) (/ cap mag) 1.0)
            fxv  (* fx0 scl)
            fyv  (* fy0 scl)]
        (aset fx i fxv)
        (aset fy i fyv)
        ;; Suspension pushes along the contact normal; tyre forces act in the
        ;; contact plane. Both are applied at the contact point, which is what
        ;; produces roll, pitch and weight transfer without any extra machinery.
        (.addForceAtPoint body
                          #js {:x (* load (aget s-normal 0))
                               :y (* load (aget s-normal 1))
                               :z (* load (aget s-normal 2))}
                          #js {:x cpx :y cpy :z cpz}
                          true)
        (.addForceAtPoint body
                          #js {:x (+ (* fxv (aget s-fwd 0)) (* fyv (aget s-right 0)))
                               :y (+ (* fxv (aget s-fwd 1)) (* fyv (aget s-right 1)))
                               :z (+ (* fxv (aget s-fwd 2)) (* fyv (aget s-right 2)))}
                          #js {:x cpx :y cpy :z cpz}
                          true)
        ;; --- wheel spin ---------------------------------------------------
        (let [drive (if (driven i) (* engine-torque drive) 0.0)
              react (* -1.0 fxv radius)
              w     (+ (aget omega i) (/ (* (+ drive react) dt) wheel-inertia))
              bt    (+ (* brake-torque brake)
                       (if (and handbrake (handbraked i)) handbrake-torque 0.0))
              dw    (/ (* bt dt) wheel-inertia)
              w'    (cond
                      (<= (js/Math.abs w) dw) 0.0
                      (pos? w) (- w dw)
                      :else (+ w dw))]
          (aset omega i (* w' (- 1.0 (* rolling-resistance dt))))
          (aset spin i (+ (aget spin i) (* (aget omega i) dt))))))))

;; --- public -----------------------------------------------------------------

(defn update!
  "Advance steering and apply all four wheels' forces for this tick. Must run
  before `world.step`, because the forces it adds are consumed by that step."
  [{:keys [steer tuning] :as veh} cmd dt]
  (let [{:keys [max-steer steer-speed-falloff steer-rate]} @tuning
        ^js body (:body veh)
        ;; Rapier's addForce accumulator persists across steps until cleared --
        ;; unlike an impulse. Without this the wheel forces from every previous
        ;; tick stay applied and the car launches into orbit within a second.
        _        (.resetForces body true)
        _        (.resetTorques body true)
        ^js lv   (.linvel body)
        speed    (js/Math.hypot (.-x lv) (.-z lv))
        ;; Steering authority falls off with speed, otherwise the car spins the
        ;; moment it has any pace.
        limit    (* max-steer (max 0.25 (- 1.0 (* steer-speed-falloff speed))))
        ;; Negated: a positive rotation about +Y swings a -Z-facing car left,
        ;; and input +1 means "right".
        target   (* limit (- (:steer cmd)))
        cur      (aget steer 0)
        d        (* steer-rate dt max-steer)
        next     (cond (> target (+ cur d)) (+ cur d)
                       (< target (- cur d)) (- cur d)
                       :else target)]
    (aset steer 0 next)
    (dotimes [i 4] (step-wheel! veh i dt (gear-for veh cmd)))))

(defn clear-motion!
  "Everything about how the car is moving, zeroed. Damage is not motion and is
  deliberately left alone -- this is what a car being picked up and put down
  somewhere else needs, and a leashed rival keeps its dents."
  [{:keys [omega susp susp-prev fz fx fy slip-a slip-r contact spin steer tuning]}]
  (let [rest (:suspension-rest @tuning)]
    (doseq [a [omega fz fx fy slip-a slip-r spin]] (.fill a 0))
    (.fill susp rest) (.fill susp-prev rest)
    (.fill contact 0) (.fill steer 0)))

(defn reset-state! [{:keys [^js damage] :as veh}]
  (clear-motion! veh)
  (.fill damage 0))

(defn chassis-position [{:keys [^js body]}]
  (let [t (.translation body)] [(.-x t) (.-y t) (.-z t)]))

(defn heading
  "Unit vector the chassis points along, in world space."
  [{:keys [^js body]}]
  (qrot! s-fwd (.rotation body) 0.0 0.0 -1.0)
  [(aget s-fwd 0) (aget s-fwd 1) (aget s-fwd 2)])

(defn wheels-on-ground [{:keys [contact]}]
  (reduce + (array-seq contact)))
