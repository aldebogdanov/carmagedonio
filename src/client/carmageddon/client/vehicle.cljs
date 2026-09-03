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
   ;; [front rear left right total], each 0 = pristine, 1 = destroyed. Held
   ;; here rather than in the sim so the vehicle model is self-contained and an
   ;; AI or remote car carries its own.
   ;;
   ;; The total is accumulated separately rather than derived from the panels.
   ;; Deriving it means a car that has been hit hard in one place only and a car
   ;; that has been hit everywhere lightly can come out the same, and they
   ;; should not: the first still drives.
   :damage    (js/Float64Array. 5)
   ;; [engine grip armour], all 1.0 at rest. Power-ups multiply these rather
   ;; than editing the tuning atom: the reference car's tuning is shared with
   ;; the measurement harness, and a boost that leaked into it would quietly
   ;; change what every published number means.
   :boost     (doto (js/Float64Array. 3) (.fill 1.0))
   ;; What the road is worth today. A property of the world acting on the car,
   ;; not of the driver -- which is why it lives here beside the damage rather
   ;; than in the Command.
   :surface   (doto (js/Float64Array. 1) (.fill 1.0))
   ;; [brake reverse], both 0..1, written by `update!` from the *resolved*
   ;; command. The renderer lights the lamps off this rather than inferring
   ;; them from the velocity, which gets a car coasting downhill wrong in both
   ;; directions -- it is slowing down and no pedal is down, and it is picking
   ;; up speed backwards with the brake pedal buried.
   :lights    (js/Float64Array. 2)})

(def ^:const boost-engine 0)
(def ^:const boost-grip 1)
(def ^:const boost-armour 2)

;; What the driver last did with the pedals, for whoever is drawing the lamps.
;; Two scalars rather than a pair, because this is read per car per frame and a
;; vector there is garbage sixty times a second for two numbers.
(defn brake-of   [{:keys [^js lights]}] (aget lights 0))
(defn reverse-of [{:keys [^js lights]}] (aget lights 1))

(defn set-boost! [{:keys [^js boost]} i v] (aset boost i v))
(defn set-surface! [{:keys [^js surface]} v] (aset surface 0 v))
(defn surface-of [{:keys [^js surface]}] (aget surface 0))
(defn boost-of [{:keys [^js boost]} i] (aget boost i))
(defn clear-boosts! [{:keys [^js boost]}] (.fill boost 1.0))

(def ^:const dmg-front 0)
(def ^:const dmg-rear  1)
(def ^:const dmg-left  2)
(def ^:const dmg-right 3)
(def ^:const dmg-total 4)

(defn damage
  "How wrecked the car is overall, 0 to 1."
  [{:keys [^js damage]}] (aget damage dmg-total))

(defn panel
  "One panel's damage: `dmg-front`, `dmg-rear`, `dmg-left` or `dmg-right`."
  [{:keys [^js damage]} i] (aget damage i))

(defn panels
  "All four panels as a vector, for the HUD and the bodywork."
  [{:keys [^js damage]}]
  [(aget damage 0) (aget damage 1) (aget damage 2) (aget damage 3)])

(defn forward-speed
  "Signed metres per second along the chassis' forward axis."
  [{:keys [^js body]}]
  (let [^js lv (.linvel body)]
    (qrot! s-fwd (.rotation body) 0.0 0.0 -1.0)
    (+ (* (.-x lv) (aget s-fwd 0))
       (* (.-y lv) (aget s-fwd 1))
       (* (.-z lv) (aget s-fwd 2)))))

(defn- impact-panel
  "Which panel took the hit, inferred from the direction the car was travelling
  in its own frame.

  A contact-force event carries a force magnitude and a pair of collider
  handles -- not a contact point -- so where the car was hit has to be worked
  out rather than read. Its own velocity is the right thing to work it out
  from: driving into a wall is a front-end impact, reversing into a bollard is
  a rear one, and being caught while sliding is a side one. It also agrees with
  what the player will say happened, which matters more here than being right
  about some grazing contact."
  [{:keys [^js body]}]
  (let [^js q  (.rotation body)
        ^js lv (.linvel body)]
    (qrot! s-fwd q 0.0 0.0 -1.0)
    (qrot! s-right q 1.0 0.0 0.0)
    (let [vf (+ (* (.-x lv) (aget s-fwd 0)) (* (.-y lv) (aget s-fwd 1))
                (* (.-z lv) (aget s-fwd 2)))
          vr (+ (* (.-x lv) (aget s-right 0)) (* (.-y lv) (aget s-right 1))
                (* (.-z lv) (aget s-right 2)))]
      (if (>= (js/Math.abs vf) (js/Math.abs vr))
        (if (pos? vf) dmg-front dmg-rear)
        (if (pos? vr) dmg-right dmg-left)))))

(defn add-damage!
  "Accumulate impact damage, saturating at 1.

  The total takes all of it and decides when the car is written off; the panel
  the car was moving into takes half again as much, so a driver who only ever
  hits things nose-first cooks the engine well before the car is finished. A
  car that has only ever been rear-ended still pulls."
  [{:keys [^js damage ^js boost] :as veh} amount]
  ;; Armour divides what arrives, so a plated car takes the same hits and keeps
  ;; more of itself.
  (let [amount (/ amount (max 0.05 (aget boost boost-armour)))
        p (impact-panel veh)]
    (aset damage dmg-total (min 1.0 (+ (aget damage dmg-total) amount)))
    (aset damage p (min 1.0 (+ (aget damage p) (* 1.5 amount))))))

(defn repair!
  "Undo `amount` of every kind of damage. Panels come back faster than the
  total, so a repair visibly un-bends the car before it fully restores it."
  [{:keys [^js damage]} amount]
  (dotimes [i 5]
    (aset damage i (max 0.0 (- (aget damage i) (if (= i dmg-total) amount (* 1.4 amount)))))))

(defn set-damage!
  "Force the damage state: four panels and a total.

  Gameplay damage always arrives through `add-damage!`, which decides for
  itself which panel took it. This is for the measurement harness, which needs
  to ask what a folded nose costs without having to arrange to crash into
  something at exactly the right angle first."
  [{:keys [^js damage]} [f r l rt] total]
  (aset damage dmg-front f)
  (aset damage dmg-rear  r)
  (aset damage dmg-left  l)
  (aset damage dmg-right rt)
  (aset damage dmg-total total))

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
        ;; The limiter moves with the boost. Without this a nitro would add
        ;; torque the gearing immediately took back, and top speed would be
        ;; exactly what it was before.
        top  (* top (:boost-top cmd 1.0))
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
           slip-a slip-r contact spin steer ^js damage] :as veh}
   i dt {:keys [drive brake handbrake]}]
  (let [{:keys [connections radii steered driven handbraked]} layout
        radius (nth radii i)
        ^js boost (:boost veh)
        ^js surface (:surface veh)
        {:keys [suspension-rest spring-rate damper-compression damper-rebound
                max-load nominal-load grip grip-rear-bias load-sensitivity
                lat-B lat-C lat-E long-B long-C long-E
                engine-torque brake-torque handbrake-torque
                wheel-inertia rolling-resistance]} @tuning
        [cx cy cz] (nth connections i)
        ;; A wrecked car should be a worse car, and it should be worse in the
        ;; way it was broken. Everything here saturates well short of
        ;; undriveable, because a car you cannot move is not a challenge, it is
        ;; just the end of the run without the screen saying so.
        dmg    (aget damage dmg-total)
        front  (aget damage dmg-front)
        ;; The engine and the radiator are at the front, so that is what costs
        ;; power. Broad damage costs some anyway -- a bent shell drags.
        engine-torque (* engine-torque
                         (- 1.0 (* 0.35 dmg))
                         (- 1.0 (* 0.40 front))
                         (aget boost boost-engine))
        ;; Brake lines and discs are behind the same bumper.
        brake-torque  (* brake-torque (- 1.0 (* 0.30 front)))
        ;; Wheels 0 and 2 are the left pair; a caved-in flank ruins the
        ;; geometry on that side and the tyres on it stop working properly.
        side   (aget damage (if (even? i) dmg-left dmg-right))
        grip          (* grip (- 1.0 (* 0.22 dmg)) (- 1.0 (* 0.28 side))
                         (aget boost boost-grip) (aget surface 0))
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
  [{:keys [steer tuning ^js damage ^js lights] :as veh} cmd dt]
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
        ;; A car with one flank caved in pulls towards the damage: that side
        ;; is dragging and the geometry on it is bent. Added to the input
        ;; rather than to the output so it is limited by the same lock the
        ;; driver has, and so full opposite lock can still hold it straight --
        ;; which is the point. It should cost attention, not control.
        ;; 0.22 was the first guess and it was far too much: the harness put
        ;; a car with one flank in at 62 m off its own line in four seconds,
        ;; which is not a car that pulls, it is a car that turns.
        pull     (* 0.10 (- (aget damage dmg-right) (aget damage dmg-left)))
        ;; Negated: a positive rotation about +Y swings a -Z-facing car left,
        ;; and input +1 means "right".
        target   (* limit (- (max -1.0 (min 1.0 (+ (:steer cmd) pull)))))
        cur      (aget steer 0)
        ;; Bent steering is slow steering.
        d        (* steer-rate dt max-steer (- 1.0 (* 0.35 (aget damage dmg-total))))
        next     (cond (> target (+ cur d)) (+ cur d)
                       (< target (- cur d)) (- cur d)
                       :else target)]
    (aset steer 0 next)
    ;; Resolved once, not once per wheel. It was inside the loop, which meant
    ;; four `forward-speed` calls and four map builds a tick per vehicle for an
    ;; answer that cannot differ between wheels.
    (let [c (gear-for veh (assoc cmd :boost-top (boost-of veh boost-engine)))]
      (aset lights 0 (max 0.0 (min 1.0 (:brake c 0.0))))
      ;; Reverse is negative drive: at rest the brake pedal selects it, and
      ;; rolling backwards it is what the brake pedal keeps doing.
      (aset lights 1 (if (neg? (:drive c 0.0)) 1.0 0.0))
      (dotimes [i 4]
        (step-wheel! veh i dt c)))))

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
