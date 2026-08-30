(ns carmageddon.client.testbed
  "Headless vehicle characterisation.

  Runs scripted manoeuvres against the real simulation and reports numbers, so
  'does it feel right' has something underneath it that can be argued with. It
  needs no browser -- Rapier's wasm runs fine under Node -- which means vehicle
  behaviour is CI-testable and a tuning change that quietly ruins braking shows
  up as a diff rather than as a vague complaint three weeks later.

      npx shadow-cljs compile testbed && node target/testbed.js"
  (:require [carmageddon.client.cars :as cars]
            [carmageddon.client.input :as input]
            [carmageddon.client.sim :as sim]
            [carmageddon.client.vehicle :as vehicle]
            [carmageddon.client.chunks :as chunks]
            ["@dimforge/rapier3d-compat" :as RAPIER]
            [carmageddon.shared.worldgen :as worldgen]
            [carmageddon.shared.constants :as k]))

(def ^:private g 9.80665)

(defn- command [tick {:keys [throttle brake steer handbrake]}]
  (input/->Command tick (or throttle 0.0) (or brake 0.0) (or steer 0.0)
                   (boolean handbrake) false))

(defn- step-n!
  "Advance `n` ticks. `control` is either an input map or a fn of
  (tick, telemetry) -> input map. Returns the tick counter."
  [sim tick n control]
  (loop [t tick, i 0]
    (if (< i n)
      (let [in (if (fn? control) (control t (sim/telemetry sim)) control)]
        (sim/step! sim (command t in))
        (recur (inc t) (inc i)))
      t)))

(def ^:dynamic *kind*
  "Which catalogue entry the manoeuvres are run against. A dynamic var rather
  than an argument threaded through every manoeuvre: the manoeuvres are about
  what a vehicle does, not about which one, and the vehicle under test is
  exactly the kind of thing a binding is for."
  cars/default-kind)

(defn- fresh!
  "A settled car on an empty plane. Long enough for the suspension to stop
  ringing, which for a lorry on 130 kN springs takes rather more than a
  hatchback."
  []
  (let [s (sim/create! {:flat? true :kind *kind*})]
    (step-n! s 0 150 {})
    s))

(defn- speed-kmh [tel] (* 3.6 (js/Math.abs (:speed tel))))

(defn- planar-speed [{[vx _ vz] :vel}] (js/Math.hypot vx vz))

(defn- lateral-g
  "Yaw rate times speed is the centripetal acceleration the tyres are producing."
  [tel]
  (/ (js/Math.abs (* (second (:angvel tel)) (planar-speed tel))) g))

(defn- round [n x] (let [m (js/Math.pow 10 n)] (/ (js/Math.round (* x m)) m)))

(defn- mean [xs] (if (seq xs) (/ (reduce + xs) (count xs)) 0.0))

;; --- manoeuvres -------------------------------------------------------------

(defn accelerate
  "Standing start to `target` km/h at full throttle."
  [target]
  (let [s (fresh!)
        [x0 _ z0] (:pos (sim/telemetry s))]
    (loop [t 0]
      (let [tel (sim/telemetry s)]
        (cond
          (>= (speed-kmh tel) target)
          (let [[x _ z] (:pos tel)]
            {:seconds  (round 2 (* t k/dt))
             :metres   (round 1 (js/Math.hypot (- x x0) (- z z0)))
             :reached  (round 1 (speed-kmh tel))})

          (> t 1800) {:seconds nil :top-speed-kmh (round 1 (speed-kmh tel))}

          :else (do (sim/step! s (command t {:throttle 1.0})) (recur (inc t))))))))

(defn top-speed
  "Full throttle until it stops accelerating. This is the number gearing is for:
  without it every vehicle converges on the same terminal speed, because past
  the tyres the only thing resisting is body damping.

  Stops when a whole second of throttle is worth less than 0.05 m/s rather than
  after a fixed run: a tractor is done in six seconds and a muscle car is not."
  []
  (let [s (fresh!)]
    (loop [t 0, was 0.0]
      (let [v (js/Math.abs (:speed (sim/telemetry s)))]
        (cond
          (and (pos? t) (zero? (mod t 60)) (< (- v was) 0.05))
          {:kmh (round 1 (* 3.6 v))}

          (> t 5400) {:kmh (round 1 (* 3.6 v)) :converged false}

          :else (do (sim/step! s (command t {:throttle 1.0}))
                    (recur (inc t) (if (zero? (mod t 60)) v was))))))))

(defn brake-from
  "Accelerate to `from` km/h, then full brake to a stop."
  [from]
  (let [s (fresh!)
        t (loop [t 0]
            (if (or (>= (speed-kmh (sim/telemetry s)) from) (> t 1800))
              t
              (do (sim/step! s (command t {:throttle 1.0})) (recur (inc t)))))
        [x0 _ z0] (:pos (sim/telemetry s))
        entry (speed-kmh (sim/telemetry s))]
    (loop [t t, i 0, front [], rear []]
      (let [tel (sim/telemetry s)
            [s0 s1 s2 s3] (:suspension tel)]
        (if (or (< (speed-kmh tel) 1.0) (> i 600))
          (let [[x _ z] (:pos tel)]
            {:entry-kmh (round 1 entry)
             :metres    (round 1 (js/Math.hypot (- x x0) (- z z0)))
             :seconds   (round 2 (* i k/dt))
             ;; Front suspension should compress and rear extend under braking.
             ;; If this is ~0 there is no weight transfer and the model is flat.
             :weight-transfer-cm
             (round 1 (* 100.0 (- (mean rear) (mean front))))})
          (do (sim/step! s (command t {:brake 1.0}))
              (recur (inc t) (inc i)
                     (conj front (/ (+ s0 s1) 2.0))
                     (conj rear (/ (+ s2 s3) 2.0)))))))))

(defn skidpad
  "Hold `target` km/h at full lock and let the cornering settle. Reports the
  steady-state lateral grip the tyre model actually delivers."
  [target steer]
  (let [s (fresh!)
        t (loop [t 0]
            (if (or (>= (speed-kmh (sim/telemetry s)) target) (> t 1800))
              t
              (do (sim/step! s (command t {:throttle 1.0})) (recur (inc t)))))
        ;; Bang-bang throttle to hold speed while the corner settles.
        hold (fn [_ tel] {:throttle (if (< (speed-kmh tel) target) 0.6 0.0)
                          :steer steer})
        t (step-n! s t 240 hold)]
    (loop [t t, i 0, lat [], slip [], sp []]
      (if (< i 180)
        (let [tel (sim/telemetry s)]
          (sim/step! s (command t (hold t tel)))
          (recur (inc t) (inc i)
                 (conj lat (lateral-g tel))
                 (conj slip (js/Math.abs (sim/sideslip-deg tel)))
                 (conj sp (planar-speed tel))))
        ;; A car that has fallen over is producing no lateral grip and a
        ;; radius of nothing, and reporting those as measurements makes a
        ;; rollover look like understeer. The tractor does exactly this at
        ;; full lock, which is the correct behaviour for a tractor.
        (let [up (nth (:contact (sim/telemetry s)) 0)
              on (sim/wheels-on-ground s)]
          {:target-kmh   target
           :lateral-g    (round 2 (mean lat))
           :sideslip-deg (round 1 (mean slip))
           :radius-m     (round 1 (/ (* (mean sp) (mean sp)) (max 0.01 (* g (mean lat)))))
           :upright      (and up (>= on 3))
           :sim          s
           :tick         t})))))

(defn friction-circle
  "The decisive test. Establish steady cornering, then add full braking.

  A tyre model with a combined-slip budget must give up lateral grip to pay for
  the longitudinal demand -- the car should wash wide. If lateral g is unchanged
  while braking hard, longitudinal and lateral forces are being computed
  independently and the car can do physically impossible things."
  [target steer]
  (let [{:keys [sim tick] free :lateral-g} (skidpad target steer)
        hold (fn [_ tel] {:throttle (if (< (speed-kmh tel) target) 0.6 0.0)
                          :steer steer})
        t (step-n! sim tick 30 (fn [t tel] (assoc (hold t tel) :brake 1.0)))]
    (loop [t t, i 0, lat []]
      (if (< i 60)
        (let [tel (sim/telemetry sim)]
          (sim/step! sim (command t (assoc (hold t tel) :brake 1.0)))
          (recur (inc t) (inc i) (conj lat (lateral-g tel))))
        (let [braking (mean lat)]
          {:lateral-g-free    free
           :lateral-g-braking (round 2 braking)
           :grip-given-up-pct (round 0 (* 100.0 (- 1.0 (/ braking (max 0.01 free)))))})))))

(defn handbrake-slide
  "Yank the handbrake mid-corner and see whether the rear steps out and holds."
  [target]
  (let [s (fresh!)
        t (loop [t 0]
            (if (or (>= (speed-kmh (sim/telemetry s)) target) (> t 1800))
              t
              (do (sim/step! s (command t {:throttle 1.0})) (recur (inc t)))))
        t (step-n! s t 30 {:steer 1.0 :throttle 0.3})]
    (loop [t t, i 0, peak 0.0, trace []]
      (if (< i 150)
        (let [tel (sim/telemetry s)
              sl  (js/Math.abs (sim/sideslip-deg tel))]
          (sim/step! s (command t {:steer 1.0 :handbrake true}))
          (recur (inc t) (inc i) (max peak sl)
                 (if (zero? (mod i 30)) (conj trace (round 1 sl)) trace)))
        {:entry-kmh          target
         :peak-sideslip-deg  (round 1 peak)
         :sideslip-trace-deg trace}))))

(defn stability
  "Drive hard for 30 s of sim time and confirm nothing diverges. Catches force
  accumulators that are never cleared, NaN from a degenerate tyre basis, and
  suspension that pumps energy into the chassis instead of out of it."
  []
  (let [s (fresh!)]
    (loop [t 0]
      (if (< t 1800)
        (do
          (sim/step! s (command t {:throttle 1.0
                                   :steer (js/Math.sin (* t 0.01))
                                   :handbrake (zero? (mod (quot t 120) 5))}))
          (recur (inc t)))
        (let [tel (sim/telemetry s)
              [x y z] (:pos tel)
              nums (concat [x y z] (:vel tel) (:load tel) (:slip-angle tel))]
          {:finite       (every? #(js/isFinite %) nums)
           :height-m     (round 2 y)
           :speed-kmh    (round 1 (speed-kmh tel))
           :max-load-N   (round 0 (apply max (:load tel)))
           :on-ground    (count (filter true? (:contact tel)))})))))

(defn worldgen-cost
  "How long one chunk takes to generate. Decides whether chunk streaming can run
  on the main thread or has to move to a worker: anything approaching a 16 ms
  frame budget will be visible as a hitch every time the player crosses a
  boundary."
  []
  (let [seed 20260823
        warm (dotimes [i 3] (worldgen/chunk-data seed i i))
        t0   (js/Date.now)
        n    25]
    (dotimes [i n] (worldgen/chunk-data seed (- i 40) (+ i 17)))
    (let [ms (/ (- (js/Date.now) t0) n)]
      {:ms-per-chunk (round 2 ms)
       :chunks-per-frame-budget (round 1 (/ 16.0 ms))
       :ring-of-49-ms (round 0 (* 49 ms))})))

(defn heightfield-orientation
  "Build a real chunk collider, drop rays onto it, and compare what physics
  reports against what worldgen says the height is.

  Worth doing explicitly: a transposed or mirrored heightfield still looks like
  plausible terrain, so this failure mode is invisible by eye and only shows up
  as the car floating above or sinking into the ground in places."
  []
  (let [seed 20260823
        cx 2 cz 1
        ^js world (RAPIER/World. #js {:x 0 :y -9.81 :z 0})
        data (worldgen/chunk-data seed cx cz)
        n    (:verts data)
        _    (chunks/add-collider! world data (dec n))
        ;; castRay reads the query pipeline, which world.step populates.
        _    (.step world)
        field (worldgen/road-field seed cx cz)
        ;; Asymmetric sample points -- a symmetric grid would pass even
        ;; when transposed.
        pts  (for [fx [0.13 0.37 0.62 0.88], fz [0.21 0.55 0.79]]
               [(+ (* cx k/chunk-size) (* fx k/chunk-size))
                (+ (* cz k/chunk-size) (* fz k/chunk-size))])
        errs (for [[x z] pts]
               (let [ray (RAPIER/Ray. #js {:x x :y 400.0 :z z} #js {:x 0 :y -1 :z 0})
                     hit (.castRay world ray 800.0 true)
                     y   (when hit (- 400.0 (or (.-timeOfImpact hit) (.-toi hit))))
                     expected (first (worldgen/surface seed field x z))]
                 (if y (js/Math.abs (- y expected)) ##Inf)))]
    {:max-error-m (round 3 (apply max errs))
     :mean-error-m (round 3 (mean errs))}))

;; --- report -----------------------------------------------------------------

(defn- line [label v] (println (str "  " label) (pr-str v)))

(defn run-all! []
  (println "\n=== vehicle characterisation ===\n")
  (println "acceleration")
  (line "0-60  km/h " (accelerate 60))
  (line "0-100 km/h " (accelerate 100))
  (println "\nbraking + weight transfer")
  (line "from 100   " (brake-from 100))
  (println "\nskidpad (steady-state cornering)")
  (line "at 50 km/h " (dissoc (skidpad 50 1.0) :sim :tick))
  (line "at 80 km/h " (dissoc (skidpad 80 1.0) :sim :tick))
  (println "\nfriction circle (brake at the cornering limit)")
  (line "at 60 km/h " (friction-circle 60 1.0))
  (println "\nhandbrake slide")
  (line "from 60    " (handbrake-slide 60))
  (println "\nstability (30 s of throttle, steering and handbrake)")
  (line "           " (stability))
  (println "\nworldgen")
  (line "chunk cost " (worldgen-cost))
  (line "heightfield" (heightfield-orientation))
  (println))

(defn- hurt!
  "A settled car with `panels` worth of damage already on it."
  [panels total]
  (let [s (fresh!)]
    (vehicle/set-damage! (sim/player-vehicle s) panels total)
    s))

(defn damaged-run
  "Full throttle in a straight line from a standstill, with no steering input
  at all. Reports how long it takes to reach 40 km/h, how far it strays
  sideways from the line it started on, and how far it then takes to stop."
  [panels total]
  (let [s (hurt! panels total)
        [x0 _ z0] (:pos (sim/telemetry s))
        [fx _ fz] (:forward (sim/telemetry s))
        t40 (loop [t 0]
              (cond (>= (speed-kmh (sim/telemetry s)) 40) t
                    (> t 1800) nil
                    :else (do (sim/step! s (command t {:throttle 1.0}))
                              (recur (inc t)))))
        t   (or t40 1800)
        t   (step-n! s t 240 {:throttle 1.0})
        [x _ z] (:pos (sim/telemetry s))
        ;; Distance from the line the car set off along: the cross product of
        ;; the travelled vector with the starting heading.
        drift (js/Math.abs (- (* (- x x0) fz) (* (- z z0) fx)))
        v0    (speed-kmh (sim/telemetry s))
        stop  (loop [t t, i 0]
                (if (or (< (speed-kmh (sim/telemetry s)) 1.0) (> i 900))
                  i
                  (do (sim/step! s (command t {:brake 1.0})) (recur (inc t) (inc i)))))]
    {:seconds-to-40 (when t40 (round 2 (* t40 k/dt)))
     :drift-m       (round 1 drift)
     :from-kmh      (round 0 v0)
     :stop-seconds  (round 2 (* stop k/dt))}))

(defn damage!
  "What being broken in each particular way actually costs.

  A damage model whose panels all do the same thing is a scalar wearing four
  hats. The columns are here so it can be seen not to be one."
  []
  (println "\n=== damage ===")
  (println "  state           0-40s   drift-m   from-kmh   stop-s")
  (doseq [[label panels total]
          [["pristine"    [0.0 0.0 0.0 0.0] 0.0]
           ["front 0.8"   [0.8 0.0 0.0 0.0] 0.5]
           ["rear 0.8"    [0.0 0.8 0.0 0.0] 0.5]
           ["left 0.8"    [0.0 0.0 0.8 0.0] 0.5]
           ["right 0.8"   [0.0 0.0 0.0 0.8] 0.5]
           ["all over"    [0.5 0.5 0.5 0.5] 0.9]]]
    (let [r (damaged-run panels total)]
      (println (str "  " (.padEnd label 16)
                    (.padEnd (str (or (:seconds-to-40 r) "-")) 8)
                    (.padEnd (str (:drift-m r)) 10)
                    (.padEnd (str (:from-kmh r)) 11)
                    (str (:stop-seconds r)))))))

(defn catalogue!
  "Every vehicle in the catalogue, measured the same way.

  The point of the table is that the rows differ. A catalogue where the truck
  accelerates like the muscle car is a catalogue of one car wearing five hats,
  and the only way to know which one you have is to measure it."
  []
  (println "\n=== catalogue ===")
  ;; 0-40 and a 30 km/h skidpad, because the tractor is geared for 43 km/h and
  ;; a table full of "-" measures nothing.
  (println "  vehicle      mass-kg   0-40s   top-kmh   brake-m   lat-g   radius-m")
  (doseq [kind cars/kinds]
    (binding [*kind* kind]
      (let [s    (fresh!)
            mass (round 0 (.mass ^js (sim/chassis-body s)))
            acc  (accelerate 40)
            top  (top-speed)
            brk  (brake-from 40)
            pad  (skidpad 30 1.0)]
        (println (str "  " (.padEnd (cars/display-name kind) 13)
                      (.padEnd (str mass) 10)
                      (.padEnd (str (or (:seconds acc) "-")) 8)
                      (.padEnd (str (:kmh top)) 10)
                      (.padEnd (str (:metres brk)) 10)
                      (.padEnd (str (if (:upright pad) (:lateral-g pad) "roll")) 8)
                      (str (if (:upright pad) (:radius-m pad) "-"))))))))

(defn sweep!
  "Vary one tuning key and report what it does to grip and to slide behaviour.
  Sweeping beats guessing: the interesting number is not peak grip, it is
  whether a slide survives once it starts."
  [k values]
  (println (str "\n=== sweep " k " ==="))
  (println "  value   lat-g   radius   0-100   brake-m   peak-slip   slip-after-1.5s")
  (let [original (get @cars/base-tuning k)]
    (doseq [v values]
      (swap! cars/base-tuning assoc k v)
      (let [pad   (skidpad 60 1.0)
            hb    (handbrake-slide 60)
            acc   (accelerate 100)
            brk   (brake-from 100)
            trace (:sideslip-trace-deg hb)]
        (println (str "  " (.padEnd (str v) 8)
                      (.padEnd (str (:lateral-g pad)) 8)
                      (.padEnd (str (:radius-m pad)) 9)
                      (.padEnd (str (or (:seconds acc) "-")) 8)
                      (.padEnd (str (:metres brk)) 10)
                      (.padEnd (str (:peak-sideslip-deg hb)) 12)
                      (str (nth trace 3 "-"))))))
    (swap! cars/base-tuning assoc k original)))

(defn -main [& _]
  (-> (sim/init!)
      (.then (fn [_]
               (run-all!)
               (catalogue!)
               (damage!)
               (sweep! :grip [0.9 1.15 1.35 1.6 1.9])
               (sweep! :grip-rear-bias [1.0 0.96 0.92 0.88 0.84])
               (println)
               (js/process.exit 0)))
      (.catch (fn [e] (js/console.error e) (js/process.exit 1)))))
