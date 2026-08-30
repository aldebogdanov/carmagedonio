(ns carmageddon.client.cars
  "The vehicle catalogue: what you can drive, and what the rivals turn up in.

  One entry is everything that makes a vehicle that vehicle -- its box, its
  mass, where its wheels are and how big they are, which of them are driven,
  what it is geared for, and the shapes bolted on top of the chassis so a truck
  reads as a truck from the driving seat.

  The numbers are held here rather than in `sim` because they are content, not
  simulation: `vehicle` does not know a tractor from a muscle car, it reads a
  layout and a tuning map and integrates. Adding a vehicle should never mean
  touching the tyre model.

  `:muscle` is the reference car. Its tuning is `base-tuning` verbatim, which is
  what the measurement harness sweeps -- so a change there still means what it
  used to mean, and every other vehicle is expressed as a diff from it.")

(def base-tuning
  "The reference vehicle's tuning. Live-adjustable so the testbed can sweep it
  and a tuning overlay can drive it from the browser. Read fresh every tick, so
  a change takes effect immediately.

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
    ;; What the drivetrain is geared for, m/s. Drive torque falls away as the
    ;; car approaches it. Without this every vehicle ends up at the same
    ;; terminal speed, because the only things resisting are rolling resistance
    ;; and body damping and neither of them knows it is pushing a lorry.
    :top-speed            62.0
    :brake-torque         1800.0    ; N.m per wheel -- enough to lock
    :handbrake-torque     2600.0
    :wheel-inertia        1.4       ; kg.m^2
    :rolling-resistance   0.6

    ;; steering
    :max-steer            0.55      ; radians at standstill
    :steer-speed-falloff  0.016     ; authority lost per m/s
    :steer-rate           6.0}))    ; radians/second of input travel

(defn- connections
  "Four suspension mounts from a track width, a wheelbase and a ride height.

  Forward is -Z, so the front axle sits at negative z. Front and rear tracks are
  separate because a tractor's are not remotely the same."
  [front-track rear-track wheelbase axle-y]
  [[(- front-track) axle-y (- wheelbase)]
   [front-track     axle-y (- wheelbase)]
   [(- rear-track)  axle-y wheelbase]
   [rear-track      axle-y wheelbase]])

;; Shapes bolted to the chassis box, in chassis-local metres:
;; [x y z  hx hy hz  tint] where tint is :paint, :glass or :trim.
;; The chassis box itself is the hull; these are what make the silhouette.

(def catalogue
  {:muscle
   {:name    "Muscle"
    :half    [0.90 0.30 1.90]
    :density 292.0                 ; ~1200 kg
    :wheels  {:radius 0.35 :width 0.26
              :front-track 0.85 :rear-track 0.85 :wheelbase 1.35 :axle-y -0.15}
    :driven  #{2 3}
    :paint   0xb03a2e
    :tuning  {}                    ; the reference: see `base-tuning`
    :body    [[0.0 0.34 0.25  0.74 0.26 0.90 :glass]      ; cabin, set back
              [0.0 0.20 -1.55 0.86 0.10 0.30 :trim]       ; front splitter
              [0.0 0.36 1.78  0.80 0.06 0.14 :trim]]}     ; ducktail spoiler

   :hatchback
   {:name    "Hatchback"
    :half    [0.80 0.30 1.62]
    :density 258.0                 ; ~800 kg
    :wheels  {:radius 0.31 :width 0.22
              :front-track 0.76 :rear-track 0.76 :wheelbase 1.15 :axle-y -0.16}
    ;; Front-wheel drive, which is what makes it understeer where the muscle
    ;; car oversteers. The handbrake still works the rear axle.
    :driven  #{0 1}
    :paint   0x2f74b5
    :tuning  {:engine-torque 700.0 :top-speed 47.0
              :grip 1.52 :grip-rear-bias 1.10
              :spring-rate 24000.0 :nominal-load 1960.0 :max-load 14000.0
              :damper-compression 2200.0 :damper-rebound 3000.0
              :brake-torque 1350.0 :handbrake-torque 1700.0
              :wheel-inertia 1.0 :max-steer 0.62}
    :body    [[0.0 0.38 0.20  0.70 0.30 1.00 :glass]
              [0.0 0.34 1.40  0.72 0.26 0.24 :paint]]}    ; tailgate

   :pickup
   {:name    "Pickup"
    :half    [0.95 0.36 2.35]
    :density 300.0                 ; ~1930 kg
    :wheels  {:radius 0.40 :width 0.30
              :front-track 0.92 :rear-track 0.92 :wheelbase 1.62 :axle-y -0.22}
    :driven  #{0 1 2 3}            ; four-wheel drive; it climbs kerbs
    :paint   0x2d6a4a
    :tuning  {:engine-torque 620.0 :top-speed 45.0   ; per wheel, but four of them
              :grip 1.45
              :suspension-rest 0.40 :spring-rate 46000.0
              :nominal-load 4730.0 :max-load 32000.0
              :damper-compression 4200.0 :damper-rebound 5600.0
              :brake-torque 2200.0 :handbrake-torque 3000.0
              :wheel-inertia 2.2 :max-steer 0.50}
    :body    [[0.0 0.46 -0.85 0.86 0.40 1.05 :glass]      ; cab
              [0.0 0.30 1.70  0.90 0.24 0.06 :paint]      ; tailgate
              [-0.86 0.30 0.75 0.06 0.24 0.95 :paint]     ; bed sides
              [0.86  0.30 0.75 0.06 0.24 0.95 :paint]]}

   :truck
   {:name    "Truck"
    :half    [1.22 0.80 3.55]
    :density 176.0                 ; ~5400 kg
    ;; Same roll-threshold arithmetic as the tractor, and it matters more here:
    ;; a box body five metres high over a 1.1 m half-track went over at full
    ;; lock at 60 km/h. Wider, lower, and on tyres it runs out of before the
    ;; inside wheels come up.
    :wheels  {:radius 0.52 :width 0.38
              :front-track 1.18 :rear-track 1.22 :wheelbase 2.45 :axle-y -0.52}
    :driven  #{2 3}
    :paint   0x8e9299
    :tuning  {:engine-torque 3600.0 :top-speed 32.0
              :grip 1.12 :load-sensitivity 0.18
              :suspension-rest 0.44 :spring-rate 130000.0
              :nominal-load 13200.0 :max-load 95000.0
              :damper-compression 11000.0 :damper-rebound 14000.0
              :brake-torque 7000.0 :handbrake-torque 8000.0
              :wheel-inertia 7.0
              :max-steer 0.36 :steer-speed-falloff 0.030 :steer-rate 3.2}
    :body    [[0.0 1.05 -2.25 1.12 0.95 1.25 :glass]      ; cab over the axle
              [0.0 0.60 1.10  1.20 0.75 2.35 :paint]      ; box body
              [0.0 0.20 -3.60 1.15 0.30 0.14 :trim]]}     ; bull bar

   :tractor
   {:name    "Tractor"
    :half    [0.78 0.55 1.55]
    :density 372.0                 ; ~1980 kg, most of it over the back axle
    ;; Small at the front, enormous at the back -- the whole silhouette. Per
    ;; wheel radii, not one number, which is why `layout` carries four.
    ;; The mounts sit high and the track is wide for the size, which is the
    ;; only thing keeping it on its feet: a tractor's roll threshold is roughly
    ;; half-track over centre-of-gravity height, and the first numbers here put
    ;; that at 0.83 g against tyres worth 1.35 -- it went over every time the
    ;; harness asked it to corner.
    :wheels  {:radius 0.42 :rear-radius 0.80 :width 0.30 :rear-width 0.46
              :front-track 0.66 :rear-track 0.95 :wheelbase 1.15 :axle-y -0.12}
    :driven  #{2 3}
    :paint   0x2f7d32
    :tuning  {:engine-torque 4200.0 :top-speed 12.0     ; all torque, no pace
              ;; Agricultural tyres on tarmac, and deliberately below the roll
              ;; threshold: it should slide before it tips.
              :grip 1.02 :load-sensitivity 0.15
              :suspension-rest 0.22 :spring-rate 60000.0
              :nominal-load 4900.0 :max-load 40000.0
              :damper-compression 6000.0 :damper-rebound 7000.0
              :brake-torque 3200.0 :handbrake-torque 4000.0
              :wheel-inertia 9.0 :max-steer 0.62 :steer-speed-falloff 0.030}
    :body    [[0.0 0.72 0.55  0.56 0.55 0.55 :glass]      ; cab
              [0.0 0.30 -1.25 0.44 0.30 0.35 :paint]      ; bonnet
              [-0.30 1.05 -0.90 0.07 0.50 0.07 :trim]]}})  ; exhaust stack

(def kinds
  "Catalogue order. The player cycles through this and rivals are drawn from it,
  so it is a vector rather than the map's key order."
  [:muscle :hatchback :pickup :truck :tractor])

(def default-kind :muscle)

(defn spec [kind] (get catalogue kind (get catalogue default-kind)))

(defn half [kind] (:half (spec kind)))
(defn density [kind] (:density (spec kind)))
(defn paint [kind] (:paint (spec kind)))
(defn display-name [kind] (:name (spec kind)))

(defn layout
  "What `vehicle` needs to know about where the wheels are.

  `:radii` is per wheel rather than one number: the tractor's rear wheels are
  nearly twice the front's, and a single radius would have it either riding on
  stilts at the front or dragging its back axle."
  [kind]
  (let [{:keys [wheels driven]} (spec kind)
        {:keys [radius rear-radius width rear-width
                front-track rear-track wheelbase axle-y]} wheels
        rr (or rear-radius radius)]
    {:connections (connections front-track rear-track wheelbase axle-y)
     :radii       [radius radius rr rr]
     :widths      [width width (or rear-width width) (or rear-width width)]
     :radius      radius
     :steered     #{0 1}
     :driven      driven
     ;; The handbrake works the rear axle whatever the drive layout. Tying it
     ;; to `driven` put it on the front wheels of the front-wheel-drive car,
     ;; which is not a handbrake, it is a way to fall over.
     :handbraked  #{2 3}}))

(defn tuning
  "A tuning atom for `kind`.

  The reference car gets `base-tuning` itself, not a copy: the measurement
  harness sweeps that atom and expects the car it is measuring to notice."
  [kind]
  (let [over (:tuning (spec kind))]
    (if (empty? over)
      base-tuning
      (atom (merge @base-tuning over)))))
