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

;; What being written off does to a vehicle, as a *velocity change* rather than
;; an impulse. This was three fixed impulse numbers, and a fixed impulse is a
;; different event depending on what it lands on: the same 900 N.s that gave a
;; saloon a 0.43 m/s hop moved a tanker 9 cm and threw an 86 kg scooter forty
;; metres into the air. Multiplying by the body's own mass makes a wreck look
;; the same whatever was wrecked, which is the only version anyone can tune.
(def ^:private wreck-lift 2.4)     ; m/s straight up when a car writes one off
(def ^:private wreck-shove 0.18)   ; share of the striker's velocity handed on
(def shock-lift 7.0)               ; and what a shock does: off its wheels entirely

(def ^:private lane 0.45)          ; share of the carriageway half-width to sit off
(def ^:private stop-line 0.86)     ; how far along a street a red light holds a car
(def ^:private follow-gap 7.5)     ; metres to leave to the car in front
(def ^:private straight-bonus 6.0) ; weighting for carrying straight on at a node

;; Civilian paint. The two darkest -- a near-black navy and a bottle green --
;; were genuinely hard to see coming under cloud, so both were lifted. A dark
;; car is fine; a car you cannot see until it is in the windscreen is not.
(def ^:private colours
  [0x9fa4ab 0x53688a 0x8c3a34 0xd8d2c4 0x3f6b52 0x6b5a3e 0x39465c 0xb08a3c])

(def ^:private glass 0x2b3d4e)
(def ^:private rubber 0x1b1b1e)
(def ^:private trim 0x36363c)

(def ^:private hazard 0xd8d2c4)

;; Lamps have two colours each: the lens when it is not lit, and the lens when
;; it is. Switching a light on is a single instance-colour write, which is why
;; every car on the map can have working lights for the price of the frames on
;; which one of them changes its mind.
(def ^:private lamp-off 0x7a7566)
(def ^:private lamp-on 0xfff4d2)
(def ^:private tail-off 0x6b2a25)
(def ^:private tail-on 0xc4392c)
(def ^:private tail-brake 0xff4a38)
(def ^:private marker-off 0x6e5c36)
(def ^:private marker-on 0xffb43a)

;; A rider is not painted in the vehicle's colour: a scooter and the person on
;; it are two different objects that happen to be travelling together, and
;; painting them the same makes a two-wheeler read as a bollard.
(def ^:private jacket 0x2f3540)
(def ^:private helmet 0xdad4c6)
(def ^:private skin 0xc79a6b)

(def ^:private tints
  {:glass glass :trim trim :hazard hazard
   :jacket jacket :helmet helmet :skin skin
   :lamp lamp-off :tail tail-off :marker marker-off})

;; Which bits of a car's lamp state are set. Kept as an integer rather than a
;; map so `sync!` can decide whether anything changed with one comparison per
;; car per frame -- which is the whole cost of this feature when nothing is
;; happening.
(def ^:const lit-bit 1)
(def ^:const brake-bit 2)

;; Local +Z is forward here, not -Z: `place!` yaws by atan2(nx, nz), which maps
;; local +Z onto the direction of travel. It never mattered while a car was a
;; symmetrical box; it matters the moment one has a windscreen.
;;
;; `parts` are the shapes bolted to the hull, `wheel` where the four go. The
;; collider is still one cuboid -- what the player hits is the shape of the
;; vehicle, not the shape of its cab.
;;
;; Each of these was two boxes and read as a brick with wheels. What separates a
;; car from a cube is not detail, it is three things: a bonnet lower than the
;; roof, a greenhouse narrower and shorter than the body, and both of them
;; *sloped*. `:tilt` bakes the angle into the part's matrix at build time, so
;; the slope is free at runtime.
;;
;; A positive tilt drops the +Z (front) end: `write-local!`'s third column is
;; (0, -sin a, cos a), so the nose goes down.
(defn- lamps
  "Headlights, tail lights and side markers.

  These come out of their own pool, drawn with an unlit material: a lamp that
  takes a light calculation is a lamp that goes out exactly when it is needed,
  which is the whole problem with modelling a light as a pale box. An unlit
  instance is the same one draw call and it stays bright through a storm.

  The markers are the ones that are easy to leave out and shouldn't be. Head
  and tail lights say which way a car is facing; only a side marker says a car
  is *there*, crossing the junction you are about to arrive at."
  [hx hy hz]
  (let [x (* 0.62 hx)
        y (* 0.30 hy)]
    [{:shape :lamp :at [(- x) y hz] :size [(* 0.42 hx) 0.16 0.05] :tint :lamp}
     {:shape :lamp :at [x y hz] :size [(* 0.42 hx) 0.16 0.05] :tint :lamp}
     {:shape :lamp :at [(- x) (* 0.42 hy) (- hz)] :size [(* 0.40 hx) 0.15 0.05] :tint :tail}
     {:shape :lamp :at [x (* 0.42 hy) (- hz)] :size [(* 0.40 hx) 0.15 0.05] :tint :tail}
     {:shape :lamp :at [(- hx) y (* 0.70 hz)] :size [0.05 0.12 0.22] :tint :marker}
     {:shape :lamp :at [hx y (* 0.70 hz)] :size [0.05 0.12 0.22] :tint :marker}]))

(defn- single-lamps
  "One headlight and one tail light, on the centre line. What a two-wheeler
  has, and the reason a single light in the mirror at night is a different
  thing to be told than a pair of them."
  [hx hy hz]
  [{:shape :lamp :at [0.0 (* 0.55 hy) hz] :size [(* 0.66 hx) 0.13 0.05] :tint :lamp}
   {:shape :lamp :at [0.0 (* 0.30 hy) (- hz)] :size [(* 0.56 hx) 0.11 0.05] :tint :tail}])

(def ^:private types
  [{:name :saloon
    :half [0.82 0.55 1.95] :ride 0.60
    :wheel {:r 0.32 :w 0.22 :track 0.80 :base 1.32}
    :parts (concat
            [;; bonnet, sloping away from the windscreen
             {:at [0.0 0.60 1.06] :size [1.54 0.16 1.55] :tilt 0.10}
             ;; greenhouse: inset on every side, and raked
             {:at [0.0 0.80 -0.24] :size [1.38 0.46 1.72] :tilt 0.05 :tint :glass}
             {:at [0.0 1.02 -0.38] :size [1.26 0.10 1.10]}
             ;; boot, dropping the other way
             {:at [0.0 0.58 -1.38] :size [1.52 0.14 1.06] :tilt -0.07}
             {:at [0.0 0.02 1.92] :size [1.60 0.20 0.10] :tint :trim}]
            (lamps 0.82 0.55 1.95))}

   {:name :hatch
    :half [0.76 0.52 1.62] :ride 0.56
    :wheel {:r 0.29 :w 0.20 :track 0.74 :base 1.10}
    :parts (concat
            [{:at [0.0 0.56 1.00] :size [1.42 0.14 1.10] :tilt 0.12}
             {:at [0.0 0.76 -0.18] :size [1.30 0.44 1.66] :tilt 0.04 :tint :glass}
             {:at [0.0 0.96 -0.30] :size [1.20 0.10 1.20]}
             ;; A hatchback has no boot: the back of the roof runs straight down
             ;; to the tail, which is the whole silhouette.
             {:at [0.0 0.70 -1.42] :size [1.28 0.60 0.30] :tilt -0.30 :tint :glass}]
            (lamps 0.76 0.52 1.62))}

   {:name :van
    :half [0.88 0.86 2.35] :ride 0.90
    :wheel {:r 0.35 :w 0.24 :track 0.84 :base 1.60}
    :parts (concat
            [;; Short nose, then a slab. A van is a box, but it is a box with a
             ;; face on the front of it.
             {:at [0.0 0.62 2.02] :size [1.66 0.34 0.66] :tilt 0.22}
             {:at [0.0 0.72 1.44] :size [1.62 0.70 0.42] :tilt 0.26 :tint :glass}
             {:at [0.0 0.94 -0.30] :size [1.74 0.16 3.60]}
             {:at [-0.89 0.30 -0.40] :size [0.06 0.70 3.20] :tint :glass}
             {:at [0.89 0.30 -0.40] :size [0.06 0.70 3.20] :tint :glass}]
            (lamps 0.88 0.86 2.35))}

   {:name :pickup
    :half [0.86 0.60 2.25] :ride 0.66
    :wheel {:r 0.36 :w 0.26 :track 0.84 :base 1.55}
    :parts (concat
            [{:at [0.0 0.64 1.52] :size [1.60 0.18 1.20] :tilt 0.14}
             {:at [0.0 0.86 0.56] :size [1.48 0.52 1.34] :tilt 0.06 :tint :glass}
             {:at [0.0 1.10 0.48] :size [1.38 0.10 1.02]}
             ;; the bed
             {:at [-0.80 0.36 -0.95] :size [0.12 0.48 2.30]}
             {:at [0.80 0.36 -0.95] :size [0.12 0.48 2.30]}
             {:at [0.0 0.36 -2.18] :size [1.68 0.48 0.12]}]
            (lamps 0.86 0.60 2.25))}

   {:name :tanker
    ;; The one civilian vehicle worth going out of your way for. Marked
    ;; volatile: wrecking it does not score much more than a lorry, it changes
    ;; the street for the next twenty-five seconds.
    :volatile? true
    :half [1.05 1.20 3.40] :ride 1.28
    :wheel {:r 0.46 :w 0.32 :track 0.98 :base 2.30}
    :parts (concat
            [{:at [0.0 0.86 2.72] :size [2.02 1.10 0.90] :tilt 0.10 :tint :glass}
             {:at [0.0 1.44 2.60] :size [1.92 0.20 1.10]}
             {:shape :tank :at [0.0 1.10 -0.70] :size [1.90 1.90 4.40] :tint :hazard}
             {:at [0.0 0.10 -0.70] :size [1.20 0.90 4.40] :tint :trim}
             {:at [0.0 0.02 3.38] :size [2.08 0.34 0.14] :tint :trim}]
            (lamps 1.05 1.20 3.40))}

   {:name :lorry
    :half [1.05 1.20 3.30] :ride 1.28
    :wheel {:r 0.46 :w 0.32 :track 0.98 :base 2.20}
    :parts (concat
            [;; Cab over the front axle, then the container behind it.
             {:at [0.0 0.86 2.62] :size [2.02 1.10 0.90] :tilt 0.10 :tint :glass}
             {:at [0.0 1.44 2.50] :size [1.92 0.20 1.10]}
             {:at [0.0 0.50 -0.90] :size [2.16 2.30 4.60]}
             {:at [0.0 0.02 3.28] :size [2.08 0.34 0.14] :tint :trim}]
            (lamps 1.05 1.20 3.30))}

   ;; --- two wheels ---------------------------------------------------------
   ;;
   ;; A two-wheeler is not a small car, and the differences are what make it
   ;; worth having on the road: the wheels are on the centre line rather than
   ;; at four corners (`:n 2` in the wheel map), the whole thing weighs a
   ;; fifth of a saloon so it goes a long way when it is hit, it carries a
   ;; single headlight rather than a pair, and there is a person sitting on it
   ;; in plain view rather than behind glass.

   {:name :scooter
    :half [0.26 0.42 0.82] :ride 0.50
    ;; Drawn: the block under the seat. Hit: the whole machine and its rider.
    :hull [0.15 0.13 0.40]
    ;; Light. It is a box collider like everything else, so this is the number
    ;; that decides whether clipping one is an event or a nudge.
    :density 120.0
    :pace 0.82
    :wheel {:r 0.21 :w 0.10 :base 0.60 :n 2}
    :parts (concat
            [;; The step-through floor, which is the whole silhouette: a
             ;; scooter is a bike with a gap where the tank should be.
             {:at [0.0 -0.20 -0.02] :size [0.42 0.10 1.24]}
             {:at [0.0 0.30 0.60] :size [0.48 0.60 0.09] :tilt 0.24}
             {:at [0.0 0.60 0.52] :size [0.58 0.06 0.06] :tint :trim}
             {:at [0.0 0.28 -0.34] :size [0.38 0.15 0.58] :tint :trim}
             ;; The rider: torso, arms and a helmet. Sitting upright, which is
             ;; the pose that tells a scooter from a motorbike at a distance.
             {:at [0.0 0.56 -0.16] :size [0.34 0.60 0.28] :tint :jacket}
             {:at [0.0 0.62 0.24] :size [0.40 0.12 0.60] :tilt 0.42 :tint :skin}
             {:at [0.0 0.98 -0.06] :size [0.27 0.29 0.29] :tint :helmet}]
            (single-lamps 0.30 0.42 0.82))}

   {:name :bike
    :half [0.28 0.50 1.05] :ride 0.58
    :hull [0.15 0.17 0.42]
    :density 150.0
    :pace 1.22
    :wheel {:r 0.30 :w 0.13 :base 0.74 :n 2}
    :parts (concat
            [{:at [0.0 0.26 0.16] :size [0.32 0.26 0.72]}
             {:at [0.0 0.30 -0.52] :size [0.30 0.13 0.66] :tint :trim}
             ;; Forks, raked back the way a fork is.
             {:at [0.0 0.10 0.84] :size [0.15 0.66 0.11] :tilt -0.26 :tint :trim}
             {:at [0.0 0.58 0.70] :size [0.62 0.06 0.06] :tint :trim}
             {:at [0.20 -0.20 -0.44] :size [0.09 0.09 0.86] :tint :trim}
             ;; Leaning forward onto the bars, which is the other half of the
             ;; difference: same two wheels, entirely different posture.
             {:at [0.0 0.60 -0.14] :size [0.36 0.64 0.30] :tilt 0.30 :tint :jacket}
             {:at [0.0 0.70 0.30] :size [0.42 0.11 0.66] :tilt 0.30 :tint :skin}
             {:at [0.0 1.00 0.16] :size [0.27 0.29 0.29] :tint :helmet}]
            (single-lamps 0.32 0.50 1.05))}])

(def ^:private type-index (zipmap (map :name types) (range)))

(defn- lamp-parts
  "Where a type's lamps live in its rig, by tint.

  A rig is [hull] ++ parts ++ wheels, so part `i` of the type is rig slot
  `i + 1`. Resolved once per type at build time: repainting a car's brake
  lights should not mean searching its parts for the ones that are brake
  lights."
  [type]
  (let [ps (:parts type)
        of (fn [t] (into-array (keep-indexed (fn [i p] (when (= t (:tint p)) (inc i)))
                                             ps)))]
    {:heads (of :lamp) :tails (of :tail) :markers (of :marker)}))

(def ^:private type-lamps (mapv lamp-parts types))

(def ^:private type-mix
  ;; Written as names and looked up, not as indices. The first version was a
  ;; literal list of numbers and referred to index 6 of a six-element vector --
  ;; every chunk that happened to draw that slot threw during generation and
  ;; quietly failed to load. A name that does not exist is a nil in this vector
  ;; and shows up the moment anything uses it; a number that does not exist
  ;; looks exactly like one that does.
  ;;
  ;; One vehicle in twelve is a lorry and one in twenty-four a tanker. Rare
  ;; enough that finding a tanker is an opportunity rather than the scenery,
  ;; common enough that a drive across a city turns one up.
  (mapv type-index
        [:saloon :saloon  :hatch  :scooter :hatch  :lorry
         :pickup :van     :saloon :hatch   :bike   :lorry
         :saloon :saloon  :hatch  :saloon  :hatch  :saloon
         :pickup :scooter :saloon :hatch   :bike   :tanker]))

(assert (every? some? type-mix) "traffic type-mix names a vehicle that does not exist")

(deftype Car [body collider handle key idx colour type ti meshes slots
              ^:mutable from ^:mutable to ^:mutable t ^:mutable speed
              ^:mutable leg ^:mutable alive? ^:mutable rnd ^:mutable ekey
              ^:mutable dist ^:mutable braking? ^:mutable lamps]
  ;; A record would allocate a new one of these per car per tick. Traffic is the
  ;; one place in the client where state is genuinely mutated in place, and
  ;; deftype fields are munged by the ClojureScript compiler rather than left to
  ;; Closure to guess at -- which a plain #js object is not.
  Object
  (toString [_] (str "Car " idx " " from "->" to)))

(def ^:private box-slots 6400)     ; bodies and bodywork
(def ^:private wheel-slots 3200)
(def ^:private tank-slots 96)
(def ^:private lamp-slots 3600)    ; six per car, and cars come and go in blocks

(defn- type-rig
  "One vehicle as a rig: hull, bodywork, four wheels.

  The wheels are `spin?` parts, which turn continuously rather than leaning
  back and forth the way a pedestrian's leg does. Everything else is rigid and
  its transform is built once, here, rather than sixty times a second."
  [{:keys [half hull parts wheel]}]
  (let [[hx hy hz] half
        ;; What is *drawn* for the hull, which is not always what is *hit*. On
        ;; a car they are the same box. On a two-wheeler they are not: the
        ;; collider has to be big enough to be worth hitting and to hold the
        ;; rider, and drawing that box puts a fridge on the road with a head
        ;; sticking out of it. The visible hull there is the engine block, and
        ;; the silhouette is made by the parts bolted to it.
        [vx vy vz] (or hull half)
        {wr :r ww :w track :track base :base wn :n :or {wn 4}} wheel
        y (+ (- hy) (* 0.35 wr))]
    (fig/rig
     (concat
      [{:shape :box :at [0.0 0.0 0.0] :size [(* 2 vx) (* 2 vy) (* 2 vz)]}]
      (for [{:keys [at size tilt shape]} parts]
        {:shape (or shape :box) :at at :size size :tilt (or tilt 0.0)})
      ;; Two wheels or four. A two-wheeler's are on the centre line, one at
      ;; each end -- there is no track to sit them either side of.
      (if (= 2 wn)
        (for [j (range 2)]
          {:shape :wheel :spin? true
           :at [0.0 y (if (zero? j) base (- base))]
           :size [ww (* 2 wr) (* 2 wr)]})
        (for [j (range 4)]
          {:shape :wheel :spin? true
           :at [(if (even? j) (- track) track) y (if (< j 2) base (- base))]
           :size [ww (* 2 wr) (* 2 wr)]}))))))

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
                                material wheel-slots {})
               ;; Axis along Z, so a scaled one is a tank lying down the length
               ;; of a lorry. Its own pool because it is the only round thing on
               ;; the road and there are never many of them.
               :tank (fig/pool scene
                               (doto (three/CylinderGeometry. 0.5 0.5 1 14)
                                 (.rotateX (/ js/Math.PI 2)))
                               material tank-slots {:receive? true})
               ;; Unlit, and casting no shadow. Both deliberate: a light
               ;; source that is shaded goes out under the cloud that is the
               ;; reason it came on, and a light source that casts a shadow is
               ;; a light source that is somehow also opaque.
               :lamp (fig/pool scene (three/BoxGeometry. 1 1 1)
                               (three/MeshBasicMaterial. #js {:color 0xffffff})
                               lamp-slots {:cast? false})}
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
  "A slot per part, from whichever pool that part's shape belongs to: the hull
  and the bodywork out of `:box`, a tanker's barrel out of `:tank`, the wheels
  out of `:wheel`."
  [pools type rig]
  (let [parts (:parts rig)
        slots (js/Int32Array. (count parts))]
    (dotimes [i (count parts)]
      (aset slots i (fig/claim! (get pools (:shape (nth parts i))))))
    slots))

(defn- spawn-one! [ts key idx from to t0 speed]
  (let [{:keys [^js world seed pools]} @ts
        ;; Which vehicle, drawn from a fixed mix rather than uniformly: a road
        ;; where a fifth of the traffic is lorries is a depot, not a street.
        ti (nth type-mix (mod (+ idx (* 3 (nth from 0)) (* 5 (nth from 1)))
                              (count type-mix)))
        type (nth types ti)
        lg (leg seed from to)
        ;; A scooter does not keep up with the traffic and a motorbike is
        ;; through it before you have finished looking. One multiplier, and it
        ;; is most of what makes a two-wheeler behave like one.
        speed (* speed (:pace type 1.0))
        p (place! #js {:x 0.0 :y 0.0 :z 0.0 :h 0.0} lg t0 (:ride type))
        [hx hy hz] (:half type)
        ^js body (.createRigidBody
                  world
                  (-> (.kinematicPositionBased RAPIER/RigidBodyDesc)
                      (.setTranslation (.-x p) (.-y p) (.-z p))))
        ^js collider (.createCollider
                      world
                      (-> (.cuboid RAPIER/ColliderDesc hx hy hz)
                          (.setDensity (:density type 300.0))
                          (.setFriction 0.8)
                          (.setRestitution 0.1)
                          (.setActiveEvents (.-CONTACT_FORCE_EVENTS RAPIER/ActiveEvents))
                          (.setContactForceEventThreshold 1500.0))
                      body)
        colour (nth colours (mod (+ idx (nth from 0) (nth from 1)) (count colours)))
        rig (nth (:rigs @ts) ti)
        slots (claim-slots! pools type rig)
        meshes (nth (:rig-meshes @ts) ti)]
    ;; Paint is written once. A car's colour does not change until it is a wreck.
    (let [parts (:parts rig)
          n (count parts)
          nb (inc (count (:parts type)))]
      (fig/set-colour! (:box pools) (aget slots 0) colour)
      (dotimes [i (count (:parts type))]
        (let [{:keys [tint shape]} (nth (:parts type) i)]
          (fig/set-colour! (get pools (or shape :box)) (aget slots (inc i))
                           (get tints tint colour))))
      (dotimes [i (- n nb)]
        (fig/set-colour! (:wheel pools) (aget slots (+ nb i)) rubber)))
    (->Car body collider (.-handle collider) key idx colour type ti meshes slots
           from to t0 speed lg true
           (js/Math.abs (js/Math.sin (+ (* 12.9898 idx) (* 0.017 (nth from 0)))))
           (edge-key from to) 0.0 false
           ;; -1 is "never painted": no real state equals it, so the first
           ;; `sync!` after a car spawns always writes its lamps.
           -1)))

(defn- paint-lamps!
  "Repaint one car's lamps for a lamp state. Called only when that state has
  actually changed, which for a car driving down an empty street is never."
  [pools ^Car c state]
  (let [^js pool (:lamp pools)
        ^js slots (.-slots c)
        {:keys [^js heads ^js tails ^js markers]} (nth type-lamps (.-ti c))
        lit? (pos? (bit-and state lit-bit))
        brake? (pos? (bit-and state brake-bit))]
    (dotimes [i (alength heads)]
      (fig/set-colour! pool (aget slots (aget heads i)) (if lit? lamp-on lamp-off)))
    (dotimes [i (alength tails)]
      (fig/set-colour! pool (aget slots (aget tails i))
                       (cond brake? tail-brake lit? tail-on :else tail-off)))
    (dotimes [i (alength markers)]
      (fig/set-colour! pool (aget slots (aget markers i))
                       (if lit? marker-on marker-off)))))

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
              parts (:parts (nth (:rigs @ts) (.-ti c)))
              ^js slots (.-slots c)]
          (dotimes [i (count parts)]
            (fig/release! (get pools (:shape (nth parts i))) (aget slots i)))
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
            ;; Where the car would get to if nothing were in its way. Kept,
            ;; because the difference between that and where it actually gets
            ;; to is the brake pedal -- there is no pedal to read here, so the
            ;; brake lights are inferred from the gap between intent and
            ;; permission.
            want (+ (.-t c) (/ (* (.-speed c) dt) total))
            t' want
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
        (set! (.-braking? c) (< t' (- want 1.0e-9)))
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
  "Place every car's body, bodywork and wheels, and set its lights.

  Wrecks are read from the body like anything else -- once a car is debris the
  physics is the only thing that knows where it is, and its wheels stop turning
  because it has stopped covering ground. A wreck's lights are out, which is
  also how you tell one from a car stopped at a light.

  `lights?` is the world's answer to whether it is dark enough to need them,
  and it is the same answer for every car -- so it arrives as one argument
  rather than being asked per vehicle."
  [ts lights?]
  (let [{:keys [chunks pools rigs ^js body-m ^js local-m ^js out-m
                ^js qpos ^js quat ^js one]} @ts
        lit (if lights? lit-bit 0)
        touched (volatile! false)]
    (doseq [[_ {:keys [cars]}] chunks]
      (dotimes [i (alength cars)]
        (let [^Car c (aget cars i)
              rig (nth rigs (.-ti c))]
          (fig/body-matrix! body-m qpos quat one ^js (.-body c))
          ;; The wheels are told how far the car has come, not how long it has
          ;; been going: a car held at a red light stands with its wheels still.
          (fig/place-rig! rig (.-meshes c) (.-slots c) body-m local-m out-m
                          (/ (.-dist c) (:r (:wheel (.-type c)))))
          (let [state (if (.-alive? c)
                        (bit-or lit (if (.-braking? c) brake-bit 0))
                        0)]
            (when (not= state (.-lamps c))
              (set! (.-lamps c) state)
              (vreset! touched true)
              (paint-lamps! pools c state))))))
    (fig/flush! (:box pools))
    (fig/flush! (:wheel pools))
    (fig/flush! (:tank pools))
    ;; Matrices moved for every lamp on the map; colours only for the cars that
    ;; changed their minds. Uploading the colour buffer regardless would undo
    ;; most of the point of comparing.
    (if @touched
      (fig/flush! (:lamp pools))
      (set! (.-needsUpdate (.-instanceMatrix ^js (:mesh (:lamp pools)))) true))))

(defn traffic? [ts handle] (contains? (:by-collider @ts) handle))

(defn wreck!
  "Stop driving a car and let physics have it. Returns the delta describing what
  was wrecked, or nil if this was not the hit that did it.

  `vel` is the striker's velocity, in m/s -- not an impulse, despite what the
  old parameter name claimed. `lift` is how hard the wreck is thrown upward,
  also in m/s; a shock passes its own."
  ([ts handle vel] (wreck! ts handle vel wreck-lift))
  ([ts handle vel lift]
   (when-let [^Car c (get (:by-collider @ts) handle)]
     (when (.-alive? c)
       (set! (.-alive? c) false)
       (overlay/record! (:overlay @ts) (.-key c) :cars (.-idx c))
       (let [^js body (.-body c)]
         (.setBodyType body (.-Dynamic RAPIER/RigidBodyType) true)
         ;; Mass is read *after* the type change: a kinematic body has no
         ;; dynamics to report one from. The floor is paranoia -- a zero here
         ;; would silently turn every wreck into a body that does not move.
         (let [m (max 1.0 (.mass body))]
           (.applyImpulse body
                          #js {:x (* m wreck-shove (nth vel 0))
                               :y (* m lift)
                               :z (* m wreck-shove (nth vel 2))}
                          true)))
       (swap! ts update :wrecked inc)
       (let [t (.translation ^js (.-body c))]
         {:cx (first (.-key c)) :cz (second (.-key c)) :index (.-idx c)
          ;; The caller decides what a wreck means. A tanker means fire.
          :pos [(.-x t) (.-y t) (.-z t)]
          :volatile? (boolean (:volatile? (.-type c)))})))))

(defn wreck-near!
  "Wreck every living car within `r` of (x, z). Returns their deltas.

  This is what a weapon does: it does not hit one thing, it clears a space."
  [ts x z r]
  (let [r2 (* r r)
        hits (for [[_ {:keys [cars]}] (:chunks @ts)
                   i (range (alength cars))
                   :let [^Car c (aget cars i)]
                   :when (.-alive? c)
                   :let [t (.translation ^js (.-body c))
                         dx (- (.-x t) x) dz (- (.-z t) z)]
                   :when (< (+ (* dx dx) (* dz dz)) r2)]
               (.-handle c))]
    (vec (keep #(wreck! ts % [0.0 0.0 0.0] shock-lift) (vec hits)))))

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
