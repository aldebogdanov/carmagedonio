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
  "One entry per `worldgen/pickup-kinds`, in that order.

  `shape` picks which pair of pools draws it and `scale` how big. The last two
  hold nothing and buy nothing: they are points, and they are discs rather than
  stars so that a line of them down a carriageway reads as money rather than as
  six power-ups you have not identified yet."
  ;; Durations are double what they were. Eight seconds is long enough to
  ;; notice a power-up and not long enough to use one: by the time you have
  ;; registered what you picked up and found something to spend it on, it has
  ;; gone. Sixteen leaves room for the decision the timer exists to create.
  [{:name :repair :colour 0x4fbf6a :label "REPAIR"    :secs 0.0  :shape :star :scale 1.0}
   {:name :nitro  :colour 0xf05a28 :label "NITRO"     :secs 18.0 :shape :star :scale 1.0}
   {:name :grip   :colour 0x39a0d8 :label "GRIP"      :secs 24.0 :shape :star :scale 1.0}
   {:name :armour :colour 0xb9bec6 :label "ARMOUR"    :secs 28.0 :shape :star :scale 1.0}
   {:name :flame  :colour 0xf2a63a :label "FIRETRAIL" :secs 16.0 :shape :star :scale 1.0}
   {:name :shock  :colour 0xd8d84a :label "SHOCK"     :secs 16.0 :shape :star :scale 1.0}
   {:name :coin   :colour 0xe8b93a :label "+45"       :secs 0.0  :shape :coin :scale 0.55}
   {:name :nugget :colour 0xffd24a :label "+400"      :secs 0.0  :shape :coin :scale 1.15}])

(defn kind-name [k] (:name (nth kinds k)))

(def ^:private reach 3.2)       ; metres: how close is "drove through it"
;; Per shape, and very different numbers: there are three power-ups in a chunk
;; and two trails of up to nine coins. Measured on a loaded city: 39 stars
;; against 173 coins.
(def ^:private slots {:star 320 :coin 900})
(def ^:private star-r 0.62)
(def ^:private star-depth 0.17)

(defn- star-geometry
  "A five-pointed star, extruded, built once and shared by every crate.

  These were cubes, and so is half the scenery: a smashable crate, a gas
  cylinder and a power-up were three differently-coloured boxes, and the only
  way to learn which was which was to drive into one. A star is not a shape
  anything else in the world has, which is the entire point of it.

  Extrusion runs along +Z and the star's face is in XY, so spinning it about Y
  turns it edge-on and back -- the way a pickup has spun in every game that has
  ever had one."
  []
  (let [shape (three/Shape.)
        points 5
        inner (* 0.46 star-r)]
    (dotimes [i (* 2 points)]
      (let [a (- (* i (/ js/Math.PI points)) (/ js/Math.PI 2))
            rad (if (even? i) star-r inner)
            x (* rad (js/Math.cos a))
            y (* rad (js/Math.sin a))]
        (if (zero? i) (.moveTo shape x y) (.lineTo shape x y))))
    (.closePath shape)
    (doto (three/ExtrudeGeometry. shape #js {:depth star-depth :bevelEnabled false})
      (.center))))

(defn- coin-geometry
  "A disc standing on its edge, so spinning it about Y flashes it face-on and
  then edge-on -- which is what a coin does and what nothing else here does."
  []
  (doto (three/CylinderGeometry. star-r star-r 0.10 14)
    (.rotateX (/ js/Math.PI 2))))

;; Fire trail: a short-lived puddle every few ticks. Any faster and the pool
;; cap is spent in two seconds of driving.
(def ^:private trail-every 9)
(def ^:private trail-r 2.6)
(def ^:private trail-life 3.6)

;; Shock: arcs to whatever is nearest, on a beat rather than continuously.
(def ^:private shock-every 24)
(def ^:private shock-reach 14.0)

;; And what that looks like. The shock had no visual at all: every 0.4 s things
;; died in silence, with nothing connecting the car to what it had killed --
;; which reads as a bug rather than as a weapon.
;;
;; An arc is three straight segments with a kink at each joint, out of an unlit
;; pool. Three boxes is enough to stop it reading as a stick, and the whole
;; thing is written once when it is struck and released when it expires, so a
;; beat costs a handful of matrix writes and the frames in between cost nothing.
(def ^:private arc-slots 128)
(def ^:private arc-life 0.22)     ; seconds -- a strobe against a 0.4 s beat
(def ^:private arc-thick 0.09)
(def ^:private arc-kink 0.13)     ; sideways jag, as a share of the span
(def ^:private arc-segments 3)

(def ^:private z-axis (three/Vector3. 0 0 1))

(defn create [^js scene ov]
  (atom {:scene scene
         :overlay ov
         ;; Two shapes, each drawn twice: the solid one, and a larger one
         ;; behind it in the same colour, additive. That second pass is what
         ;; makes a pickup shine. It replaces an `emissive` that had been set
         ;; to black since the day it was written and had never done anything,
         ;; which is why a crate on a wet road at dusk was a dark lump.
         ;;
         ;; Instance colours reach a basic material, so every pickup glows its
         ;; own colour out of one material.
         :pools (into {}
                      (for [[shape geom] [[:star (star-geometry)]
                                          [:coin (coin-geometry)]]]
                        [shape
                         {:solid (fig/pool scene geom
                                           (three/MeshPhongMaterial.
                                            #js {:shininess 40 :flatShading true})
                                           (slots shape) {:cast? true})
                          :glow (fig/pool scene geom
                                          (three/MeshBasicMaterial.
                                           #js {:transparent true :opacity 0.42
                                                :depthWrite false
                                                :blending (.-AdditiveBlending three)})
                                          (slots shape) {:cast? false})}]))
         ;; Unlit and shadowless, like every other light source in the game.
         :arc-pool (fig/pool scene (three/BoxGeometry. 1 1 1)
                             (three/MeshBasicMaterial. #js {:color 0xfff27a})
                             arc-slots {:cast? false})
         :m4 (three/Matrix4.)
         ;; Scratch for building an arc. Allocated once: a beat can strike
         ;; twenty things and none of it should be garbage.
         :va (three/Vector3.)
         :vb (three/Vector3.)
         :dir (three/Vector3.)
         :sc (three/Vector3.)
         :q (three/Quaternion.)
         :arcs []            ; [{:slots [..] :t0 seconds}]
         :chunks {}          ; [cx cz] -> [{:x :y :z :kind :idx :slot}]
         :active {}          ; kind -> seconds remaining
         :taken 0}))

(defn taken-in [ps key] (overlay/destroyed (:overlay @ps) key :pickups))

(defn add-chunk! [ps key arr]
  (when (and arr (pos? (.-length arr)))
    (let [gone (taken-in ps key)
          st worldgen/pickup-stride
          n (/ (.-length arr) st)
          pools (:pools @ps)
          made (vec (for [idx (range n)
                          :when (not (contains? gone idx))
                          :let [o (* idx st)
                                kind (int (aget arr (+ o 3)))
                                {:keys [colour shape]} (nth kinds kind)
                                {:keys [solid glow]} (get pools shape)
                                slot (fig/claim! solid)
                                gslot (fig/claim! glow)]]
                      (do (fig/set-colour! solid slot colour)
                          (fig/set-colour! glow gslot colour)
                          {:x (aget arr (+ o 0)) :y (aget arr (+ o 1))
                           :z (aget arr (+ o 2)) :kind kind :idx idx
                           :shape shape :slot slot :glow-slot gslot})))]
      (swap! ps assoc-in [:chunks key] made)
      made)))

(defn- release-pickup! [pools {:keys [shape slot glow-slot]}]
  (let [{:keys [solid glow]} (get pools shape)]
    (fig/release! solid slot)
    (fig/release! glow glow-slot)))

(defn remove-chunk! [ps key]
  (let [pools (:pools @ps)]
    (doseq [p (get (:chunks @ps) key)] (release-pickup! pools p))
    (swap! ps update :chunks dissoc key)))

(defn- take-index! [ps key idx]
  (overlay/record! (:overlay @ps) key :pickups idx)
  (when-let [p (first (filter #(= idx (:idx %)) (get (:chunks @ps) key)))]
    (release-pickup! (:pools @ps) p)
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
  "Everything the car at (x, z) is standing on, as `[{:kind :delta} ...]`.

  A distance check rather than a sensor collider: there are a handful of these
  per chunk, the answer is wanted once a tick, and a collider would mean a
  broad-phase entry and a contact event for something that is not solid.

  All of them, not the first. It took one per tick, which is invisible for a
  coin trail -- the next one is metres away -- and wrong the moment two crates
  sit close enough to drive through together: you took one of them and the
  other stayed on the road behind you.

  Found first, taken second, and the finding is forced before anything is
  taken. `for` is lazy and chunked, so realising it while collecting would
  consume up to thirty-two crates the car never reached."
  [ps x z]
  (let [r2 (* reach reach)
        hits (vec (for [[key ps'] (:chunks @ps)
                        p ps'
                        :let [dx (- (:x p) x) dz (- (:z p) z)]
                        :when (< (+ (* dx dx) (* dz dz)) r2)]
                    [key p]))]
    (mapv (fn [[key p]]
            (take-index! ps key (:idx p))
            {:kind (:kind p)
             :delta {:cx (first key) :cz (second key) :index (:idx p)}})
          hits)))

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
    ;; A zero timer means it happens now and is over: the repair, and the two
    ;; that are simply points. Keying on that rather than on `:repair` by name
    ;; is what let coins arrive without another branch here.
    (if (pos? secs)
      (do (swap! ps assoc-in [:active name] secs)
          (effect! veh kind true))
      (when (= :repair name) (vehicle/repair! veh 0.45)))
    label))

(defn- segment!
  "One straight piece of an arc, as a unit box stretched between two points."
  [ps slot ax ay az bx by bz]
  (let [{:keys [arc-pool ^js m4 ^js va ^js vb ^js dir ^js sc ^js q]} @ps]
    (.set va ax ay az)
    (.set vb bx by bz)
    (.subVectors dir vb va)
    (let [len (.length dir)]
      (when (> len 1e-4)
        (.divideScalar dir len)
        ;; The box's own length runs along Z, so this is the rotation taking Z
        ;; onto the segment. Cheaper and shorter than building the basis by
        ;; hand, and it cannot get the handedness wrong.
        (.setFromUnitVectors q z-axis dir)
        (.lerp vb va 0.5)                       ; vb becomes the midpoint
        (.set sc arc-thick arc-thick len)
        (.compose m4 vb q sc)
        (fig/set-matrix! arc-pool slot m4)))))

(defn arc!
  "Draw a bolt from the car to something it just hit, for a fifth of a second.

  Called by whoever decided what was hit -- this namespace is about what is
  held, and where the lightning lands is a gameplay question."
  [ps [fx fy fz] [tx ty tz] now-s]
  (let [pool (:arc-pool @ps)
        dx (- tx fx) dy (- ty fy) dz (- tz fz)
        span (js/Math.hypot dx dz)
        ;; Sideways in the ground plane, so the kink is visible from a car
        ;; rather than only from above.
        px (if (pos? span) (/ (- dz) span) 1.0)
        pz (if (pos? span) (/ dx span) 0.0)
        j (* arc-kink span)
        ;; Two interior points, jagged opposite ways. A deterministic wobble
        ;; off the endpoint keeps twenty simultaneous bolts from being twenty
        ;; copies of the same shape.
        w (- (mod (* 0.618 (+ (js/Math.abs tx) (js/Math.abs tz))) 1.0) 0.5)
        pt (fn [t s]
             [(+ fx (* t dx) (* s j px))
              (+ fy (* t dy) (* 0.35 j (- 0.5 (js/Math.abs (- t 0.5)))))
              (+ fz (* t dz) (* s j pz))])
        [ax ay az] (pt 0.34 (+ 0.6 w))
        [bx by bz] (pt 0.68 (- -0.6 w))
        slots (js/Int32Array. arc-segments)]
    (dotimes [i arc-segments] (aset slots i (fig/claim! pool)))
    (segment! ps (aget slots 0) fx fy fz ax ay az)
    (segment! ps (aget slots 1) ax ay az bx by bz)
    (segment! ps (aget slots 2) bx by bz tx ty tz)
    (swap! ps update :arcs conj {:slots slots :t0 now-s})
    nil))

(def ^:private crackle-arcs 3)
(def ^:private crackle-r 3.4)

(defn- crackle!
  "Short bolts to the ground around the car, so a held shock is visible whether
  or not there is anything near enough to kill."
  [ps x y z now-s]
  (dotimes [i crackle-arcs]
    (let [a (* 6.283185307179586 (/ (+ i (* 3.0 (mod now-s 1.0))) crackle-arcs))
          d (* crackle-r (+ 0.55 (* 0.45 (mod (* 7.3 now-s) 1.0))))]
      (arc! ps [x (+ y 0.4) z]
            [(+ x (* d (js/Math.cos a))) (- y 0.5) (+ z (* d (js/Math.sin a)))]
            now-s))))

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
          ;; A few bolts to nothing in particular, before anything is asked
          ;; about what was hit. The shock only existed on the frames it
          ;; happened to kill something, so on an empty street it was invisible
          ;; and indistinguishable from not having picked it up -- which is
          ;; most of why it read as missing. Now it is visibly armed.
          (crackle! ps x y z (* 0.001 (js/Date.now)))
          (emit [:shock x y z shock-reach]))))))

(defn- age-arcs!
  "Release every bolt that has burnt out. Returns true if anything changed, so
  `sync!` only re-uploads the buffer on the frames a bolt appeared or went."
  [ps now-s]
  (let [{:keys [arcs arc-pool]} @ps
        dead (filter #(> (- now-s (:t0 %)) arc-life) arcs)]
    (when (seq dead)
      (doseq [{:keys [^js slots]} dead]
        (dotimes [i arc-segments] (fig/release! arc-pool (aget slots i))))
      (swap! ps update :arcs (fn [v] (vec (remove #(> (- now-s (:t0 %)) arc-life) v))))
      true)))

(defn active [ps] (:active @ps))

(defn bars
  "What is held, as `[[name seconds-left fraction] ...]`.

  The fraction is worked out here because only this namespace knows how long
  each one runs for. The cluster used to divide by a hardcoded fourteen, which
  was right for exactly one power-up and drew a bar over full for the rest the
  moment the durations changed."
  [ps]
  (vec (for [[k left] (:active @ps)
             :let [full (:secs (nth kinds (index-of-name k)) 1.0)]]
         [k left (max 0.0 (min 1.0 (/ left full)))])))

(defn label-for [k]
  (:label (first (filter #(= k (:name %)) kinds))))

(defn sync!
  "Spin them. A pickup that sits still on a grey road is a pickup nobody sees."
  [ps now-s]
  (let [{:keys [pools ^js m4 chunks]} @ps]
    (doseq [[_ ps'] chunks
            {:keys [x y z slot glow-slot kind shape]} ps']
      (let [a (+ (* 1.6 now-s) (* 0.7 kind))
            bob (* 0.18 (js/Math.sin (+ (* 2.2 now-s) kind)))
            size (:scale (nth kinds kind) 1.0)
            ;; The halo breathes against the solid one rather than with it,
            ;; which is what makes the pair read as shining instead of as two
            ;; overlapping shapes.
            pulse (* size (+ 1.34 (* 0.12 (js/Math.sin (+ (* 3.1 now-s) kind)))))
            {:keys [solid glow]} (get pools shape)
            c (js/Math.cos a) s (js/Math.sin a)
            ^js e (.-elements m4)
            write! (fn [k target slot]
                     ;; Yaw by hand rather than through an Object3D: one pickup
                     ;; is nine stores and a matrix write, and there is no other
                     ;; transform involved.
                     (aset e 0 (* k c)) (aset e 1 0.0) (aset e 2 (* k (- s))) (aset e 3 0.0)
                     (aset e 4 0.0) (aset e 5 k) (aset e 6 0.0) (aset e 7 0.0)
                     (aset e 8 (* k s)) (aset e 9 0.0) (aset e 10 (* k c)) (aset e 11 0.0)
                     (aset e 12 x) (aset e 13 (+ y bob)) (aset e 14 z) (aset e 15 1.0)
                     (fig/set-matrix! target slot m4))]
        (write! size solid slot)
        (write! pulse glow glow-slot)))
    (doseq [[_ {:keys [solid glow]}] pools]
      (fig/flush! solid)
      (fig/flush! glow)))
  ;; Bolts are written once, when they are struck. All this has to do is take
  ;; away the ones that have burnt out -- and only tell the GPU when one has.
  (when (age-arcs! ps now-s) (fig/flush! (:arc-pool @ps))))

(defn stats [ps]
  {:live (reduce + (map count (vals (:chunks @ps))))
   :taken (:taken @ps)
   :active (count (:active @ps))})
