(ns carmageddon.client.weather
  "Sky, rain, and what a wet road does to a car.

  The weather is a pure function of the world's seed and the wall clock, so
  everyone in a room is standing in the same downpour without a byte crossing
  the wire. It is the same trick the world itself uses: the seed says what,
  the clock says when, and nothing has to be synchronised because nothing is
  being decided locally.

  Grip is the reason this is not just a filter over the picture. A wet road is
  a different car -- longer braking, earlier slides, and a corner that was
  comfortable at 70 taken at 55 instead. The surface multiplier is a property
  of the world acting on the vehicle, which is why it lives beside the vehicle's
  damage and boosts rather than inside the driver's command.

  Roads do not dry the instant the rain stops. Wetness chases the rain quickly
  upward and slowly down, so the twenty seconds after a squall are the
  interesting ones: it looks clear and it still is not."
  (:require ["three" :as three]
            [carmageddon.client.figures :as fig]
            [carmageddon.shared.noise :as noise]))

;; Roughly four minutes from one front to the next. Long enough that a run of
;; ninety seconds sees one kind of weather and remembers it, short enough that
;; two runs are not the same.
(def ^:private period 230.0)

(def ^:private drops 1400)
(def ^:private drop-box 26.0)     ; the cube of rain carried around the camera

;; Grip on a soaked road, as a fraction of dry. 0.78 is a real number for a
;; wet asphalt surface and it is also, measured on the skidpad, the difference
;; between a car that rotates on the throttle and one that does not.
(def ^:private wet-grip 0.78)

(defn rain-at
  "How hard it is raining at time `t`, 0 to 1. Pure, shared, unsynchronised."
  [seed t]
  (let [;; Two octaves over one axis of the same value noise the terrain uses.
        ;; Biased so that most of the time it is not raining -- weather that is
        ;; always doing something is weather nobody notices.
        n (noise/fbm2d (+ seed 5501) (/ t period) 0.37 2)]
    (max 0.0 (min 1.0 (* 2.6 (- n 0.55))))))

(defn cloud-at
  "How overcast it is, 0 to 1. Runs ahead of the rain, because the sky darkens
  before it starts and clears after it stops."
  [seed t]
  (let [n (noise/fbm2d (+ seed 5501) (+ 0.12 (/ t period)) 0.37 2)]
    (max 0.0 (min 1.0 (* 1.9 (- n 0.42))))))

(defn create [^js scene ^js sun ^js renderer seed]
  (let [mat (three/MeshBasicMaterial. #js {:color 0xcfe0ee :transparent true
                                           :opacity 0.75 :depthWrite false})
        ;; The sky fill, found by name rather than passed in: `update!` raises
        ;; it as it takes the sun away, which is what an overcast sky actually
        ;; does. Without it, dimming the sun and the exposure together turned a
        ;; dark car into a hole in the road.
        ^js fill (.getObjectByName scene "fill")]
    (atom {:scene scene
           :sun sun
           :fill fill
           :fill0 (when fill (.-intensity fill))
           :renderer renderer
           :seed seed
           ;; Thin vertical slivers. A raindrop is one of the few things in
           ;; this game that genuinely is a stretched box.
           :pool (fig/pool scene (three/BoxGeometry. 1 1 1) mat drops {:cast? false})
           :m4 (three/Matrix4.)
           :wet 0.0
           :rain 0.0
           :cloud 0.0
           ;; Where each drop sits inside the moving box, and how fast it
           ;; falls. Fixed at build time; the box moves, the drops recycle.
           :ox (js/Float32Array. drops)
           :oy (js/Float32Array. drops)
           :oz (js/Float32Array. drops)
           :ov (js/Float32Array. drops)
           :seeded? false
           ;; The clear-weather values, kept so they can be returned to.
           :sun0 (.-intensity sun)
           :exposure0 (.-toneMappingExposure renderer)
           :fog0 (when-let [f (.-fog scene)] (.-far f))})))

(defn- seed-drops! [ws]
  (let [{:keys [^js ox ^js oy ^js oz ^js ov]} @ws]
    (dotimes [i drops]
      (aset ox i (* drop-box (- (js/Math.random) 0.5)))
      (aset oy i (* drop-box (js/Math.random)))
      (aset oz i (* drop-box (- (js/Math.random) 0.5)))
      (aset ov i (+ 22.0 (* 12.0 (js/Math.random)))))
    (swap! ws assoc :seeded? true)))

(defn update!
  "Advance the weather and push it into the scene. `t` is wall-clock seconds."
  [ws t dt]
  (let [{:keys [seed ^js sun ^js fill ^js renderer scene wet sun0 fill0 fog0
                exposure0]} @ws
        rain (rain-at seed t)
        cloud (cloud-at seed t)
        ;; Up fast, down slow. Chasing the rain symmetrically would mean the
        ;; road were dry the moment the last drop fell, which is not how any
        ;; road behaves and throws away the most interesting part of a shower.
        k (if (> rain wet) (* 0.85 dt) (* 0.055 dt))
        wet' (+ wet (* (min 1.0 k) (- rain wet)))]
    (swap! ws assoc :wet wet' :rain rain :cloud cloud)
    ;; The sun goes out before the rain arrives and comes back after it leaves.
    (set! (.-intensity sun) (* sun0 (- 1.0 (* 0.50 cloud))))
    ;; And the sky takes over. A cloud layer is a diffuser, not a lid: it kills
    ;; the key light and the hard shadow and replaces both with a bright, flat
    ;; fill. Modelling only the first half is what made a storm read as night.
    (when fill (set! (.-intensity fill) (* fill0 (+ 1.0 (* 0.35 cloud)))))
    (when-let [^js fog (.-fog scene)]
      (set! (.-far fog) (* fog0 (- 1.0 (* 0.45 rain))))
      (.setHex (.-color fog) (if (> cloud 0.35) 0x8d95a0 0xbdd6e8)))
    ;; The background is a sky *texture*, not a colour, so it cannot be tinted
    ;; the way the fog can. Exposure is dimmed instead: one number, and it
    ;; takes the sky, the buildings and the road down together -- which is what
    ;; an overcast day actually does to a scene. Dimming only the sun left a
    ;; storm brightly lit from behind a cloud that was not there.
    ;;
    ;; The dim is now 0.30 rather than 0.45, and it is the *last* of three
    ;; things taking light out of the scene rather than one of two. Overcast
    ;; should look overcast; it should not make a navy hatchback invisible.
    (set! (.-toneMappingExposure renderer) (* exposure0 (- 1.0 (* 0.30 cloud))))
    wet'))

(defn wetness [ws] (:wet @ws))
(defn raining? [ws] (> (:rain @ws) 0.05))

(defn gloom
  "How dark it is: 0 when nobody would have their lights on, and 0.25 to 1
  when they would.

  One number rather than a flag and a level, because everything downstream
  wants both. Whether a lamp is lit is `(pos? gloom)`; how far a headlight beam
  carries is the value itself -- a beam drawn at full strength in light
  overcast is a grey wedge lying on a sunlit road, and the only thing that
  fixes that is for it to be nearly invisible until the sky is genuinely dark.

  There is no night in this world yet, so the trigger is the weather rather
  than the clock. That is not a compromise: under a front the sun is halved,
  the exposure is down a third and the fog has closed to two thirds -- exactly
  the conditions in which a car without lights is hard to see."
  [ws]
  (let [{:keys [cloud rain]} @ws]
    (if (or (> cloud 0.38) (> rain 0.08))
      (max 0.25 (min 1.0 (max cloud (* 1.4 rain))))
      0.0)))

(defn lights-on? [ws] (pos? (gloom ws)))

(defn grip-scale
  "What the surface is worth right now, as a multiple of dry grip."
  [ws]
  (+ wet-grip (* (- 1.0 wet-grip) (- 1.0 (:wet @ws)))))

(defn sync!
  "Fall the rain. The box of drops is centred on the car and wraps, so a
  thousand of them cover everywhere anyone can see for the price of a thousand
  instances rather than a world's worth."
  [ws x y z t]
  (when-not (:seeded? @ws) (seed-drops! ws))
  (let [{:keys [pool ^js m4 rain ^js ox ^js oy ^js oz ^js ov]} @ws
        n (js/Math.round (* drops (min 1.0 (* 1.3 rain))))
        ^js e (.-elements m4)]
    (dotimes [i drops]
      (if (>= i n)
        (fig/set-matrix! pool i (doto m4 (.makeScale 0 0 0)))
        (let [;; Height is a sawtooth in time, so no per-drop state is written
              ;; and the whole field is a function of the clock.
              h (mod (- (aget oy i) (* (aget ov i) t)) drop-box)]
          (.makeScale m4 0.028 0.58 0.028)
          (aset e 12 (+ x (aget ox i)))
          ;; From just under the car to the top of the box. The first version
          ;; started the column at the height it was *given* rather than at the
          ;; ground, and every drop fell in a band ten metres over the roof.
          (aset e 13 (+ y -1.5 h))
          (aset e 14 (+ z (aget oz i)))
          (fig/set-matrix! pool i m4))))
    (fig/flush! pool)))

(defn stats [ws]
  {:rain (:rain @ws) :cloud (:cloud @ws) :wet (:wet @ws)})

(defn label
  "One word for the dashboard."
  [ws]
  (let [{:keys [rain cloud wet]} @ws]
    (cond (> rain 0.55) "STORM"
          (> rain 0.12) "RAIN"
          (> wet 0.25) "WET"
          (> cloud 0.45) "OVERCAST"
          :else nil)))
