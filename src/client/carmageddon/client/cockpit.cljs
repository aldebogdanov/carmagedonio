(ns carmageddon.client.cockpit
  "The instrument cluster.

  Everything the player has to read while driving, drawn as instruments rather
  than printed as a sentence. The old HUD was one line of eighteen labelled
  numbers across the top of the screen, which meant that reading your speed
  cost as much attention as reading your damage, and both cost as much as
  reading the chunk count -- so in practice nobody read any of them.

  An instrument is worth the pixels when its *shape* carries the value: a
  needle's angle is speed without a number attached to it, and a car diagram
  with one corner going red says where the damage is faster than the word
  `front` can. Anything without a shape -- the frame rate, the chunk counts --
  is diagnostics and stays as small dim text somewhere else.

  Canvas rather than DOM for the same reason the map is: this repaints every
  frame, and twenty elements' worth of layout and style recalculation per frame
  is not free."
  (:require [carmageddon.shared.rules :as rules]))

(def ^:private w 560)
(def ^:private h 152)

(def ^:private ink   "#e8e4d8")
(def ^:private dim   "rgba(232,228,216,0.45)")
(def ^:private panel "rgba(14,16,20,0.74)")
(def ^:private amber "#f0a830")
(def ^:private good  "#57b85b")
(def ^:private bad   "#d8443a")

(defn create []
  (let [^js canvas (js/document.getElementById "cockpit")]
    (when canvas
      (let [dpr (min 2 (or (.-devicePixelRatio js/window) 1))]
        (set! (.-width canvas) (* w dpr))
        (set! (.-height canvas) (* h dpr))
        (let [^js ctx (.getContext canvas "2d")]
          ;; One scale at build time rather than per draw: everything below is
          ;; written in the 560x152 design space and stays crisp on a retina
          ;; display without a single number changing.
          (.scale ctx dpr dpr)
          ;; A line that appears for a moment when something happens. Held
          ;; here rather than passed in: it is about the *dashboard*, not about
          ;; the simulation, and nothing else needs to know it exists.
          {:canvas canvas :ctx ctx :flash (volatile! nil)})))))

(defn flash!
  "Show `text` on the cluster for a couple of seconds."
  [{:keys [flash]} text]
  (when flash (vreset! flash {:text text :until (+ (js/Date.now) 2200)})))

(defn- text!
  [^js ctx s x y font colour align]
  (set! (.-font ctx) font)
  (set! (.-fillStyle ctx) colour)
  (set! (.-textAlign ctx) align)
  (.fillText ctx s x y))

(def ^:private mono "ui-monospace, SFMono-Regular, Menlo, monospace")

(defn- dial!
  "The speedometer: a 240-degree sweep, ticks every 20 km/h, and a needle.

  The scale comes from the vehicle rather than being fixed, so the needle sits
  in the same part of the dial at the tractor's 40 km/h as at the muscle car's
  165. A shared scale would leave the tractor's needle pinned at the bottom
  left and tell the driver nothing."
  [^js ctx cx cy r kmh top-kmh]
  (let [a0 (* js/Math.PI 0.75)
        sweep (* js/Math.PI 1.5)
        top (max 40.0 top-kmh)
        ang (fn [v] (+ a0 (* sweep (min 1.0 (max 0.0 (/ v top))))))]
    ;; The face.
    (set! (.-strokeStyle ctx) "rgba(232,228,216,0.22)")
    (set! (.-lineWidth ctx) 8)
    (.beginPath ctx)
    (.arc ctx cx cy r a0 (+ a0 sweep))
    (.stroke ctx)
    ;; How much of it is being used, which is the bit you see out of the corner
    ;; of your eye.
    (set! (.-strokeStyle ctx) (if (> kmh (* 0.85 top)) bad amber))
    (set! (.-lineWidth ctx) 8)
    (.beginPath ctx)
    (.arc ctx cx cy r a0 (ang kmh))
    (.stroke ctx)
    ;; Ticks, at whatever interval keeps them countable.
    (let [step (cond (> top 140) 40 (> top 80) 20 :else 10)]
      (set! (.-strokeStyle ctx) dim)
      (set! (.-lineWidth ctx) 1.5)
      (doseq [v (range 0 (inc (int top)) step)]
        (let [a (ang v)
              c (js/Math.cos a) s (js/Math.sin a)]
          (.beginPath ctx)
          (.moveTo ctx (+ cx (* c (- r 9))) (+ cy (* s (- r 9))))
          (.lineTo ctx (+ cx (* c (- r 2))) (+ cy (* s (- r 2))))
          (.stroke ctx))))
    ;; Needle.
    (let [a (ang kmh)
          c (js/Math.cos a) s (js/Math.sin a)]
      (set! (.-strokeStyle ctx) ink)
      (set! (.-lineWidth ctx) 2.5)
      (.beginPath ctx)
      (.moveTo ctx (- cx (* c 6)) (- cy (* s 6)))
      (.lineTo ctx (+ cx (* c (- r 6))) (+ cy (* s (- r 6))))
      (.stroke ctx))
    (set! (.-fillStyle ctx) ink)
    (.beginPath ctx)
    (.arc ctx cx cy 3.5 0 6.2832)
    (.fill ctx)
    (text! ctx (str (js/Math.round kmh)) cx (+ cy 26) (str "600 26px " mono) ink "center")
    (text! ctx "km/h" cx (+ cy 40) (str "9px " mono) dim "center")))

(defn- damage-car!
  "A plan view of the car with each panel shaded by how bent it is.

  This is the instrument the whole four-panel damage model was waiting for. A
  percentage per panel is four numbers to read and compare; a diagram with one
  corner going red is one glance."
  [^js ctx x y [front rear left right] total]
  (let [bw 52 bh 72
        shade (fn [v] (str "rgba(" (js/Math.round (+ 70 (* 165 v))) ","
                           (js/Math.round (- 168 (* 130 v))) ","
                           (js/Math.round (- 90 (* 30 v))) ",0.92)"))
        quad (fn [px py qw qh v]
               (set! (.-fillStyle ctx) (shade v))
               (.fillRect ctx px py qw qh))]
    ;; Front and rear take the top and bottom thirds, the flanks the middle
    ;; band's sides -- which is roughly what a panel beater would draw.
    (quad x y bw 22 front)
    (quad x (+ y 22) 16 28 left)
    (quad (+ x (- bw 16)) (+ y 22) 16 28 right)
    (quad x (+ y 50) bw 22 rear)
    ;; The roof, which is never damaged, so the outline reads as a car seen
    ;; from above rather than as four bars.
    (set! (.-fillStyle ctx) "rgba(20,22,26,0.8)")
    (.fillRect ctx (+ x 16) (+ y 22) (- bw 32) 28)
    (set! (.-strokeStyle ctx) "rgba(0,0,0,0.6)")
    (set! (.-lineWidth ctx) 1)
    (.strokeRect ctx (+ x 0.5) (+ y 0.5) bw bh)
    (text! ctx (str (js/Math.round (* 100 total)) "%")
           (+ x (/ bw 2)) (+ y bh 14) (str "600 13px " mono)
           (if (> total 0.6) bad ink) "center")
    (text! ctx "damage" (+ x (/ bw 2)) (+ y bh 26) (str "9px " mono) dim "center")))

(defn- lamp!
  "A tell-tale. Dark until it means something, like the ones on a real dash."
  [^js ctx x y label on? colour]
  (set! (.-fillStyle ctx) (if on? colour "rgba(232,228,216,0.10)"))
  (.fillRect ctx x y 44 15)
  (text! ctx label (+ x 22) (+ y 11) (str "600 9px " mono)
         (if on? "#12141a" dim) "center"))

(def ^:private powerups-label
  {:nitro "NITRO" :grip "GRIP" :armour "ARMOUR" :flame "FIRETRAIL" :shock "SHOCK"})

(defn draw!
  "Repaint the cluster. Called every frame: it is a speedometer."
  [{:keys [^js ctx flash]}
   {:keys [kmh top-kmh gear panels damage remaining score peds target
           rivals wheels drift? handbrake? online car state powerups weather grip]}]
  (when ctx
    (.clearRect ctx 0 0 w h)
    ;; The bezel.
    (set! (.-fillStyle ctx) panel)
    (.beginPath ctx)
    (.roundRect ctx 0 0 w h 10)
    (.fill ctx)
    (set! (.-strokeStyle ctx) "rgba(232,228,216,0.12)")
    (set! (.-lineWidth ctx) 1)
    (.stroke ctx)

    (dial! ctx 74 74 52 kmh top-kmh)

    ;; Gear and tell-tales.
    (set! (.-fillStyle ctx) "rgba(232,228,216,0.08)")
    (.fillRect ctx 142 28 40 44)
    (text! ctx gear 162 60 (str "700 30px " mono)
           (if (= "R" gear) amber ink) "center")
    (lamp! ctx 140 82 "AIR" (< wheels 2) amber)
    (lamp! ctx 140 100 "DRIFT" drift? amber)
    (lamp! ctx 140 118 "HAND" handbrake? bad)

    (damage-car! ctx 200 26 panels damage)

    ;; The clock, which is the thing that ends the run.
    (let [low? (< remaining 15.0)]
      (text! ctx (.toFixed remaining 1) 300 58 (str "700 34px " mono)
             (cond (= :lost state) bad low? bad :else ink) "left")
      (text! ctx "seconds left" 302 74 (str "9px " mono) dim "left"))

    ;; The tally.
    (text! ctx (str score) 300 106 (str "700 20px " mono) amber "left")
    (text! ctx "points" 302 120 (str "9px " mono) dim "left")

    (text! ctx (str peds "/" target) 420 106 (str "700 20px " mono)
           (if (>= peds target) good ink) "left")
    (text! ctx "pedestrians" 422 120 (str "9px " mono) dim "left")

    (text! ctx (str rivals) 420 58 (str "700 20px " mono)
           (if (zero? rivals) good ink) "left")
    (text! ctx "rivals left" 422 74 (str "9px " mono) dim "left")

    (text! ctx car (- w 14) 22 (str "600 12px " mono) dim "right")
    ;; What the sky is doing, and what it has left on the road. The grip figure
    ;; is the one that matters: it is why the corner you took last lap does not
    ;; work this one.
    ;; Bottom left of the cluster, not the top right. Right-aligned against the
    ;; bezel it ran straight through "rivals left", which sits at x 420 -- the
    ;; two were legible only when the weather was clear enough to say nothing.
    (when weather
      (text! ctx (str weather " \u00b7 grip " (.toFixed (* 100 (or grip 1.0)) 0) "%")
             200 148 (str "600 11px " mono)
             (if (< (or grip 1.0) 0.9) bad dim) "left"))
    ;; Who else is here. Only when there is somebody: an instrument that always
    ;; reads zero is a label.
    (when (pos? (or online 0))
      (text! ctx (str "\u25cf " online " online") (- w 14) 38
             (str "600 11px " mono) good "right"))

    ;; Whatever is being held, and for how much longer. Bars rather than
    ;; numbers: what matters is that it is about to run out, not that it has
    ;; 3.4 seconds left.
    (let [ps (seq powerups)]
      (dotimes [i (count ps)]
        (let [[k secs] (nth ps i)
              x (+ 300 (* i 86))]
          (set! (.-fillStyle ctx) "rgba(232,228,216,0.10)")
          (.fillRect ctx x 128 78 12)
          (set! (.-fillStyle ctx) amber)
          (.fillRect ctx x 128 (* 78 (min 1.0 (/ secs 14.0))) 12)
          (text! ctx (or (powerups-label k) (str k)) (+ x 39) 137
                 (str "600 8px " mono) "#12141a" "center"))))

    (when-let [{:keys [text until]} (and flash @flash)]
      (when (> until (js/Date.now))
        (text! ctx text 74 148 (str "700 13px " mono) amber "center")))

    (case state
      :won  (text! ctx "WON" (- w 14) 140 (str "700 16px " mono) good "right")
      :lost (text! ctx "OUT OF TIME" (- w 14) 140 (str "700 16px " mono) bad "right")
      nil)))

(defn state-of
  "Everything the cluster needs, as one value.

  Assembled by the caller rather than reached for here: the cockpit knows how
  to draw a dashboard and deliberately nothing about where a simulation keeps
  its wheels."
  [{:keys [speed top-speed panels damage game rivals wheels slip handbrake?
           online car powerups weather grip]}]
  (let [{:keys [remaining score peds state]} game]
    {:kmh       (js/Math.abs (* 3.6 speed))
     :top-kmh   (* 3.6 top-speed)
     ;; An automatic has three positions and the driver only ever needs to know
     ;; which way the car will go when they press the pedal.
     :gear      (cond (> speed 0.8) "D" (< speed -0.8) "R" :else "N")
     :panels    panels
     :damage    damage
     :remaining remaining
     :score     score
     :peds      peds
     :target    rules/target-kills
     :rivals    rivals
     :wheels    wheels
     :drift?    (> slip 22.0)
     :handbrake? handbrake?
     :online    online
     :powerups  powerups
     :weather   weather
     :grip      grip
     :car       car
     :state     state}))
