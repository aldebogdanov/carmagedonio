(ns carmageddon.client.camera
  "Switchable follow cameras.

  Kept out of `render` because a camera is less a view of the scene than a small
  simulation of its own: it carries state that has to survive between frames
  (smoothed heading, orbit angles, current mode) and it has input of its own.
  Folding that into the draw path made both harder to follow.

  The chase views smooth *heading*, not position. The camera this replaces was
  yaw-locked, on the grounds that a behind-the-car view is nauseating -- but the
  nausea comes from yaw chasing every twitch of the steering, not from tracking
  yaw at all. Damping the angle removes it and buys back the one thing a chase
  camera is for: showing where you are going rather than which way is north."
  (:require ["three" :as three]))

(def ring
  "Cycle order for the mode key."
  [:chase :chase-far :hood :top :orbit])

(def labels
  {:chase "chase" :chase-far "far" :hood "hood" :top "top" :orbit "orbit"})

;; Precomputed rather than found by index: `ring` is a vector of keywords and
;; identity comparison on keywords is not something to rely on.
(def ^:private next-mode
  (zipmap ring (concat (rest ring) [(first ring)])))

;; `dist`/`rise` are metres behind and above the car, `aim` lifts the look-at
;; point off the road so the horizon is not centred, and `ahead` pushes it in
;; front of the car -- which is what makes a chase camera read as "looking where
;; you are going" rather than "staring at a roof".
(def ^:private rigs
  {:chase     {:dist 10.0 :rise 3.8 :aim 1.2 :ahead 4.5 :fov 68.0}
   :chase-far {:dist 18.0 :rise 7.5 :aim 1.8 :ahead 6.5 :fov 62.0}
   :hood      {:fov 80.0}
   :top       {:rise 46.0 :fov 45.0}
   :orbit     {:fov 65.0}})

;; Speed response. Pulling back and widening the lens with speed is the oldest
;; trick in the driving-game book and it is worth the four lines: it conveys
;; velocity far more cheaply than any amount of motion blur.
(def ^:private speed-ref 42.0)      ; m/s at which the stretch is fully applied
(def ^:private dist-stretch 0.38)   ; +38% camera distance
(def ^:private fov-stretch 13.0)    ; +13 degrees

(def ^:private yaw-rate 6.0)        ; heading smoothing, 1/s
(def ^:private pos-rate 9.0)        ; position smoothing, 1/s

(def ^:private orbit-min-dist 5.0)
(def ^:private orbit-max-dist 90.0)
(def ^:private orbit-max-pitch 1.45)   ; just under vertical, where yaw degenerates

(def ^:private collide-margin 0.45) ; keep the near plane clear of the wall
(def ^:private collide-min 2.2)     ; never end up inside the car

(defn- ang-lerp
  "Interpolate between angles the short way round.

  Lerping the raw numbers makes the camera swing the long way through 2*pi every
  time the car's heading crosses the -pi/pi seam -- which happens once a lap and
  is unmissable when it does."
  [a b t]
  (let [d (- b a)
        d (js/Math.atan2 (js/Math.sin d) (js/Math.cos d))]
    (+ a (* d t))))

(defn- approach
  "Frame-rate independent smoothing factor for `dt` seconds at `rate` per second.

  A fixed per-frame lerp (the 0.08 this replaces) makes the camera lag twice as
  far at 30 fps as at 60, so the feel of the car changes with the frame rate."
  [rate dt]
  (- 1.0 (js/Math.exp (- (* rate dt)))))

(defn- heading
  "Yaw of a quaternion about +Y, taken from where the car's nose points.

  Local forward is -Z, matching the chassis box and `sim/forward-vector`; the
  two must agree or the camera sits on the bonnet."
  [qx qy qz qw]
  (js/Math.atan2 (* -2.0 (+ (* qx qz) (* qw qy)))
                 (- (* 2.0 (+ (* qx qx) (* qy qy))) 1.0)))

(defn create!
  "Camera state. `camera` is the three.js PerspectiveCamera from `render/create!`."
  [^js camera]
  {:camera    camera
   :mode      (volatile! :chase)
   :yaw       (volatile! 0.0)
   :fov       (volatile! (.-fov camera))
   :look-back (volatile! false)
   :primed    (volatile! false)
   :orbit     (volatile! {:yaw 0.0 :pitch 0.42 :dist 16.0})
   :dragging  (volatile! false)
   ;; Scratch, reused every frame rather than allocated. The draw path runs at
   ;; display rate and three.js vectors are ordinary objects.
   :pos   (three/Vector3.)
   :look  (three/Vector3.)
   :tmp   (three/Vector3.)
   :tmpq  (three/Quaternion.)
   :up-y  (three/Vector3. 0 1 0)
   ;; Top-down wants north up the screen, not an arbitrary roll: with a +Y up
   ;; vector and a camera looking straight down, `lookAt` is degenerate.
   :up-n  (three/Vector3. 0 0 -1)})

(defn mode [{:keys [mode]}] @mode)

(defn cycle-mode!
  "Advance to the next view. Un-primes the smoothing so the new view snaps into
  place rather than sliding in from wherever the last one was looking."
  [{:keys [mode primed]}]
  (vreset! mode (next-mode @mode))
  (vreset! primed false)
  @mode)

(defn attach!
  "Wire up the camera's own controls. Returns a detach fn.

  These deliberately do not go through `input/sample`: a `Command` is what the
  simulation consumes, and it must stay identical whether it came from a human,
  the AI or the network. Where the local player happens to be pointing a camera
  is presentation, and putting it in the command would send it over the wire."
  [cs ^js canvas]
  (let [{:keys [look-back orbit dragging]} cs
        on-key-down (fn [^js e]
                      (case (.-code e)
                        "KeyC" (do (.preventDefault e) (cycle-mode! cs))
                        "KeyB" (vreset! look-back true)
                        nil))
        on-key-up   (fn [^js e]
                      (when (= "KeyB" (.-code e)) (vreset! look-back false)))
        on-down     (fn [^js e]
                      (when (= :orbit @(:mode cs))
                        (vreset! dragging true)
                        (.setPointerCapture canvas (.-pointerId e))))
        on-up       (fn [^js e]
                      (vreset! dragging false)
                      (when (.hasPointerCapture canvas (.-pointerId e))
                        (.releasePointerCapture canvas (.-pointerId e))))
        on-move     (fn [^js e]
                      (when @dragging
                        (vswap! orbit
                                (fn [o]
                                  (-> o
                                      (update :yaw + (* 0.006 (.-movementX e)))
                                      (update :pitch
                                              #(-> (+ % (* 0.006 (.-movementY e)))
                                                   (max (- orbit-max-pitch))
                                                   (min orbit-max-pitch))))))))
        on-wheel    (fn [^js e]
                      (when (= :orbit @(:mode cs))
                        (.preventDefault e)
                        (vswap! orbit update :dist
                                #(-> (* % (js/Math.exp (* 0.0012 (.-deltaY e))))
                                     (max orbit-min-dist)
                                     (min orbit-max-dist)))))
        ;; Focus can be lost mid-hold; without this the world stays reversed.
        on-blur     (fn [_] (vreset! look-back false) (vreset! dragging false))]
    (.addEventListener js/window "keydown" on-key-down)
    (.addEventListener js/window "keyup" on-key-up)
    (.addEventListener js/window "blur" on-blur)
    (.addEventListener canvas "pointerdown" on-down)
    (.addEventListener canvas "pointerup" on-up)
    (.addEventListener canvas "pointermove" on-move)
    (.addEventListener canvas "wheel" on-wheel #js {:passive false})
    (fn detach! []
      (.removeEventListener js/window "keydown" on-key-down)
      (.removeEventListener js/window "keyup" on-key-up)
      (.removeEventListener js/window "blur" on-blur)
      (.removeEventListener canvas "pointerdown" on-down)
      (.removeEventListener canvas "pointerup" on-up)
      (.removeEventListener canvas "pointermove" on-move)
      (.removeEventListener canvas "wheel" on-wheel))))

(defn- set-fov!
  "Retuning the lens rebuilds the projection matrix, so only do it when the
  change would actually be visible."
  [{:keys [^js camera fov]} want]
  (when (> (js/Math.abs (- want @fov)) 0.05)
    (vreset! fov want)
    (set! (.-fov camera) want)
    (.updateProjectionMatrix camera)))

(defn- unobstruct!
  "Pull the camera in until nothing solid lies between it and the car.

  Without this the first building we drive past swallows the view. `cast`
  excludes the player's own body, so the chassis is not treated as an occluder."
  [^js pos cx cy cz cast]
  (when cast
    (let [dx (- (.-x pos) cx) dy (- (.-y pos) cy) dz (- (.-z pos) cz)
          d  (js/Math.sqrt (+ (* dx dx) (* dy dy) (* dz dz)))]
      (when (> d 0.01)
        (let [nx (/ dx d) ny (/ dy d) nz (/ dz d)]
          (when-let [hit (cast cx cy cz nx ny nz d)]
            (let [d' (max collide-min (- hit collide-margin))]
              (.set pos (+ cx (* nx d')) (+ cy (* ny d')) (+ cz (* nz d'))))))))))

(defn update!
  "Place the camera for this frame.

  `cast` is (fn [ox oy oz dx dy dz max] -> hit-distance-or-nil) against the
  physics world, passed in rather than required so this namespace does not
  depend on the simulation. `dt` is the real frame time, not the fixed step:
  the camera is presentation and is allowed to be frame-rate driven."
  [{:keys [^js camera ^js pos ^js look ^js tmp ^js tmpq ^js up-y ^js up-n
           mode yaw primed orbit look-back]
    :as cs}
   px py pz qx qy qz qw speed dt cast]
  (let [m     @mode
        rig   (rigs m)
        boost (min 1.0 (/ (js/Math.abs speed) speed-ref))
        ;; The first frame after a mode change has nothing sensible to smooth
        ;; from, so it snaps.
        snap? (not @primed)
        tp    (if snap? 1.0 (approach pos-rate dt))
        ty    (if snap? 1.0 (approach yaw-rate dt))]
    (vreset! primed true)
    (case m
      :hood
      (do
        (set! (.-x tmpq) qx) (set! (.-y tmpq) qy)
        (set! (.-z tmpq) qz) (set! (.-w tmpq) qw)
        ;; Rigidly mounted: no smoothing at all. Feeling the suspension through
        ;; the camera is the whole point of an in-car view.
        (.set tmp 0.0 0.62 (if @look-back 1.1 -0.55))
        (.applyQuaternion tmp tmpq)
        (.set pos (+ px (.-x tmp)) (+ py (.-y tmp)) (+ pz (.-z tmp)))
        (.set tmp 0.0 0.35 (if @look-back 60.0 -60.0))
        (.applyQuaternion tmp tmpq)
        (.set look (+ px (.-x tmp)) (+ py (.-y tmp)) (+ pz (.-z tmp)))
        (.copy (.-up camera) up-y))

      :top
      (do
        ;; The tiny z nudge keeps `lookAt` out of the degenerate straight-down
        ;; case; the -Z up vector is what puts north at the top of the screen.
        (.set pos px (+ py (:rise rig)) (+ pz 0.001))
        (.set look px py pz)
        (.copy (.-up camera) up-n))

      :orbit
      (let [{oy :yaw op :pitch od :dist} @orbit
            h (* od (js/Math.cos op))]
        (.set pos
              (+ px (* h (js/Math.sin oy)))
              (+ py (* od (js/Math.sin op)) 1.0)
              (+ pz (* h (js/Math.cos oy))))
        (unobstruct! pos px (+ py 0.6) pz cast)
        (.set look px (+ py 0.8) pz)
        (.copy (.-up camera) up-y))

      ;; :chase and :chase-far
      (let [want (heading qx qy qz qw)
            y    (if snap? (vreset! yaw want) (vswap! yaw ang-lerp want ty))
            ;; Look-back snaps rather than swinging round: a smoothed 180 takes
            ;; long enough that by the time you can see behind you it no longer
            ;; matters.
            y    (if @look-back (+ y js/Math.PI) y)
            fx   (js/Math.sin y)
            fz   (js/Math.cos y)
            dist (* (:dist rig) (+ 1.0 (* dist-stretch boost)))
            tx   (- px (* fx dist))
            ty*  (+ py (:rise rig))
            tz   (- pz (* fz dist))]
        (.set pos
              (+ (.-x pos) (* (- tx (.-x pos)) tp))
              (+ (.-y pos) (* (- ty* (.-y pos)) tp))
              (+ (.-z pos) (* (- tz (.-z pos)) tp)))
        (unobstruct! pos px (+ py 0.6) pz cast)
        (.set look
              (+ px (* fx (:ahead rig)))
              (+ py (:aim rig))
              (+ pz (* fz (:ahead rig))))
        (.copy (.-up camera) up-y)))
    (set-fov! cs (+ (:fov rig) (if (#{:chase :chase-far} m)
                                 (* fov-stretch boost)
                                 0.0)))
    (.copy (.-position camera) pos)
    (.lookAt camera look)))
