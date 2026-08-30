(ns carmageddon.client.minimap
  "A map of where you are and what is around you.

  Drawn from `worldgen/area-kind`, which samples the same fields the generator
  does and none of its geometry -- so the map can say what the chunk two
  kilometres away is without that chunk ever being built. Generating it to find
  out would cost about six milliseconds; asking costs four field samples.

  Cells are cached and only ever computed once. Driving one chunk east shifts
  the window by a column, so a boundary crossing costs a dozen new samples
  rather than the whole grid, and standing still costs nothing at all."
  (:require [carmageddon.shared.constants :as k]
            [carmageddon.shared.worldgen :as worldgen]))

(def ^:private radius 4)
;; Chunks either side, so 9 across -- 2.3 km of context. It used to be 13, and
;; that was a better world map than it was a tactical one: at that scale a
;; rival at the end of its leash is eleven pixels from the centre, which is
;; underneath the marker for your own car.
(def ^:private span (inc (* 2 radius)))

(def ^:private colours
  {:water    "#3f6f96"
   :wild     "#5d7a4e"
   :woods    "#3c5c39"
   :farm     "#8a9152"
   :village  "#95845e"
   :suburb   "#9b8f7e"
   :industry "#7c6f66"
   :city     "#8e8b86"
   :downtown "#adaaa4"})

(def ^:private road-colour "rgba(20,20,22,0.55)")
(def ^:private grid-colour "rgba(0,0,0,0.16)")

(defn create [seed]
  (let [^js canvas (js/document.getElementById "map")]
    (when canvas
      {:seed seed
       :canvas canvas
       :ctx (.getContext canvas "2d")
       :label (js/document.getElementById "where")
       ;; [cx cz] -> kind. Never invalidated: a seed's world does not change.
       :cache (js/Map.)
       :misses (volatile! 0)
       :at (volatile! nil)
       ;; Rivals are shown by default and can be switched off. Some players
       ;; would rather be surprised, and a map with four things moving on it
       ;; is a busier read than one with none.
       :show-rivals (volatile! true)})))

(defn attach!
  "The map's own controls. Returns a detach fn.

  Separate from `input/attach!` on purpose, for the same reason the camera's
  controls are: what a local player has chosen to draw on their map is not part
  of the `Command` the simulation consumes and must never reach the wire."
  [{:keys [show-rivals]}]
  (let [on-key (fn [^js e]
                 (when (= "KeyM" (.-code e))
                   (.preventDefault e)
                   (vswap! show-rivals not)))]
    (.addEventListener js/window "keydown" on-key)
    (fn detach! [] (.removeEventListener js/window "keydown" on-key))))

(defn rivals-shown? [{:keys [show-rivals]}] (boolean @show-rivals))

(defn- cache-key
  "Injective for any coordinates anyone will drive to, and a number rather than
  a string because this is asked a few hundred times per redraw. The first
  attempt packed the pair as `cx * 100000 + cz`, which collides the moment `cz`
  runs past that -- two different places would have shared an answer."
  [cx cz]
  (+ (* cx 16777216) (+ cz 8388608)))

(defn- kind-at [{:keys [seed ^js cache misses]} cx cz]
  (let [k (cache-key cx cz)
        hit (.get cache k)]
    (if (undefined? hit)
      (let [v (worldgen/area-kind seed cx cz)]
        (.set cache k v)
        (vswap! misses inc)
        v)
      hit)))

(defn area-here
  "The label for the chunk the player is standing in."
  [ms x z]
  (let [[cx cz] (worldgen/chunk-of x z)]
    (worldgen/area-labels (kind-at ms cx cz))))

;; The map is anchored on the *car*, not on the chunk it happens to be in. That
;; is the whole difference between a map that scrolls and one that jumps: with
;; the cells aligned to the chunk grid, the entire picture steps sideways by a
;; full cell every time a boundary is crossed, and the marker snaps back to the
;; middle. Anchoring on the car means the marker never moves and the world
;; slides under it.

(defn- draw-cells! [{:keys [^js ctx] :as ms} px pz per-m size]
  (let [half (* 0.5 (/ size per-m))            ; metres from centre to edge
        x0 (- px half) z0 (- pz half)
        [c0x c0z] (worldgen/chunk-of x0 z0)
        [c1x c1z] (worldgen/chunk-of (+ px half) (+ pz half))
        cell (* k/chunk-size per-m)]
    (doseq [cx (range c0x (inc c1x))
            cz (range c0z (inc c1z))]
      (let [sx (* (- (* cx k/chunk-size) x0) per-m)
            sz (* (- (* cz k/chunk-size) z0) per-m)]
        (set! (.-fillStyle ctx) (get colours (kind-at ms cx cz) "#000"))
        (.fillRect ctx sx sz (inc cell) (inc cell))))))

(defn- draw-roads!
  "The arterial lattice, straight from the line indices.

  No street is generated to draw this. Whether a lattice line is a main road is
  arithmetic on its index, which is the same reason the generator itself never
  has to be told where the main roads are."
  [{:keys [^js ctx]} px pz per-m size]
  (let [half (* 0.5 (/ size per-m))
        x0 (- px half) z0 (- pz half)
        w  (/ size per-m)
        g0x (long (Math/floor (/ x0 worldgen/street-spacing)))
        g1x (long (Math/ceil (/ (+ x0 w) worldgen/street-spacing)))
        g0z (long (Math/floor (/ z0 worldgen/street-spacing)))
        g1z (long (Math/ceil (/ (+ z0 w) worldgen/street-spacing)))]
    (set! (.-strokeStyle ctx) road-colour)
    (set! (.-lineWidth ctx) 2)
    (.beginPath ctx)
    (doseq [g (range g0x (inc g1x))
            :when (worldgen/arterial-line? g)
            :let [sx (* (- (* g worldgen/street-spacing) x0) per-m)]]
      (.moveTo ctx sx 0) (.lineTo ctx sx size))
    (doseq [g (range g0z (inc g1z))
            :when (worldgen/arterial-line? g)
            :let [sz (* (- (* g worldgen/street-spacing) z0) per-m)]]
      (.moveTo ctx 0 sz) (.lineTo ctx size sz))
    (.stroke ctx)
    ;; A faint chunk grid, so the scale is readable. Drawn in world space like
    ;; everything else, so it slides rather than staying pinned to the canvas.
    (set! (.-strokeStyle ctx) grid-colour)
    (set! (.-lineWidth ctx) 1)
    (.beginPath ctx)
    (let [[c0x c0z] (worldgen/chunk-of x0 z0)
          [c1x c1z] (worldgen/chunk-of (+ px half) (+ pz half))]
      (doseq [cx (range c0x (+ 2 c1x))
              :let [sx (* (- (* cx k/chunk-size) x0) per-m)]]
        (.moveTo ctx sx 0) (.lineTo ctx sx size))
      (doseq [cz (range c0z (+ 2 c1z))
              :let [sz (* (- (* cz k/chunk-size) z0) per-m)]]
        (.moveTo ctx 0 sz) (.lineTo ctx size sz)))
    (.stroke ctx)))

(defn- draw-player!
  "A triangle at the exact centre, pointing where the car points. North stays
  up: a map that turns with you is harder to read than one that does not."
  [{:keys [^js ctx]} size heading]
  (let [c (* 0.5 size)]
    (.save ctx)
    (.translate ctx c c)
    (.rotate ctx (- heading))
    (.beginPath ctx)
    (.moveTo ctx 0 -11)
    (.lineTo ctx 7 8)
    (.lineTo ctx 0 4)
    (.lineTo ctx -7 8)
    (.closePath ctx)
    (set! (.-fillStyle ctx) "#e8433a")
    (set! (.-strokeStyle ctx) "rgba(255,255,255,0.9)")
    (set! (.-lineWidth ctx) 1.5)
    (.fill ctx)
    (.stroke ctx)
    (.restore ctx)
    (set! (.-strokeStyle ctx) "rgba(255,255,255,0.45)")
    (set! (.-lineWidth ctx) 2)
    (.strokeRect ctx 1 1 (- size 2) (- size 2))))

(defn- draw-rivals!
  "One arrowhead per rival, pointing the way it is driving.

  Drawn in world space against the same anchor as everything else, so a blip
  sits exactly over the piece of map the car it represents is on. Wrecked
  rivals stay on the map as grey crosses: a burnt-out car is still a landmark,
  and it is useful to know which one of them you have already dealt with."
  [{:keys [^js ctx]} px pz per-m size blips]
  (let [half (* 0.5 (/ size per-m))
        x0 (- px half) z0 (- pz half)]
    (doseq [{:keys [x z heading out damage]} blips
            :let [sx (* (- x x0) per-m)
                  sz (* (- z z0) per-m)]
            ;; A rival is leashed long before it could leave the map, but a
            ;; wreck left behind will drift off it, and canvas has no clipping
            ;; here beyond the element itself.
            :when (and (<= 0 sx size) (<= 0 sz size))]
      (.save ctx)
      (.translate ctx sx sz)
      (if out
        (do (set! (.-strokeStyle ctx) "rgba(60,60,64,0.9)")
            (set! (.-lineWidth ctx) 2.5)
            (.beginPath ctx)
            (.moveTo ctx -4 -4) (.lineTo ctx 4 4)
            (.moveTo ctx 4 -4) (.lineTo ctx -4 4)
            (.stroke ctx))
        (do (.rotate ctx (- heading))
            (.beginPath ctx)
            (.moveTo ctx 0 -8)
            (.lineTo ctx 5.5 6)
            (.lineTo ctx 0 3)
            (.lineTo ctx -5.5 6)
            (.closePath ctx)
            ;; Fresh rivals are bright; a battered one fades towards the
            ;; colour of the wreck it is about to become.
            (set! (.-fillStyle ctx)
                  (str "rgb(" (js/Math.round (- 250 (* 90 damage))) ","
                       (js/Math.round (- 170 (* 120 damage))) ",40)"))
            (set! (.-strokeStyle ctx) "rgba(0,0,0,0.75)")
            (set! (.-lineWidth ctx) 1.5)
            (.fill ctx)
            (.stroke ctx)))
      (.restore ctx))))

(defn draw!
  "Redraw the map. `heading` is the car's yaw, in the same convention
  `camera/heading` uses. `blips` is where the rivals are, or nil.

  Called at HUD rate, not per frame -- a map that updates twice a second is
  indistinguishable from one that updates sixty times."
  ([ms x z heading] (draw! ms x z heading nil))
  ([{:keys [^js canvas ^js label at show-rivals] :as ms} x z heading blips]
  (when canvas
    (let [size (.-width canvas)
          per-m (/ size (* span k/chunk-size))]
      (draw-cells! ms x z per-m size)
      (draw-roads! ms x z per-m size)
      (when (and @show-rivals (seq blips))
        (draw-rivals! ms x z per-m size blips))
      (draw-player! ms size heading)
      (let [nm (area-here ms x z)]
        (when (and label (not= nm @at))
          (vreset! at nm)
          (set! (.-textContent label) nm)))))))

(defn stats
  "How much of the map has actually had to be worked out. Every cell is computed
  once, ever: driving a kilometre adds a few dozen, and standing still adds
  none."
  [{:keys [^js cache misses]}]
  {:cached (.-size cache) :computed @misses})
