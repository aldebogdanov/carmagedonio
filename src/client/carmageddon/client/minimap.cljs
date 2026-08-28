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

(def ^:private radius 6)                     ; chunks either side, so 13 across
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
       :at (volatile! nil)})))

(defn- kind-at [{:keys [seed ^js cache]} cx cz]
  (let [k (+ (* cx 100000) cz)]
    (or (.get cache k)
        (let [v (worldgen/area-kind seed cx cz)]
          (.set cache k v)
          v))))

(defn area-here
  "The label for the chunk the player is standing in."
  [ms x z]
  (let [[cx cz] (worldgen/chunk-of x z)]
    (worldgen/area-labels (kind-at ms cx cz))))

(defn- draw-cells! [{:keys [^js ctx] :as ms} cx cz cell]
  (dotimes [i span]
    (dotimes [j span]
      (let [kx (+ cx (- i radius))
            kz (+ cz (- j radius))]
        (set! (.-fillStyle ctx) (get colours (kind-at ms kx kz) "#000"))
        (.fillRect ctx (* i cell) (* j cell) (inc cell) (inc cell))))))

(defn- draw-roads!
  "The arterial lattice, straight from the line indices.

  No street is generated to draw this. Whether a lattice line is a main road is
  arithmetic on its index, which is the same reason the generator itself never
  has to be told where the main roads are."
  [{:keys [^js ctx]} cx cz cell size]
  (let [;; World bounds of the window, and the lattice lines inside them.
        x0 (* (- cx radius) k/chunk-size)
        z0 (* (- cz radius) k/chunk-size)
        w  (* span k/chunk-size)
        px (/ size w)
        g0x (long (Math/floor (/ x0 worldgen/street-spacing)))
        g1x (long (Math/ceil (/ (+ x0 w) worldgen/street-spacing)))
        g0z (long (Math/floor (/ z0 worldgen/street-spacing)))
        g1z (long (Math/ceil (/ (+ z0 w) worldgen/street-spacing)))]
    (set! (.-strokeStyle ctx) road-colour)
    (set! (.-lineWidth ctx) 2)
    (.beginPath ctx)
    (doseq [g (range g0x (inc g1x))
            :when (worldgen/arterial-line? g)
            :let [sx (* (- (* g worldgen/street-spacing) x0) px)]]
      (.moveTo ctx sx 0) (.lineTo ctx sx size))
    (doseq [g (range g0z (inc g1z))
            :when (worldgen/arterial-line? g)
            :let [sz (* (- (* g worldgen/street-spacing) z0) px)]]
      (.moveTo ctx 0 sz) (.lineTo ctx size sz))
    (.stroke ctx)
    ;; A faint chunk grid, so the scale is readable.
    (set! (.-strokeStyle ctx) grid-colour)
    (set! (.-lineWidth ctx) 1)
    (.beginPath ctx)
    (dotimes [i (inc span)]
      (.moveTo ctx (* i cell) 0) (.lineTo ctx (* i cell) size)
      (.moveTo ctx 0 (* i cell)) (.lineTo ctx size (* i cell)))
    (.stroke ctx)))

(defn- draw-player!
  "A triangle at the player's true position within the centre chunk, pointing
  where the car points. North is up, which is why the map never rotates: a map
  that turns with you is harder to read than one that does not."
  [{:keys [^js ctx]} x z cx cz cell size heading]
  (let [;; Offset inside the centre cell, so the marker slides rather than
        ;; jumping a whole chunk at a time.
        fx (- (/ x k/chunk-size) cx)
        fz (- (/ z k/chunk-size) cz)
        px (* (+ radius fx) cell)
        pz (* (+ radius fz) cell)]
    (.save ctx)
    (.translate ctx px pz)
    (.rotate ctx (- heading))
    (.beginPath ctx)
    (.moveTo ctx 0 -9)
    (.lineTo ctx 6 7)
    (.lineTo ctx 0 3)
    (.lineTo ctx -6 7)
    (.closePath ctx)
    (set! (.-fillStyle ctx) "#e8433a")
    (set! (.-strokeStyle ctx) "rgba(255,255,255,0.9)")
    (set! (.-lineWidth ctx) 1.5)
    (.fill ctx)
    (.stroke ctx)
    (.restore ctx)
    ;; Frame last, over everything.
    (set! (.-strokeStyle ctx) "rgba(255,255,255,0.45)")
    (set! (.-lineWidth ctx) 2)
    (.strokeRect ctx 1 1 (- size 2) (- size 2))))

(defn draw!
  "Redraw the map. `heading` is the car's yaw, in the same convention
  `camera/heading` uses: local -Z, measured as atan2 of the world forward.

  Called at HUD rate, not per frame -- a map that updates twice a second is
  indistinguishable from one that updates sixty times, and this rasterises 169
  cells."
  [{:keys [^js canvas ^js label at] :as ms} x z heading]
  (when canvas
    (let [size (.-width canvas)
          cell (/ size span)
          [cx cz] (worldgen/chunk-of x z)]
      (draw-cells! ms cx cz cell)
      (draw-roads! ms cx cz cell size)
      (draw-player! ms x z cx cz cell size heading)
      (let [name (worldgen/area-labels (kind-at ms cx cz))]
        (when (and label (not= name @at))
          (vreset! at name)
          (set! (.-textContent label) name))))))
