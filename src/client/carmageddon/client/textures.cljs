(ns carmageddon.client.textures
  "Procedurally drawn textures.

  Browser-only and asset-free on purpose: everything is painted onto a canvas at
  boot, so there are no files to fetch, nothing to cache-bust, and no binary
  blobs in the repo. Generation runs off the shared seeded PRNG, so a given seed
  paints the same world every time -- the same property the chunk generator will
  rely on in M2.

  The ground texture is not decoration. On an untextured plane there is no
  optical flow, so speed is invisible and the vehicle cannot be tuned by feel."
  (:require ["three" :as three]
            [carmageddon.shared.prng :as prng]
            [carmageddon.shared.worldgen :as worldgen]))

(defn- canvas ^js [size]
  (let [c (js/document.createElement "canvas")]
    (set! (.-width c) size)
    (set! (.-height c) size)
    c))

(defn- fill! [^js ctx color x y w h]
  (set! (.-fillStyle ctx) color)
  (.fillRect ctx x y w h))

(defn- rgb [r g b]
  (str "rgb(" (js/Math.round r) "," (js/Math.round g) "," (js/Math.round b) ")"))

(defn- jitter
  "Sprinkle `n` translucent specks. This is what actually reads as motion when
  the ground is scrolling past at 100 km/h."
  [^js ctx rng size n [r g b] spread max-px]
  (dotimes [_ n]
    (let [x (* size (prng/next-double! rng))
          y (* size (prng/next-double! rng))
          d (* spread (- (prng/next-double! rng) 0.5))
          s (+ 1.0 (* max-px (prng/next-double! rng)))]
      (fill! ctx (rgb (+ r d) (+ g d) (+ b d)) x y s s))))

;; --- individual textures ----------------------------------------------------

(defn- ground-canvas
  "Near-field grain only. Macro variation is carried by vertex colours on the
  ground mesh, so nothing in here is allowed to be large enough to recognise --
  that is what keeps a 400-tile repeat from reading as a grid."
  [seed size]
  (let [c   (canvas size)
        ctx (.getContext c "2d")
        rng (prng/make seed)]
    (fill! ctx (rgb 74 92 66) 0 0 size size)
    (jitter ctx rng size  1100 [66 82 58] 30 26.0)  ; soil patches, ~40 cm
    (jitter ctx rng size  2400 [84 100 70] 26 12.0) ; growth clumps, ~18 cm
    (jitter ctx rng size  9000 [72 90 64] 34  5.0)  ; coarse grain
    (jitter ctx rng size 18000 [74 92 66] 22  2.0)  ; fine grain
    c))

(defn- road-canvas [seed size]
  (let [c   (canvas size)
        ctx (.getContext c "2d")
        rng (prng/make seed)]
    (fill! ctx (rgb 58 58 62) 0 0 size size)
    (jitter ctx rng size 14000 [58 58 62] 30 2.0)
    (jitter ctx rng size 900 [40 40 44] 26 4.0)
    c))

(defn- body-canvas
  "Panel grain for bodywork, and nothing else.

  This is a *modulation* map, not a paint job, and that distinction was the
  single biggest reason cars were too dark. It used to be painted in the muscle
  car's red, (176, 46, 34) -- and three.js multiplies `map` by `color`. The
  reference car came out right because its paint was very nearly the colour of
  its own texture; every other vehicle in the game had its blue multiplied by
  34/255 and its green by 46/255 before a single light was applied. A navy
  hatchback was arithmetically incapable of being navy.

  Near-white, so `color` is what the car is painted and this only adds the
  seams and the stripe. Both are darker than the base rather than lighter, so
  they read on a white truck as well as on a dark one."
  [seed size base-rgb]
  (let [c   (canvas size)
        ctx (.getContext c "2d")
        rng (prng/make seed)
        [r g b] base-rgb]
    (fill! ctx (rgb r g b) 0 0 size size)
    (jitter ctx rng size 2600 base-rgb 10 2.0)
    ;; Panel seams: cheap, but they give the body a scale reference so the car
    ;; does not read as an untextured slab.
    (set! (.-strokeStyle ctx) "rgba(0,0,0,0.26)")
    (set! (.-lineWidth ctx) 2)
    (doseq [f [0.18 0.5 0.82]]
      (.beginPath ctx)
      (.moveTo ctx 0 (* size f))
      (.lineTo ctx size (* size f))
      (.stroke ctx))
    ;; A single off-centre stripe -- makes yaw and roll legible at a glance.
    (fill! ctx "rgba(0,0,0,0.14)" 0 (* size 0.40) size (* size 0.06))
    c))

(defn- crate-canvas [seed size]
  (let [c   (canvas size)
        ctx (.getContext c "2d")
        rng (prng/make seed)]
    (fill! ctx (rgb 165 132 82) 0 0 size size)
    (dotimes [i 6]
      (let [y (* size (/ i 6.0))
            d (* 22 (- (prng/next-double! rng) 0.5))]
        (fill! ctx (rgb (+ 165 d) (+ 132 d) (+ 82 d)) 0 y size (/ size 6.2))))
    (jitter ctx rng size 4000 [165 132 82] 26 2.0)
    (set! (.-strokeStyle ctx) "rgba(60,40,20,0.55)")
    (set! (.-lineWidth ctx) 3)
    (.strokeRect ctx 2 2 (- size 4) (- size 4))
    c))

;; --- facades ----------------------------------------------------------------
;;
;; A facade used to be one picture per zone stretched over the whole wall, so a
;; window was a fixed fraction of a building rather than a fixed size: an
;; office sixty metres wide had windows ten metres across, a tower had thirteen
;; storeys whatever its height, and a house had three. `buildings` now tiles
;; these at a real bay width and storey height instead -- see the shader there
;; -- so what is painted here is a *tile*: a few bays by a few storeys that
;; repeats seamlessly, plus a separate ground storey, because the ground floor
;; of a building is the one floor that is different from the others.

(def ^:private cell 64)   ; px per window cell, either way

(defn- canvas-wh ^js [w h]
  (let [c (js/document.createElement "canvas")]
    (set! (.-width c) w)
    (set! (.-height c) h)
    c))

(defn- speckle!
  "`jitter`, for a canvas that is not square."
  [^js ctx rng w h n [r g b] spread max-px]
  (dotimes [_ n]
    (let [x (* w (prng/next-double! rng))
          y (* h (prng/next-double! rng))
          d (* spread (- (prng/next-double! rng) 0.5))
          s (+ 1.0 (* max-px (prng/next-double! rng)))]
      (fill! ctx (rgb (+ r d) (+ g d) (+ b d)) x y s s))))

(defn- shade-of [[r g b] f] (rgb (* r f) (* g f) (* b f)))

(defn- glass
  "One pane: lit, reflecting the sky, or dark -- in that order of rarity."
  [rng lit]
  (let [roll (prng/next-double! rng)]
    (cond (< roll lit)          "rgb(255,222,150)"
          (< roll (+ lit 0.30)) "rgb(118,140,162)"
          (< roll (+ lit 0.62)) "rgb(70,86,104)"
          :else                 "rgb(34,40,50)")))

(def ^:private shutter-paints
  ["rgb(46,96,74)" "rgb(52,86,140)" "rgb(120,52,44)" "rgb(64,116,128)"])

(defn- window-cell!
  "Paint the window in one cell of an upper storey, in the zone's style."
  [^js ctx rng style tint lit x y c]
  (let [r (fn [fx fy fw fh colour] (fill! ctx colour (+ x (* fx c)) (+ y (* fy c))
                                          (* fw c) (* fh c)))]
    (case style
      :curtain
      ;; Glass nearly edge to edge, a spandrel under it and a mullion down it.
      (do (r 0.0 0.74 1.0 0.26 (shade-of tint 0.72))
          (r 0.04 0.04 0.92 0.68 (glass rng lit))
          (r 0.485 0.04 0.03 0.68 (shade-of tint 0.55)))

      :industrial
      ;; A band of small panes high up, where the light falls on the floor.
      (do (r 0.08 0.12 0.84 0.38 (if (< (prng/next-double! rng) lit)
                                   "rgb(240,214,150)" "rgb(96,110,118)"))
          (doseq [f [0.29 0.50 0.71]] (r f 0.12 0.02 0.38 (shade-of tint 0.6)))
          (r 0.08 0.30 0.84 0.02 (shade-of tint 0.6)))

      :planks
      (do (doseq [i (range 5)]
            (r (* i 0.2) 0.0 0.012 1.0 (shade-of tint 0.66)))
          (when (< (prng/next-double! rng) 0.15)
            (r 0.36 0.30 0.28 0.26 (glass rng lit))))

      :logs
      ;; Eight logs to a storey with a dark seam between each, and a small
      ;; window in a white carved frame with painted shutters either side --
      ;; the frame is the thing a Russian village is recognised by.
      (do (dotimes [i 8]
            (r 0.0 (* i 0.125) 1.0 0.125 (shade-of tint (if (even? i) 1.0 0.88)))
            (r 0.0 (+ (* i 0.125) 0.112) 1.0 0.016 (shade-of tint 0.52)))
          (let [sh (nth shutter-paints (prng/next-int! rng (count shutter-paints)))]
            (r 0.22 0.34 0.12 0.40 sh)
            (r 0.66 0.34 0.12 0.40 sh))
          (r 0.32 0.30 0.36 0.50 "rgb(232,228,216)")
          (r 0.29 0.22 0.42 0.08 "rgb(232,228,216)")
          (r 0.36 0.35 0.28 0.40 (glass rng lit))
          (r 0.495 0.35 0.02 0.40 "rgb(232,228,216)"))

      :plaster
      ;; Deep-set and small, with a timber lintel over it.
      (do (r 0.31 0.27 0.38 0.48 (shade-of tint 0.80))
          (r 0.34 0.30 0.32 0.42 (glass rng (* 0.5 lit)))
          (r 0.27 0.21 0.46 0.06 "rgb(108,76,50)")
          (when (< (prng/next-double! rng) 0.35)
            (let [sh (nth shutter-paints (prng/next-int! rng (count shutter-paints)))]
              (r 0.20 0.27 0.10 0.48 sh)
              (r 0.70 0.27 0.10 0.48 sh))))

      :balcony
      ;; Tall doors on to a balcony, shutters folded back, a railing.
      (let [sh (nth shutter-paints (prng/next-int! rng (count shutter-paints)))]
        (r 0.22 0.16 0.12 0.70 sh)
        (r 0.66 0.16 0.12 0.70 sh)
        (r 0.34 0.16 0.32 0.70 (glass rng lit))
        (r 0.26 0.62 0.48 0.03 "rgb(40,40,44)")
        (doseq [i (range 7)] (r (+ 0.27 (* i 0.07)) 0.62 0.012 0.24 "rgb(40,40,44)")))

      :timber
      ;; Dark posts and beams over white plaster, paper screens between.
      (do (r 0.0 0.0 0.07 1.0 "rgb(62,46,36)")
          (r 0.0 0.0 1.0 0.08 "rgb(62,46,36)")
          (r 0.0 0.64 1.0 0.05 "rgb(62,46,36)")
          (r 0.14 0.18 0.74 0.44 (if (< (prng/next-double! rng) lit)
                                   "rgb(250,226,170)" "rgb(222,214,190)"))
          (doseq [f [0.32 0.51 0.69]] (r f 0.18 0.015 0.44 "rgb(80,64,50)"))
          (doseq [f [0.32 0.47]] (r 0.14 f 0.74 0.012 "rgb(80,64,50)")))

      :shuttered
      (do (r 0.28 0.22 0.44 0.52 (shade-of tint 0.82))
          (if (< (prng/next-double! rng) 0.4)
            ;; Shutters shut: louvres.
            (let [sh (nth shutter-paints (prng/next-int! rng (count shutter-paints)))]
              (r 0.30 0.24 0.40 0.48 sh)
              (doseq [i (range 8)]
                (r 0.30 (+ 0.26 (* i 0.058)) 0.40 0.012 "rgba(0,0,0,0.35)")))
            (r 0.31 0.25 0.38 0.46 (glass rng lit))))

      :tall
      ;; Civic: tall windows with a pediment-ish hood.
      (do (r 0.30 0.10 0.40 0.72 (shade-of tint 0.84))
          (r 0.33 0.14 0.34 0.64 (glass rng lit))
          (r 0.27 0.06 0.46 0.05 (shade-of tint 1.12)))

      ;; :punched -- an ordinary window with a sill under it.
      (do (r 0.22 0.18 0.56 0.56 (shade-of tint 0.80))
          (r 0.25 0.21 0.50 0.50 (glass rng lit))
          (r 0.495 0.21 0.012 0.50 (shade-of tint 0.70))
          (r 0.20 0.74 0.60 0.05 (shade-of tint 1.14))))))

(defn- upper-canvas
  "`cols` bays by `rows` storeys of upper floors, seamless both ways."
  [seed {:keys [tint cols rows lit style]}]
  (let [w (* cols cell) h (* rows cell)
        c (canvas-wh w h)
        ctx (.getContext c "2d")
        rng (prng/make seed)]
    (fill! ctx (apply rgb tint) 0 0 w h)
    (speckle! ctx rng w h (* 0.04 w h) tint 18 3.0)
    (dotimes [row rows]
      (dotimes [col cols]
        (window-cell! ctx rng style tint lit (* col cell) (* row cell) cell)))
    c))

(def ^:private sign-paints
  ["rgb(200,52,44)" "rgb(222,176,46)" "rgb(46,138,76)" "rgb(44,90,176)"
   "rgb(216,90,44)" "rgb(150,60,150)"])

(defn- ground-canvas*
  "The ground storey: `cols` bays wide, one storey tall, street at the bottom."
  [seed {:keys [tint cols lit style ground-style ground bay]}]
  (let [w  (* cols cell)
        ;; Same pixels per metre as across, so a shopfront is not squashed.
        h  (max cell (js/Math.round (* cell (/ ground bay))))
        c  (canvas-wh w h)
        ctx (.getContext c "2d")
        rng (prng/make seed)
        r  (fn [x fx fy fw fh colour]
             (fill! ctx colour (+ x (* fx cell)) (* fy h) (* fw cell) (* fh h)))]
    (fill! ctx (apply rgb tint) 0 0 w h)
    (speckle! ctx rng w h (* 0.04 w h) tint 18 3.0)
    (dotimes [col cols]
      (let [x (* col cell)
            roll (prng/next-double! rng)]
        (case ground-style
          :shopfront
          (do (r x 0.0 0.0 1.0 0.22 (nth sign-paints (prng/next-int! rng (count sign-paints))))
              (r x 0.04 0.28 0.92 0.72 (shade-of tint 0.6))
              (r x 0.07 0.31 0.86 0.69 (if (< roll (+ 0.3 lit)) "rgb(236,214,160)"
                                           "rgb(120,146,160)"))
              (when (< roll 0.25) (r x 0.36 0.40 0.28 0.60 "rgb(52,52,58)")))

          :lobby
          (do (r x 0.0 0.0 1.0 1.0 "rgb(40,44,52)")
              (r x 0.05 0.06 0.40 0.94 (glass rng (+ 0.3 lit)))
              (r x 0.55 0.06 0.40 0.94 (glass rng (+ 0.3 lit))))

          :arcade
          ;; Arches. The one curve in the city, and it is enough.
          (let [cx (+ x (* 0.5 cell)) rad (* 0.30 cell)
                top (* 0.36 h)]
            (set! (.-fillStyle ctx) "rgb(46,38,32)")
            (.beginPath ctx)
            (.moveTo ctx (- cx rad) h)
            (.lineTo ctx (- cx rad) (+ top rad))
            (.arc ctx cx (+ top rad) rad js/Math.PI 0.0)
            (.lineTo ctx (+ cx rad) h)
            (.closePath ctx)
            (.fill ctx)
            (when (< roll 0.5)
              (r x 0.30 0.62 0.40 0.38 (if (< roll 0.25) "rgb(236,206,150)"
                                           "rgb(96,72,54)"))))

          :shutter
          ;; Roll-down shutters, half of them up and the shop open.
          (do (r x 0.0 0.0 1.0 0.20 (nth sign-paints (prng/next-int! rng (count sign-paints))))
              (if (< roll 0.55)
                (do (r x 0.05 0.26 0.90 0.74 "rgb(226,196,140)")
                    (doseq [i (range 3)]
                      (r x (+ 0.10 (* i 0.28)) 0.62 0.22 0.38
                         (nth sign-paints (prng/next-int! rng (count sign-paints))))))
                (do (r x 0.05 0.26 0.90 0.74 "rgb(150,152,156)")
                    (dotimes [i 10]
                      (r x 0.05 (+ 0.28 (* i 0.072)) 0.90 0.01 "rgb(110,112,116)")))))

          ;; :door -- a front door in some bays, a window in the rest.
          (if (< roll 0.32)
            (do (r x 0.34 0.30 0.32 0.70 (shade-of tint 0.7))
                (r x 0.37 0.33 0.26 0.67 "rgb(70,48,36)"))
            (do (r x 0.22 0.26 0.56 0.44 (shade-of tint 0.80))
                (r x 0.25 0.29 0.50 0.38 (glass rng lit))
                (r x 0.20 0.70 0.60 0.04 (shade-of tint 1.14)))))))
    c))

(def zone-facades
  "One facade per `worldgen/building-zones` entry, in that order.

  `bay` and `storey` are metres per window cell across and up; `ground` is the
  height of a ground storey that is drawn differently, or zero for a building
  that is the same all the way down. The painted zones are near white, because
  their colour is a paint carried per building -- see `worldgen/painted`."
  [{:name :house     :tint [122 108 92]  :bay 3.2 :storey 2.8 :ground 3.0
    :cols 4 :rows 2 :lit 0.30 :style :punched :ground-style :door}
   {:name :townhouse :tint [134 106 96]  :bay 3.0 :storey 3.1 :ground 3.3
    :cols 4 :rows 4 :lit 0.28 :style :punched :ground-style :door}
   {:name :apartment :tint [104 104 110] :bay 3.0 :storey 2.9 :ground 3.6
    :cols 8 :rows 8 :lit 0.22 :style :punched :ground-style :door}
   {:name :shop      :tint [126 116 104] :bay 3.4 :storey 3.2 :ground 4.4
    :cols 4 :rows 4 :lit 0.34 :style :punched :ground-style :shopfront}
   {:name :office    :tint [86 98 112]   :bay 2.6 :storey 3.6 :ground 5.0
    :cols 8 :rows 8 :lit 0.16 :style :curtain :ground-style :lobby}
   {:name :factory   :tint [96 94 88]    :bay 6.0 :storey 5.0 :ground 0.0
    :cols 4 :rows 2 :lit 0.10 :style :industrial}
   {:name :warehouse :tint [104 100 94]  :bay 7.0 :storey 6.0 :ground 0.0
    :cols 2 :rows 1 :lit 0.05 :style :industrial}
   {:name :civic     :tint [140 136 124] :bay 3.6 :storey 4.2 :ground 4.6
    :cols 6 :rows 3 :lit 0.20 :style :tall :ground-style :door}
   {:name :barn      :tint [118 74 58]   :bay 4.0 :storey 4.0 :ground 0.0
    :cols 2 :rows 1 :lit 0.06 :style :planks}
   ;; Heavy plant: almost no windows, and the few there are burn all night.
   {:name :plant     :tint [92 92 90]    :bay 6.0 :storey 5.0 :ground 0.0
    :cols 3 :rows 2 :lit 0.32 :style :industrial}
   {:name :yard      :tint [86 84 80]    :bay 6.0 :storey 3.0 :ground 0.0
    :cols 2 :rows 1 :lit 0.02 :style :planks}
   {:name :izba      :tint [116 82 52]   :bay 2.8 :storey 3.4 :ground 0.0
    :cols 3 :rows 1 :lit 0.25 :style :logs}
   {:name :adobe     :tint [246 242 234] :bay 3.0 :storey 3.2 :ground 0.0
    :cols 3 :rows 2 :lit 0.15 :style :plaster}
   {:name :casa      :tint [246 242 234] :bay 3.2 :storey 3.0 :ground 3.4
    :cols 4 :rows 2 :lit 0.20 :style :balcony :ground-style :arcade}
   {:name :minka     :tint [228 222 206] :bay 1.9 :storey 3.2 :ground 0.0
    :cols 4 :rows 1 :lit 0.30 :style :timber}
   {:name :shophouse :tint [246 242 234] :bay 3.4 :storey 3.0 :ground 3.8
    :cols 3 :rows 3 :lit 0.25 :style :shuttered :ground-style :shutter}])

;; `buildings/build-group!` looks a facade up by zone index, so a zone without
;; one here would not fail -- it would silently draw some other zone's
;; windows. One assert is cheaper than finding that, and checking the names
;; rather than only the count catches the same mistake made in the middle.
(assert (= (mapv :name zone-facades) (mapv :name worldgen/building-zones))
        "every building zone needs a facade, in the same order")

(defn- tyre-canvas [seed size]
  (let [c   (canvas size)
        ctx (.getContext c "2d")
        rng (prng/make seed)]
    (fill! ctx (rgb 26 26 30) 0 0 size size)
    ;; Vertical bars become tread blocks once wrapped around the cylinder, so
    ;; wheel spin is actually visible.
    (dotimes [i 18]
      (fill! ctx (rgb 14 14 17) (* size (/ i 18.0)) 0 (/ size 42.0) size))
    (jitter ctx rng size 2200 [26 26 30] 14 2.0)
    c))

;; --- three.js wrapping ------------------------------------------------------

(defn- ->texture ^js [^js canvas ^js renderer {:keys [repeat]}]
  (let [t (three/CanvasTexture. canvas)]
    (set! (.-wrapS t) three/RepeatWrapping)
    (set! (.-wrapT t) three/RepeatWrapping)
    (set! (.-colorSpace t) three/SRGBColorSpace)
    ;; Without anisotropy a ground plane viewed at a grazing angle blurs to mush
    ;; a few metres out, which is exactly where the speed cue needs to be.
    (set! (.-anisotropy t) (.getMaxAnisotropy (.-capabilities renderer)))
    (when repeat (.set (.-repeat t) (first repeat) (second repeat)))
    t))

(defn build!
  "Paint every texture once. `seed` makes the result reproducible."
  [^js renderer seed]
  {:ground (->texture (ground-canvas (prng/hash-coords seed 1 0) 512) renderer
                      ;; repeat stays 1: chunk meshes carry world-derived UVs (metres / tile size),
   ;; which is what keeps the grain continuous across chunk seams. Setting a
   ;; repeat here as well would multiply with those and tile ~25x per metre.
   {:repeat [1 1]})
   :road   (->texture (road-canvas (prng/hash-coords seed 2 0) 512) renderer
                      {:repeat [40 40]})
   ;; Near-white on purpose: see `body-canvas`. The colour of a car lives in
   ;; its material, not in here.
   :body   (->texture (body-canvas (prng/hash-coords seed 3 0) 256 [238 238 238]) renderer nil)
   :crate  (->texture (crate-canvas (prng/hash-coords seed 4 0) 256) renderer nil)
   :tyre   (->texture (tyre-canvas (prng/hash-coords seed 5 0) 128) renderer nil)
   ;; One facade per building zone, in `worldgen/building-zones` order: the
   ;; upper-floor tile, the ground storey, and the measurements `buildings`
   ;; needs to lay them out at real size.
   :facades (vec (map-indexed
                  (fn [i spec]
                    (let [up (->texture (upper-canvas (prng/hash-coords seed 6 i) spec)
                                        renderer nil)
                          gr (when (pos? (:ground spec))
                               (doto (->texture (ground-canvas* (prng/hash-coords seed 7 i)
                                                                spec)
                                                renderer nil)
                                 ;; Repeats along the street, never up it: a
                                 ;; ground storey has a top and a bottom.
                                 (-> .-wrapT (set! three/ClampToEdgeWrapping))))]
                      {:upper up :ground (or gr up) :spec spec}))
                  zone-facades))})
