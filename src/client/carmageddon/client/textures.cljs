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
            [carmageddon.shared.prng :as prng]))

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

(defn- facade-canvas
  "A wall with a grid of windows. Reading building scale at a glance is what
  makes a city feel like a city rather than a field of boxes -- and lit windows
  give the eye something to track speed against.

  `cols`/`rows` are per face, not per storey: the texture is stretched over a
  box scaled to the building, so a tall office needs many rows and a bungalow
  needs three. That works out because the window grid is chosen per zone, and
  zone already correlates with size."
  [seed size tint {:keys [cols rows lit shopfront?]
                   :or {cols 8 rows 10 lit 0.18 shopfront? false}}]
  (let [c   (canvas size)
        ctx (.getContext c "2d")
        rng (prng/make seed)
        [r g b] tint]
    (fill! ctx (rgb r g b) 0 0 size size)
    (jitter ctx rng size 3000 tint 18 3.0)
    (let [cw (/ size cols), ch (/ size rows)]
      (dotimes [row rows]
        (dotimes [col cols]
          (let [roll (prng/next-double! rng)
                wx (+ (* col cw) (* cw 0.22))
                wy (+ (* row ch) (* ch 0.18))
                ww (* cw 0.56), wh (* ch 0.5)]
            (fill! ctx
                   (cond (< roll lit)        "rgba(255,232,170,0.92)"
                         (< roll (+ lit 0.4)) "rgba(120,140,160,0.75)"
                         :else               "rgba(30,36,44,0.85)")
                   wx wy ww wh)))))
    ;; Shops are glass at street level and brick above, which is most of what
    ;; tells you a building is a shop when you are driving past it at speed.
    (when shopfront?
      (let [gh (/ size rows)]
        (fill! ctx "rgba(150,180,196,0.90)" (* size 0.04) (- size (* gh 1.25))
               (* size 0.92) (* gh 1.05))
        (fill! ctx "rgba(214,178,86,0.85)" 0 (- size (* gh 1.45))
               size (* gh 0.22))))
    c))

(def ^:private zone-facades
  "One facade per `worldgen/building-zones` entry, in that order."
  [{:tint [122 108 92]  :cols 4 :rows 3  :lit 0.30}                     ; house
   {:tint [134 106 96]  :cols 4 :rows 4  :lit 0.28}                     ; townhouse
   {:tint [104 104 110] :cols 7 :rows 9  :lit 0.22}                     ; apartment
   {:tint [126 116 104] :cols 5 :rows 4  :lit 0.34 :shopfront? true}    ; shop
   {:tint [86 98 112]   :cols 9 :rows 13 :lit 0.16}                     ; office
   {:tint [96 94 88]    :cols 6 :rows 3  :lit 0.10}                     ; factory
   {:tint [104 100 94]  :cols 3 :rows 2  :lit 0.05}                     ; warehouse
   {:tint [140 136 124] :cols 6 :rows 4  :lit 0.20}                     ; civic
   {:tint [118 74 58]   :cols 2 :rows 2  :lit 0.06}])                   ; barn

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
   ;; One facade per building zone, in `worldgen/building-zones` order. UVs come
   ;; from a shared unit cube scaled to each building, so windows stretch with
   ;; the box -- accepted deliberately: per-building geometry would mean
   ;; per-building disposal, and this is scenery.
   :facades (vec (map-indexed
                  (fn [i {:keys [tint] :as spec}]
                    (->texture (facade-canvas (prng/hash-coords seed 6 i) 256 tint spec)
                               renderer nil))
                  zone-facades))})
