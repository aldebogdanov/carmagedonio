(ns carmageddon.client.render
  "three.js presentation layer.

  Reads simulation transforms, never writes them. The renderer interpolates
  between the last two sim ticks by `alpha`, which is what decouples a 60 Hz
  sim from a 144 Hz (or 30 Hz) display without the picture juddering.

  Wheels are children of the chassis mesh, so they inherit that interpolation
  for free and only their suspension travel / steer / spin need updating. Seeing
  the suspension work is the point -- it is the main visual feedback for tuning
  the vehicle in M1.

  Visuals stay cheap: flat shading, procedurally painted textures, no shadows."
  (:require ["three" :as three]
            [carmageddon.client.camera :as camera]
            [carmageddon.client.cars :as cars]
            [carmageddon.client.sim :as sim]
            [carmageddon.client.textures :as textures]
            [carmageddon.client.vehicle :as vehicle]
            [carmageddon.shared.constants :as k]))

(def ^:private sky-top 0x4d86c6)
(def ^:private sky-horizon 0xbdd6e8)
(def ^:private ground-bounce 0x54503f)

;; How far the sun's shadow camera reaches, in metres either side of the car.
;; A shadow map covers a fixed box, so this trades resolution against range:
;; 70 m keeps the texels small enough that a lamp post has a lamp post's
;; shadow, and everything past it is beyond where anyone is looking anyway.
(def ^:private shadow-reach 90.0)

(defn- lerp [a b t] (+ a (* (- b a) t)))

(defn- mat
  ([tex] (mat tex false nil))
  ([tex vertex-colors?] (mat tex vertex-colors? nil))
  ([tex vertex-colors? colour]
   (three/MeshPhongMaterial. (cond-> #js {:map tex :shininess 6
                                          :vertexColors vertex-colors?}
                               colour (doto (aset "color" colour))))))

(defn- paint-mat
  "Bodywork. A standard material rather than the Phong everything else uses,
  because `scene.environment` only reaches standard materials -- and a sky to
  reflect is the entire difference between paint and coloured cardboard.

  Metalness was 0.45, and that was the bug behind 'the hatchback is invisible'.
  A metal has no diffuse term: the shader multiplies the base colour's diffuse
  contribution by (1 - metalness), so nearly half of what makes a blue car blue
  was being thrown away before the light even arrived. What was left was a
  specular reflection of a two-pixel gradient sky -- which under cloud is dim
  and grey, and under a storm is nearly nothing. Cars painted a dark colour to
  begin with had nothing to fall back on.

  0.16 is a clear coat over pigment, which is what car paint is. The env map
  still gives it the sky; the pigment now survives losing it."
  [tex colour]
  (three/MeshStandardMaterial. #js {:map tex :color colour
                                    :metalness 0.16 :roughness 0.46
                                    :envMapIntensity 1.15}))

(defn- chunk-geometry
  "Build a BufferGeometry for one streamed chunk.

  Indexing matches `worldgen/chunk-data` exactly -- x selects the row, z varies
  fastest -- so the visible surface and the heightfield collider are built from
  the same numbers in the same order and cannot drift apart.

  UVs are derived from world position rather than from the grid, so the tiled
  grain texture is continuous across chunk borders instead of restarting at
  every seam."
  [{:keys [verts size origin heights colors]}]
  (let [n     verts
        cells (dec n)
        step  (/ size cells)
        [x0 z0] origin
        vcount (* n n)
        pos   (js/Float32Array. (* vcount 3))
        uv    (js/Float32Array. (* vcount 2))
        idx   (js/Uint32Array. (* cells cells 6))
        tile  8.0]
    (dotimes [i n]
      (dotimes [j n]
        (let [k   (+ (* i n) j)
              o   (* k 3)
              x   (* i step)
              z   (* j step)]
          (aset pos (+ o 0) x)
          (aset pos (+ o 1) (aget heights k))
          (aset pos (+ o 2) z)
          (aset uv (+ (* k 2) 0) (/ (+ x0 x) tile))
          (aset uv (+ (* k 2) 1) (/ (+ z0 z) tile)))))
    (loop [i 0, w 0]
      (when (< i cells)
        (let [w (loop [j 0, w w]
                  (if (< j cells)
                    (let [a (+ (* i n) j)
                          b (+ (* (inc i) n) j)
                          c (+ (* (inc i) n) (inc j))
                          d (+ (* i n) (inc j))]
                      ;; Winding matters: with (a b c) the cross product comes
                      ;; out (0, -s^2, 0) -- every normal points down, the
                      ;; surface faces away, and front-face culling makes the
                      ;; whole world invisible while physics carries on fine.
                      (aset idx (+ w 0) a) (aset idx (+ w 1) c) (aset idx (+ w 2) b)
                      (aset idx (+ w 3) a) (aset idx (+ w 4) d) (aset idx (+ w 5) c)
                      (recur (inc j) (+ w 6)))
                    w))]
          (recur (inc i) w))))
    (let [g (three/BufferGeometry.)]
      (.setAttribute g "position" (three/BufferAttribute. pos 3))
      (.setAttribute g "uv" (three/BufferAttribute. uv 2))
      (.setAttribute g "color" (three/BufferAttribute. colors 3))
      (.setIndex g (three/BufferAttribute. idx 1))
      (.computeVertexNormals g)
      g)))

(defn add-chunk!
  "Chunk-manager callback. Returns the mesh, which comes back to `remove-chunk!`.

  Every chunk shares one material. Building a fresh one per chunk leaks: three
  materials hold GPU resources that only `dispose` frees, and chunks are created
  and destroyed continuously as the player drives."
  [{:keys [^js scene chunk-material]} data]
  (let [[x0 z0] (:origin data)
        ^js m (three/Mesh. (chunk-geometry data) chunk-material)]
    (.set (.-position m) x0 0 z0)
    ;; Terrain receives but does not cast. Casting would double the shadow
    ;; pass for hills that mostly shadow themselves, and self-shadowing a
    ;; heightfield is the classic source of acne.
    (set! (.-receiveShadow m) true)
    (.add scene m)
    m))

(defn remove-chunk!
  "Geometry holds GPU buffers that the garbage collector cannot reclaim on its
  own, so unloading a chunk has to dispose explicitly or the process leaks
  steadily as the player drives."
  [{:keys [^js scene]} ^js m]
  (.remove scene m)
  (.dispose (.-geometry m)))

(defn- sky-texture
  "A vertical gradient, painted once into a 2 x 256 canvas.

  Treated as an equirectangular map, so it works both as the background and --
  once run through the PMREM generator -- as the environment the cars reflect.
  A gradient is all an equirect needs when the sky has no clouds in it, and two
  pixels wide is all it needs when nothing varies with bearing."
  []
  (let [^js c (js/document.createElement "canvas")
        _ (set! (.-width c) 2)
        _ (set! (.-height c) 256)
        ^js g (.getContext c "2d")
        grad (.createLinearGradient g 0 0 0 256)
        hex (fn [n] (str "#" (.padStart (.toString n 16) 6 "0")))]
    (.addColorStop grad 0.0 (hex sky-top))
    (.addColorStop grad 0.52 (hex sky-horizon))
    ;; Below the horizon is haze, not ground: the terrain covers it, and the
    ;; only place this shows is in the cars' reflections.
    (.addColorStop grad 1.0 (hex 0x8e9384))
    (set! (.-fillStyle g) grad)
    (.fillRect g 0 0 2 256)
    (doto (three/CanvasTexture. c)
      (-> .-mapping (set! (.-EquirectangularReflectionMapping three)))
      (-> .-colorSpace (set! (.-SRGBColorSpace three))))))

(defn- build-scene!
  "Sky, sun and the shadow camera.

  The environment map is what turns flat-shaded boxes into cars: paint with
  nothing to reflect reads as coloured cardboard whatever the lighting does."
  [^js renderer]
  (let [scene   (three/Scene.)
        tex     (sky-texture)
        ^js pmrem (three/PMREMGenerator. renderer)
        env     (.-texture (.fromEquirectangular pmrem tex))
        ^js sun (three/DirectionalLight. 0xfff0d4 2.8)]
    (.dispose pmrem)
    (set! (.-background scene) tex)
    (set! (.-environment scene) env)
    ;; Fog has to close in before the streaming radius ends, or chunks visibly
    ;; pop into existence at the horizon. Matched to the horizon stop of the
    ;; gradient, so the world dissolves into the sky rather than into a band of
    ;; the wrong colour.
    (set! (.-fog scene) (three/Fog. sky-horizon 160 (* 0.92 k/stream-radius k/chunk-size)))
    ;; Mid-afternoon, about 35 degrees up. This is not a cosmetic choice: the
    ;; first version put the sun 63 degrees up, and at that angle a low block
    ;; casts a shadow shorter than its own pavement. Everything was working --
    ;; the map was full of geometry, the shaders sampled it, the depth
    ;; comparison came out "in shadow" -- and nothing was visible on the road.
    (.set (.-position sun) 95 82 62)
    (set! (.-castShadow sun) true)
    (let [^js sh (.-shadow sun)
          ^js c  (.-camera sh)]
      (.set (.-mapSize sh) 2048 2048)
      (set! (.-left c) (- shadow-reach))
      (set! (.-right c) shadow-reach)
      (set! (.-top c) shadow-reach)
      (set! (.-bottom c) (- shadow-reach))
      (set! (.-near c) 1.0)
      (set! (.-far c) 420.0)
      ;; Acne on the terrain and peter-panning under the cars are the two ways
      ;; this goes wrong, and they pull in opposite directions. normalBias
      ;; handles the first without lifting shadows off their objects.
      (set! (.-bias sh) -0.0004)
      (set! (.-normalBias sh) 0.04)
      (.updateProjectionMatrix c))
    (.add scene sun)
    (.add scene (.-target sun))
    ;; The sky fill, and the single biggest lever on whether a car is readable.
    ;; The sun lights one side of a box; this is what stops the other three
    ;; being a silhouette. It was 1.1, which was enough on a clear day and not
    ;; on any other -- and an overcast sky is *more* ambient light, not less,
    ;; which is why `weather` now raises this as it takes the sun away.
    (.add scene (doto (three/HemisphereLight. sky-horizon ground-bounce 1.55)
                  (aset "name" "fill")))
    [scene sun]))

(defn- follow-sun!
  "Keep the shadow box over the car.

  A directional light's shadow covers one fixed box in world space. Left at the
  origin it stops working the moment the player drives out of it, which in a
  world this size is about four seconds."
  [^js sun px py pz]
  (.set (.-position sun) (+ px 95.0) (+ py 82.0) (+ pz 62.0))
  (.set (.-position (.-target sun)) px py pz)
  (.updateMatrixWorld (.-target sun)))

(def ^:private part-tints
  "Bodywork that is not paint. Glass is dark and slightly blue; trim is the
  bumpers, stacks and spoilers, which read best as near-black.

  Both were darker. On a vehicle that is mostly glass and trim -- the tractor is
  a cab, a bonnet and an exhaust stack -- near-black parts and a metallic paint
  that had lost its diffuse left nothing at all to see under cloud. Lifted far
  enough to read as a surface rather than a hole."
  {:glass 0x30455a
   :trim  0x3c3c42
   ;; Lit parts. These never take a light calculation at all -- see `lamp-mat`.
   :lamp  0xfff2cf
   :tail  0xd6382c})

(def ^:private rival-paint
  "Rivals are painted from their own palette rather than their catalogue
  colour, so a truck bearing down on you is distinguishable at a glance from
  the traffic. Which one you are looking at is the map's job; *that it is
  hostile* has to be readable from the mirror."
  [0xd8722c 0x7d3fa8 0xb8322f 0x1c7d74 0xc9a227])

;; Lamp lenses, dark and lit. A lamp is drawn with an unlit material -- see
;; `lamp-mat` -- so switching one on is a colour, not a light.
(def ^:private lamp-off 0x7a7566)
(def ^:private lamp-on 0xfff4d2)
(def ^:private tail-off 0x6b2a25)
(def ^:private tail-on 0xc4392c)
(def ^:private tail-brake 0xff4a38)
(def ^:private tail-reverse 0xe8e2d0)
(def ^:private marker-off 0x6e5c36)
(def ^:private marker-on 0xffb43a)

(defn- lamp-mat
  "A lit surface. Basic rather than standard on purpose: a lamp that takes a
  light calculation is a lamp that dims under exactly the cloud that is the
  reason it was switched on, and a headlight that goes out in the rain is worse
  than no headlight at all."
  [hex]
  (three/MeshBasicMaterial. #js {:color hex}))

(def ^:private beam-length 13.0)
(def ^:private beam-radius 1.6)

(defn- beam-geometry
  "An open cone with its apex at the origin, opening along -Z, fading to nothing
  at the mouth.

  Built once and shared. A cone points +Y with its apex at +height/2, so a
  quarter turn about X puts the apex at +Z and the mouth at -Z, and the
  translate slides the apex back onto the lamp it hangs off.

  The fade is a vertex colour rather than an opacity, because opacity is a
  material uniform and cannot vary along a mesh. Under additive blending black
  *is* transparent -- it adds nothing -- so a colour ramp to black at the mouth
  is exactly a beam that runs out. Without it the cone has a hard rim and reads
  as a grey wedge lying on the road rather than as light."
  []
  (let [g (doto (three/ConeGeometry. beam-radius beam-length 12 1 true)
            (.rotateX (/ js/Math.PI 2))
            (.translate 0 0 (* -0.5 beam-length)))
        ^js pos (.getAttribute g "position")
        n (.-count pos)
        col (js/Float32Array. (* 3 n))]
    (dotimes [i n]
      (let [t (min 1.0 (max 0.0 (/ (- (.getZ pos i)) beam-length)))
            ;; Bright at the lamp, gone well before the mouth. Squared, so the
            ;; visible part of the beam is the first third of it.
            v (- 1.0 (* t t))]
        (aset col (* 3 i) v)
        (aset col (+ 1 (* 3 i)) (* 0.94 v))
        (aset col (+ 2 (* 3 i)) (* 0.76 v))))
    (.setAttribute g "color" (three/BufferAttribute. col 3))
    g))

;; --- what a held power-up looks like ----------------------------------------
;;
;; Three of the six power-ups changed only how the car behaved and nothing you
;; could see, which makes the ten seconds you are holding one hard to spend --
;; you cannot tell you still have it without reading the dashboard. All three
;; are unlit and hung on the player's car only; rivals do not collect crates.

(def ^:private jet-length 1.05)
(def ^:private jet-radius 0.17)

(defn- jet-geometry
  "An exhaust flame: a cone whose base sits at the tailpipe and whose apex
  points back down the road. A cone points +Y with the apex at +height/2, so a
  quarter turn about X puts the apex at +Z -- behind the car, since forward is
  -Z -- and the translate slides the base onto the origin."
  []
  (doto (three/ConeGeometry. jet-radius jet-length 8 1 true)
    (.rotateX (/ js/Math.PI 2))
    (.translate 0 0 (* 0.5 jet-length))))

(defn- build-boosts!
  "Nitro flames, a grip ring on the road, and an armour shell.

  Four meshes on one car, all hidden until something is held. They hang off the
  root rather than the hull so a caved-in nose does not bend the shell into the
  bodywork."
  [^js root [hx hy hz] ground]
  (let [flame (fn [] (three/MeshBasicMaterial.
                      #js {:color 0xffb03a :transparent true :opacity 0.85
                           :depthWrite false
                           :blending (.-AdditiveBlending three)
                           :side (.-DoubleSide three)}))
        jets (make-array 2)]
    (dotimes [i 2]
      (let [^js m (three/Mesh. (jet-geometry) (flame))]
        (.set (.-position m) (* (if (zero? i) -0.42 0.42) hx) (* -0.25 hy) (* 1.02 hz))
        (set! (.-visible m) false)
        (aset jets i m)
        (.add root m)))
    ;; Grip: a ring on the road under the car. Flat on the ground rather than
    ;; around the car, because grip is a thing happening at the contact patch
    ;; and that is where the eye goes looking for it.
    ;;
    ;; Sized off the car's *length*, not its width. Off the width it came out
    ;; with a radius of 1.5 m on a body 3.8 m long -- drawn, visible, and
    ;; entirely underneath the car, where nobody could see it.
    (let [^js ring (three/Mesh. (doto (three/RingGeometry. (* 1.02 hz) (* 1.26 hz) 32)
                                  (.rotateX (/ js/Math.PI -2)))
                                (three/MeshBasicMaterial.
                                 #js {:color 0x4fc4ff :transparent true
                                      :opacity 0.0 :depthWrite false
                                      :blending (.-AdditiveBlending three)
                                      :side (.-DoubleSide three)}))
          ;; Armour: a shell a hand's width proud of the bodywork. Front faces:
          ;; back faces were the first try and they are nearly invisible,
          ;; because the far side of the box is behind the car and loses the
          ;; depth test to it. What is left to see is the near panels, which is
          ;; what a shield is anyway.
          ^js shell (three/Mesh. (three/BoxGeometry. (* 2.3 hx) (* 2.5 hy) (* 2.12 hz))
                                 (three/MeshBasicMaterial.
                                  #js {:color 0x9fd8ff :transparent true
                                       :opacity 0.0 :depthWrite false
                                       :blending (.-AdditiveBlending three)
                                       :side (.-FrontSide three)}))]
      ;; On the road, not under the floorpan. `ground` is how far the tyres are
      ;; below the chassis centre, which differs by a metre between a hatchback
      ;; and a truck -- so it is measured per vehicle rather than guessed from
      ;; the hull.
      (.set (.-position ring) 0.0 (+ ground 0.04) 0.0)
      (set! (.-visible ring) false)
      (set! (.-visible shell) false)
      (.add root ring)
      (.add root shell)
      {:jets jets :ring ring :shell shell})))

(defn- draw-boosts!
  "Show what the player is holding. `held` is the set of active power-up names.

  Everything here pulses off the render clock rather than off a timer of its
  own: a flame that does not flicker is a cone, and a ring that does not
  breathe is a decal."
  [{:keys [cars ^js clock]} held]
  (when-let [{:keys [jets ^js ring ^js shell]} (:boosts (first cars))]
    (let [t (* 2 js/Math.PI @clock)
          nitro? (contains? held :nitro)
          grip? (contains? held :grip)
          armour? (contains? held :armour)]
      (dotimes [i 2]
        (let [^js m (aget jets i)]
          (set! (.-visible m) nitro?)
          (when nitro?
            ;; The two flames flicker out of phase, which is most of what stops
            ;; a pair of cones reading as a pair of cones.
            (let [f (+ 0.72 (* 0.28 (js/Math.sin (+ (* 9.0 t) (* i 2.1)))))]
              (set! (.-z (.-scale m)) f)
              (set! (.-opacity (.-material m)) (* 0.9 f))))))
      (set! (.-visible ring) grip?)
      (when grip?
        (set! (.-y (.-rotation ring)) (* 1.4 t))
        (set! (.-opacity (.-material ring)) (+ 0.46 (* 0.14 (js/Math.sin (* 3.0 t))))))
      (set! (.-visible shell) armour?)
      (when armour?
        (set! (.-opacity (.-material shell)) (+ 0.26 (* 0.09 (js/Math.sin (* 2.2 t)))))))))

(def ^:private soot 0x2b2622)
;; Allocated once: `draw-damage!` runs per car per frame and a fresh Color
;; there is four garbage objects a frame for nothing.
(def ^:private soot-colour (three/Color. soot))
(def ^:private puffs 3)

(defn- build-wheels!
  "Four cylinders, sized per corner.

  Geometry is rotated onto the X axis once, at build time, so the per-frame
  quaternion is just steer * spin. Per corner rather than per car because the
  tractor's back wheels are nearly twice the size of its front ones."
  [^js root tex layout]
  (let [{:keys [radii widths]} layout
        m   (mat (:tyre tex))
        out (make-array 4)]
    (dotimes [i 4]
      (let [r (nth radii i)
            geom (doto (three/CylinderGeometry. r r (nth widths i) 16)
                   (.rotateZ (/ js/Math.PI 2)))
            ^js w (three/Mesh. geom m)]
        (set! (.-castShadow w) true)
        (aset out i w)
        (.add root w)))
    out))

(defn- build-body!
  "The shapes bolted to the hull: cabin, bed sides, bull bar, exhaust stack.

  A box is a box, and every vehicle in the catalogue was one until this
  existed. The silhouette is what makes a truck read as a truck from behind the
  wheel, and it costs four child meshes."
  [^js hull kind paint tex lamps]
  (doseq [[x y z hx hy hz tint] (cars/body-parts kind)]
    (let [^js m (three/Mesh. (three/BoxGeometry. (* 2 hx) (* 2 hy) (* 2 hz))
                             (cond
                               (= :paint tint) (paint-mat (:body tex) paint)
                               ;; One material per lamp *group* per car, shared
                               ;; by both lamps in it. Switching the headlights
                               ;; on is then one colour write, not one per bulb.
                               (lamps tint) (lamps tint)
                               :else (three/MeshStandardMaterial.
                                      #js {:color (part-tints tint)
                                           :metalness 0.35 :roughness 0.30})))]
      (.set (.-position m) x y z)
      (.add hull m))))

(defn- build-beams!
  "Two cones of light off the front of the car, hung on the root rather than the
  hull so a caved-in nose does not bend them.

  Additive and unlit, which is the cheap way to draw a beam: it brightens
  whatever is behind it and disappears against a bright sky, which is what a
  headlight in daylight does anyway. Two transparent cones per car is nothing
  next to a spotlight, and a spotlight would have added two more lights to
  every shader in the scene for a pool on the road nobody is looking at."
  [^js root geom kind]
  (let [out (make-array 2)]
    (dotimes [i 2]
      (let [[x y z] (nth (cars/headlamps kind) i)
            ;; A little ahead of the lens. The apex is the brightest part of the
            ;; cone, and starting it inside the bodywork put a bright smudge on
            ;; the nose of the car rather than a beam in front of it.
            z (- z 0.35)
            ^js m (three/Mesh. geom (three/MeshBasicMaterial.
                                     #js {:color 0xffffff :vertexColors true
                                          :transparent true
                                          :opacity 0.0 :depthWrite false
                                          :blending (.-AdditiveBlending three)
                                          :side (.-DoubleSide three)}))]
        (.set (.-position m) x y z)
        (set! (.-visible m) false)
        (aset out i m)
        (.add root m)))
    out))

(defn- build-smoke!
  "A few translucent puffs at the nose, hidden until the engine is cooked.

  Built up front rather than spawned, because a particle system that allocates
  is a particle system that stutters, and three spheres is all this needs to
  read as a car that is about to give up."
  [^js root [_ hy hz]]
  (let [geom (three/SphereGeometry. 0.34 8 6)
        out  (make-array puffs)]
    (dotimes [i puffs]
      (let [^js m (three/Mesh. geom (three/MeshBasicMaterial.
                                     #js {:color 0x9aa0a6 :transparent true
                                          :opacity 0.0 :depthWrite false}))]
        (.set (.-position m) 0.0 hy (- hz))
        (set! (.-visible m) false)
        (aset out i m)
        (.add root m)))
    out))

(defn- build-cars!
  "One tree per vehicle: a root that carries the sim transform, a hull that can
  be deformed without taking the wheels with it, and four wheels.

  The hull used to be the root, which was fine while damage was a number on the
  HUD. It is not fine once a caved-in nose is drawn by squashing the box: the
  wheels are children, and they were squashed with it."
  [^js scene sim tex]
  (let [meshes (make-array sim/max-entities)
        ;; One cone between every headlight in the game. Geometry is shared;
        ;; only the material differs, and only so a beam can fade on its own.
        beam-geom (beam-geometry)
        cars   (mapv
                (fn [i]
                  (let [kind   (sim/kind-of sim i)
                        [hx hy hz :as half] (nth (:halves @sim) i)
                        paint  (if (zero? i)
                                 (cars/paint kind)
                                 (nth rival-paint (mod (dec i) (count rival-paint))))
                        layout (cars/layout kind)
                        ^js root (three/Group.)
                        ^js hull-mat (paint-mat (:body tex) paint)
                        lamps {:lamp (lamp-mat lamp-off)
                               :tail (lamp-mat tail-off)
                               :marker (lamp-mat marker-off)}
                        ^js hull (three/Mesh. (three/BoxGeometry. (* 2 hx) (* 2 hy) (* 2 hz))
                                              hull-mat)]
                    (build-body! hull kind paint tex lamps)
                    (set! (.-castShadow hull) true)
                    (set! (.-receiveShadow hull) true)
                    (doseq [^js c (array-seq (.-children hull))]
                      ;; Everything bolted on casts, except the lamps: a light
                      ;; source with a shadow is a light source that is also
                      ;; somehow opaque.
                      (set! (.-castShadow c) (not (instance? three/MeshBasicMaterial
                                                              (.-material c)))))
                    (.add root hull)
                    (let [wheels (build-wheels! root tex layout)
                          smoke  (build-smoke! root half)
                          beams  (build-beams! root beam-geom kind)
                          ;; Only the player collects crates, so only the
                          ;; player carries the four meshes that show it.
                          ;; How far the tyres sit below the chassis centre:
                          ;; the mount, the suspension it hangs on and the
                          ;; wheel itself.
                          ground (- (+ (js/Math.abs (nth (first (:connections layout)) 1))
                                       (:suspension-rest @(cars/tuning kind) 0.32)
                                       (:radius layout)))
                          boosts (when (zero? i) (build-boosts! root half ground))]
                      (aset meshes i root)
                      (.add scene root)
                      {:root root :hull hull :material hull-mat
                       :paint (three/Color. paint)
                       :half half :wheels wheels :mounts (:connections layout)
                       :smoke smoke :lamps lamps :beams beams :boosts boosts
                       ;; -1 is "never painted", so the first frame always
                       ;; writes. See `draw-lights!`.
                       :lit (volatile! -1)})))
                (range (count (:vehicles @sim))))]
    [meshes cars]))

(defn create!
  "Build renderer, textures, scene, one mesh per sim entity, and the wheels.
  The renderer comes first because texture anisotropy is a device capability."
  [canvas sim seed]
  (let [^js renderer (three/WebGLRenderer. #js {:canvas canvas :antialias true})
        ;; ACES flattens the highlights the way a camera does. Without it the
        ;; sunlit side of every white building clips to paper and the shaded
        ;; side goes to mud, which is most of why the world read as flat.
        _            (set! (.-toneMapping renderer) (.-ACESFilmicToneMapping three))
        _            (set! (.-toneMappingExposure renderer) 1.25)
        _            (set! (.-enabled (.-shadowMap renderer)) true)
        _            (set! (.-type (.-shadowMap renderer)) (.-PCFSoftShadowMap three))
        tex          (textures/build! renderer seed)
        [scene sun]  (build-scene! renderer)
        [meshes cars] (build-cars! scene sim tex)
        ^js cam      (three/PerspectiveCamera. 70 1 0.3 2000)]
    {:renderer       renderer
     :sun            sun
     :textures       tex
     :chunk-material (mat (:ground tex) true)
     :scene        scene
     :camera       cam
     :camera-state (camera/create! cam)
     ;; Closed over here so the camera can query the world for occluders
     ;; without `camera` having to know that a simulation exists.
     :cast         (fn [ox oy oz dx dy dz d] (sim/cast-ray sim ox oy oz dx dy dz d))
     :meshes       meshes
     ;; Hull, wheels, suspension mounts and smoke per vehicle. Mounts are per
     ;; vehicle because a lorry's are nowhere near a hatchback's, and one
     ;; shared table put every wheel in the same place.
     :cars         cars
     :clock        (volatile! 0.0)
     ;; Scratch objects, reused every frame rather than allocated.
     :q0      (three/Quaternion.)
     :q1      (three/Quaternion.)
     :qo      (three/Quaternion.)
     :qa      (three/Quaternion.)
     :qb      (three/Quaternion.)
     ;; The player's interpolated rotation, kept aside as the entity loop passes
     ;; it: the camera needs it after the loop, and `qo` is reused per entity.
     :qp      (three/Quaternion.)
     :axis-x  (three/Vector3. 1 0 0)
     :axis-y  (three/Vector3. 0 1 0)
     :size    (volatile! nil)}))

(defn resize!
  "Called every frame, so it must no-op when nothing changed: `setSize`
  reallocates the drawing buffer and `updateProjectionMatrix` rebuilds the
  camera matrix, neither of which is free at 60 Hz."
  [{:keys [^js renderer ^js camera size]} canvas]
  (let [w (.-clientWidth canvas)
        h (.-clientHeight canvas)]
    (when (and (pos? w) (pos? h) (not= [w h] @size))
      (vreset! size [w h])
      (.setPixelRatio renderer (min 2 (.-devicePixelRatio js/window)))
      (.setSize renderer w h false)
      (set! (.-aspect camera) (/ w h))
      (.updateProjectionMatrix camera))))

(defn- draw-wheels! [{:keys [cars ^js qa ^js qb ^js axis-x ^js axis-y]} sim]
  (let [wheels (:wheels @sim)]
    (dotimes [v (count cars)]
      (let [{set-of :wheels mounts :mounts} (nth cars v)
            base   (* v 4 sim/wheel-stride)]
        (dotimes [i 4]
          (let [o     (+ base (* i sim/wheel-stride))
                ^js w (aget set-of i)
                [cx cy cz] (nth mounts i)]
            ;; The wheel hangs below its chassis connection point by however far
            ;; the suspension is currently extended.
            (.set (.-position w) cx (- cy (aget wheels (+ o 0))) cz)
            (.setFromAxisAngle qa axis-y (aget wheels (+ o 1)))
            (.setFromAxisAngle qb axis-x (aget wheels (+ o 2)))
            (.multiplyQuaternions (.-quaternion w) qa qb)))))))

(defn- draw-damage!
  "Bend the bodywork to match what the car has been through.

  The hull is squashed towards whichever panels have taken damage, and the
  centre is shifted by half as much again so the *undamaged* end stays where it
  was: shrinking a box about its centre pulls both ends in, which reads as a
  car that has been compressed rather than one that has been hit.

  Repainting is a lerp towards soot. It is doing a lot of work for one line --
  a dented shape reads as damage, but a dented shape that is also filthy reads
  as damage from here, at speed, in the mirror."
  [{:keys [cars ^js clock]} sim]
  (dotimes [i (count cars)]
    (let [{:keys [^js hull ^js material ^js paint half smoke]} (nth cars i)
          v   (nth (sim/vehicles sim) i)
          [hx _ hz] half
          f   (vehicle/panel v vehicle/dmg-front)
          r   (vehicle/panel v vehicle/dmg-rear)
          l   (vehicle/panel v vehicle/dmg-left)
          rt  (vehicle/panel v vehicle/dmg-right)
          tot (vehicle/damage v)
          k   0.22]
      (.set (.-scale hull)
            (- 1.0 (* 0.5 k (+ l rt)))
            (- 1.0 (* 0.35 tot))
            (- 1.0 (* 0.5 k (+ f r))))
      (.set (.-position hull)
            (* 0.5 k (- l rt) hx)
            0.0
            (* 0.5 k (- f r) hz))
      (.lerpColors (.-color material) paint soot-colour (min 0.75 (* 0.8 tot)))
      ;; Steam and then smoke, once the radiator is behind a folded bumper.
      ;; Each puff is on its own phase of the same one-second loop, so three
      ;; spheres look like a plume rather than a pulse.
      (dotimes [j puffs]
        (let [^js m (aget smoke j)
              on?   (> f 0.45)]
          (set! (.-visible m) on?)
          (when on?
            (let [t (mod (+ (/ j puffs) @clock) 1.0)]
              (set! (.-y (.-position m)) (+ 0.2 (* 2.2 t)))
              (set! (.-z (.-position m)) (- (* 0.9 hz) (* 0.3 t)))
              (.setScalar (.-scale m) (+ 0.35 (* 1.5 t)))
              (set! (.-opacity (.-material m))
                    (* (min 1.0 (* 1.6 (- f 0.45))) (- 0.55 (* 0.55 t)))))))))))

(defn- draw-lights!
  "Head, tail and marker lamps, and the beams that go with the headlights.

  Three bits of state -- lit, braking, reversing -- packed into an integer and
  compared with what was last painted. A car cruising in the dry costs one
  integer comparison a frame; the writes happen on the frames where something
  actually changed, which is the only place they belong.

  Reversing shows as the tail cluster turning white rather than as a separate
  pair of lamps. A real car has both; at the size these are drawn on screen the
  extra pair would be two more meshes to say something the colour already says.

  The beams are graded by `gloom` rather than switched, because a headlight
  beam is only visible in air that has something in it. Drawn at full strength
  under light cloud it is a grey wedge lying on a sunlit road; graded, it is
  nothing until the sky closes in and then it is the thing lighting your way."
  [{:keys [cars]} sim gloom]
  (dotimes [i (count cars)]
    (let [on?    (pos? gloom)
          {:keys [lamps beams lit]} (nth cars i)
          v      (nth (sim/vehicles sim) i)
          brake? (> (vehicle/brake-of v) 0.05)
          rev?   (> (vehicle/reverse-of v) 0.5)
          ;; The beam strength is quantised into the state word too, or a sky
          ;; that is slowly clouding over would repaint every lamp every frame.
          state  (bit-or (if brake? 1 0) (if rev? 2 0)
                         (bit-shift-left (int (* 8 gloom)) 2))]
      (when (not= state @lit)
        (vreset! lit state)
        (.setHex (.-color ^js (:lamp lamps)) (if on? lamp-on lamp-off))
        (.setHex (.-color ^js (:marker lamps)) (if on? marker-on marker-off))
        (.setHex (.-color ^js (:tail lamps))
                 (cond rev? tail-reverse brake? tail-brake on? tail-on :else tail-off))
        (dotimes [j 2]
          (let [^js m (aget beams j)]
            (set! (.-visible m) on?)
            (set! (.-opacity (.-material m)) (* 0.26 gloom))))))))

(defn draw!
  "Interpolate every entity by `alpha` in [0,1] and present the frame.

  `dt` is the real elapsed frame time, used only by the camera. Everything else
  here is a function of the fixed timestep and `alpha`. `gloom` is the world's
  answer to how dark it is, 0 to 1 -- one answer for every car, so it arrives
  as an argument rather than being asked per vehicle. `held` is the set of
  power-up names the player is holding."
  [{:keys [^js renderer scene ^js camera meshes camera-state cast
           ^js q0 ^js q1 ^js qo ^js qp] :as rs}
   sim alpha dt gloom held]
  (let [{:keys [prev curr halves player]} @sim
        n (count halves)]
    (dotimes [i n]
      (let [o     (* i sim/stride)
            ^js m (aget meshes i)]
        (.set (.-position m)
              (lerp (aget prev (+ o 0)) (aget curr (+ o 0)) alpha)
              (lerp (aget prev (+ o 1)) (aget curr (+ o 1)) alpha)
              (lerp (aget prev (+ o 2)) (aget curr (+ o 2)) alpha))
        (.set q0 (aget prev (+ o 3)) (aget prev (+ o 4))
                 (aget prev (+ o 5)) (aget prev (+ o 6)))
        (.set q1 (aget curr (+ o 3)) (aget curr (+ o 4))
                 (aget curr (+ o 5)) (aget curr (+ o 6)))
        (.slerpQuaternions qo q0 q1 alpha)
        (.copy (.-quaternion m) qo)
        (when (= i player) (.copy qp qo))))
    (draw-wheels! rs sim)
    (vswap! (:clock rs) #(mod (+ % dt) 1.0))
    (draw-damage! rs sim)
    (draw-lights! rs sim gloom)
    (draw-boosts! rs held)
    (let [o (* player sim/stride)]
      (follow-sun! (:sun rs)
                   (lerp (aget prev (+ o 0)) (aget curr (+ o 0)) alpha)
                   (lerp (aget prev (+ o 1)) (aget curr (+ o 1)) alpha)
                   (lerp (aget prev (+ o 2)) (aget curr (+ o 2)) alpha))
      (camera/update! camera-state
                      (lerp (aget prev (+ o 0)) (aget curr (+ o 0)) alpha)
                      (lerp (aget prev (+ o 1)) (aget curr (+ o 1)) alpha)
                      (lerp (aget prev (+ o 2)) (aget curr (+ o 2)) alpha)
                      (.-x qp) (.-y qp) (.-z qp) (.-w qp)
                      (sim/player-speed sim)
                      dt
                      cast))
    (.render renderer scene camera)))
