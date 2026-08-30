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

(def ^:private sky 0x9ec4e2)

(defn- lerp [a b t] (+ a (* (- b a) t)))

(defn- mat
  ([tex] (mat tex false nil))
  ([tex vertex-colors?] (mat tex vertex-colors? nil))
  ([tex vertex-colors? colour]
   (three/MeshPhongMaterial. (cond-> #js {:map tex :shininess 6
                                          :vertexColors vertex-colors?}
                               colour (doto (aset "color" colour))))))

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
    (.add scene m)
    m))

(defn remove-chunk!
  "Geometry holds GPU buffers that the garbage collector cannot reclaim on its
  own, so unloading a chunk has to dispose explicitly or the process leaks
  steadily as the player drives."
  [{:keys [^js scene]} ^js m]
  (.remove scene m)
  (.dispose (.-geometry m)))

(defn- build-scene! []
  (let [scene   (three/Scene.)
        ^js sun (three/DirectionalLight. 0xffffff 1.25)]
    (set! (.-background scene) (three/Color. sky))
    ;; Fog has to close in before the streaming radius ends, or chunks visibly
    ;; pop into existence at the horizon.
    (set! (.-fog scene) (three/Fog. sky 120 (* 0.92 k/stream-radius k/chunk-size)))
    (.set (.-position sun) 40 80 20)
    (.add scene sun)
    (.add scene (three/HemisphereLight. 0xffffff 0x4a5240 1.0))
    scene))

(def ^:private part-tints
  "Bodywork that is not paint. Glass is dark and slightly blue; trim is the
  bumpers, stacks and spoilers, which read best as near-black."
  {:glass 0x1d2733
   :trim  0x2b2b2e})

(def ^:private rival-paint
  "Rivals are painted from their own palette rather than their catalogue
  colour, so a truck bearing down on you is distinguishable at a glance from
  the traffic. Which one you are looking at is the map's job; *that it is
  hostile* has to be readable from the mirror."
  [0xd8722c 0x7d3fa8 0xb8322f 0x1c7d74 0xc9a227])

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
        (aset out i w)
        (.add root w)))
    out))

(defn- build-body!
  "The shapes bolted to the hull: cabin, bed sides, bull bar, exhaust stack.

  A box is a box, and every vehicle in the catalogue was one until this
  existed. The silhouette is what makes a truck read as a truck from behind the
  wheel, and it costs four child meshes."
  [^js hull kind paint tex]
  (doseq [[x y z hx hy hz tint] (:body (cars/spec kind))]
    (let [^js m (three/Mesh. (three/BoxGeometry. (* 2 hx) (* 2 hy) (* 2 hz))
                             (if (= :paint tint)
                               (mat (:body tex) false paint)
                               (three/MeshPhongMaterial.
                                #js {:color (part-tints tint) :shininess 24})))]
      (.set (.-position m) x y z)
      (.add hull m))))

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
        cars   (mapv
                (fn [i]
                  (let [kind   (sim/kind-of sim i)
                        [hx hy hz :as half] (nth (:halves @sim) i)
                        paint  (if (zero? i)
                                 (cars/paint kind)
                                 (nth rival-paint (mod (dec i) (count rival-paint))))
                        layout (cars/layout kind)
                        ^js root (three/Group.)
                        ^js hull-mat (mat (:body tex) false paint)
                        ^js hull (three/Mesh. (three/BoxGeometry. (* 2 hx) (* 2 hy) (* 2 hz))
                                              hull-mat)]
                    (build-body! hull kind paint tex)
                    (.add root hull)
                    (let [wheels (build-wheels! root tex layout)
                          smoke  (build-smoke! root half)]
                      (aset meshes i root)
                      (.add scene root)
                      {:root root :hull hull :material hull-mat
                       :paint (three/Color. paint)
                       :half half :wheels wheels :mounts (:connections layout)
                       :smoke smoke})))
                (range (count (:vehicles @sim))))]
    [meshes cars]))

(defn create!
  "Build renderer, textures, scene, one mesh per sim entity, and the wheels.
  The renderer comes first because texture anisotropy is a device capability."
  [canvas sim seed]
  (let [^js renderer (three/WebGLRenderer. #js {:canvas canvas :antialias true})
        tex          (textures/build! renderer seed)
        scene        (build-scene!)
        [meshes cars] (build-cars! scene sim tex)
        ^js cam      (three/PerspectiveCamera. 70 1 0.3 2000)]
    {:renderer       renderer
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

(defn draw!
  "Interpolate every entity by `alpha` in [0,1] and present the frame.

  `dt` is the real elapsed frame time, used only by the camera. Everything else
  here is a function of the fixed timestep and `alpha`."
  [{:keys [^js renderer scene ^js camera meshes camera-state cast
           ^js q0 ^js q1 ^js qo ^js qp] :as rs}
   sim alpha dt]
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
    (let [o (* player sim/stride)]
      (camera/update! camera-state
                      (lerp (aget prev (+ o 0)) (aget curr (+ o 0)) alpha)
                      (lerp (aget prev (+ o 1)) (aget curr (+ o 1)) alpha)
                      (lerp (aget prev (+ o 2)) (aget curr (+ o 2)) alpha)
                      (.-x qp) (.-y qp) (.-z qp) (.-w qp)
                      (sim/player-speed sim)
                      dt
                      cast))
    (.render renderer scene camera)))
