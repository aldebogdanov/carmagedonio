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
            [carmageddon.client.sim :as sim]
            [carmageddon.client.textures :as textures]
            [carmageddon.shared.constants :as k]))

(def ^:private sky 0x9ec4e2)

(defn- lerp [a b t] (+ a (* (- b a) t)))

(defn- mat
  ([tex] (mat tex false))
  ([tex vertex-colors?]
   (three/MeshPhongMaterial. #js {:map tex :shininess 6
                                  :vertexColors vertex-colors?})))

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

(defn- build-wheels!
  "Four cylinders parented to the chassis. Geometry is rotated onto the X axis
  once, at build time, so the per-frame quaternion is just steer * spin."
  [^js chassis-mesh tex]
  (let [geom (doto (three/CylinderGeometry. sim/wheel-radius sim/wheel-radius 0.26 16)
               (.rotateZ (/ js/Math.PI 2)))
        m    (mat (:tyre tex))
        out  (make-array 4)]
    (dotimes [i 4]
      (let [^js w (three/Mesh. geom m)]
        (aset out i w)
        (.add chassis-mesh w)))
    out))

(defn- build-meshes! [^js scene sim tex]
  (let [meshes (make-array sim/max-entities)]
    (doseq [[i [hx hy hz]] (map-indexed vector (:halves @sim))]
      (let [^js m (three/Mesh. (three/BoxGeometry. (* 2 hx) (* 2 hy) (* 2 hz))
                               ;; Opponents share the player's body texture; telling them apart is the
        ;; camera's job, not the paintwork's.
        (mat (:body tex)))]
        (aset meshes i m)
        (.add scene m)))
    meshes))

(defn create!
  "Build renderer, textures, scene, one mesh per sim entity, and the wheels.
  The renderer comes first because texture anisotropy is a device capability."
  [canvas sim seed]
  (let [^js renderer (three/WebGLRenderer. #js {:canvas canvas :antialias true})
        tex          (textures/build! renderer seed)
        scene        (build-scene!)
        meshes       (build-meshes! scene sim tex)
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
     ;; One set of wheels per vehicle, parented to that vehicle's chassis.
     :wheel-meshes (mapv (fn [i] (build-wheels! (aget meshes i) tex))
                         (range (count (:vehicles @sim))))
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

(defn- draw-wheels! [{:keys [wheel-meshes ^js qa ^js qb ^js axis-x ^js axis-y]} sim]
  (let [wheels (:wheels @sim)]
    (dotimes [v (count wheel-meshes)]
      (let [set-of (nth wheel-meshes v)
            base   (* v 4 sim/wheel-stride)]
        (dotimes [i 4]
          (let [o     (+ base (* i sim/wheel-stride))
                ^js w (aget set-of i)
                [cx cy cz] (nth sim/wheel-connections i)]
            ;; The wheel hangs below its chassis connection point by however far
            ;; the suspension is currently extended.
            (.set (.-position w) cx (- cy (aget wheels (+ o 0))) cz)
            (.setFromAxisAngle qa axis-y (aget wheels (+ o 1)))
            (.setFromAxisAngle qb axis-x (aget wheels (+ o 2)))
            (.multiplyQuaternions (.-quaternion w) qa qb)))))))

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
