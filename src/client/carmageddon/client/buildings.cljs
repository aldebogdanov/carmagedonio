(ns carmageddon.client.buildings
  "City blocks: static, solid, and unlike props not going anywhere.

  A building is drawn as a handful of extruded volumes -- walls, a roof, an
  awning, a chimney -- and collided with as a single box. A porch is not worth a
  broad-phase entry, and the coarse footprint the generator emits alongside the
  parts is what the collider is built from, so what you can see and what you hit
  cannot drift apart.

  Parts are grouped per chunk into one `InstancedMesh` per (shape, material):
  four shapes and either the zone's facade or a flat colour. That is at most
  thirteen draws for a chunk holding eight hundred volumes.

  Walls take the facade texture, which has windows in it. Everything else takes
  a flat colour carried per instance, so a terracotta roof and a steel silo
  share one material and one draw."
  (:require ["@dimforge/rapier3d-compat" :as RAPIER]
            ["three" :as three]
            [carmageddon.shared.worldgen :as worldgen]))

(defn- gable-geometry
  "A triangular prism: ridge along Z, base at y = -0.5, unit extents.

  Built by hand because three has no prism. Winding is outward on every face --
  the terrain mesh already taught this project what a reversed normal costs."
  []
  (let [a [-0.5 -0.5 -0.5] b [0.5 -0.5 -0.5] c [0.5 -0.5 0.5] d [-0.5 -0.5 0.5]
        e [0.0 0.5 -0.5]  f [0.0 0.5 0.5]
        tris [a d f, a f e            ; left slope
              b e f, b f c            ; right slope
              a e b                   ; gable end, -Z
              d c f                   ; gable end, +Z
              a b c, a c d]           ; underside
        pos (js/Float32Array. (* 3 (count tris)))]
    (doseq [[i v] (map-indexed vector tris)]
      (aset pos (* i 3) (nth v 0))
      (aset pos (+ 1 (* i 3)) (nth v 1))
      (aset pos (+ 2 (* i 3)) (nth v 2)))
    (doto (three/BufferGeometry.)
      (.setAttribute "position" (three/BufferAttribute. pos 3))
      (.computeVertexNormals))))

(defn shapes
  "The four extruded volumes, shared with `bridges` -- a deck is a scaled box
  and a pier is a scaled cylinder, exactly as a wall and a chimney are."
  []
  {:box      (three/BoxGeometry. 1 1 1)
   :gable    (gable-geometry)
   ;; A four-sided cone is a pyramid, but its square base is on the diagonal --
   ;; turning it a quarter of a right angle and growing the radius by root two
   ;; puts the corners on the unit box, so scaling behaves like every other part.
   :pyramid  (doto (three/ConeGeometry. (/ (js/Math.sqrt 2) 2) 1 4)
               (.rotateY (/ js/Math.PI 4)))
   :cylinder (three/CylinderGeometry. 0.5 0.5 1 12)
   ;; Twenty faces, which is all a tree canopy needs at the distance one is
   ;; ever looked at.
   :blob     (three/IcosahedronGeometry. 0.5 0)})

;; --- facades at real size --------------------------------------------------
;;
;; Every wall is the same unit cube scaled to the building, so its UVs run 0..1
;; across each face whatever the face measures -- which made a window a fixed
;; *fraction* of a wall. The shader below works out how big the face actually
;; is from the instance's own matrix, and lays the zone's tile across it at a
;; real bay width and storey height: as many storeys as fit, stretched a little
;; so the last one is not cut off at the roofline, and likewise for bays at the
;; corners. The bottom storey takes the ground-floor tile instead.
;;
;; Each wall also gets a whole number of bays and storeys of offset into the
;; tile, hashed from where it stands, so a street of the same zone is not the
;; same lit windows repeated down it.

(def ^:private facade-vertex
  "
  #include <uv_vertex>
  {
    float fsx = length(instanceMatrix[0].xyz);
    float fsy = length(instanceMatrix[1].xyz);
    float fsz = length(instanceMatrix[2].xyz);
    float along = abs(normal.x) > 0.5 ? fsz : fsx;
    vFacade = vec3(uv.x * along, uv.y * fsy, abs(normal.y) > 0.5 ? 1.0 : 0.0);
    vFacadeSize = vec2(along, fsy);
    vFacadeSeed = fract(sin(dot(instanceMatrix[3].xz, vec2(12.9898, 78.233))) * 43758.5453);
  }
  ")

(def ^:private facade-fragment
  "
  #ifdef USE_MAP
  {
    float t = vFacade.y - facadeSink;
    float upperH = vFacadeSize.y - facadeSink - facadeGround;
    // Too little above the ground storey for a floor of its own: a bungalow,
    // or an office podium. The ground storey takes the whole wall instead of
    // leaving a squashed strip of window over the door.
    float groundH = upperH < 0.5 * facadeCell.y ? max(facadeGround, vFacadeSize.y - facadeSink)
                                                : facadeGround;
    upperH = max(0.01, vFacadeSize.y - facadeSink - groundH);
    float bays = max(1.0, floor(vFacadeSize.x / facadeCell.x + 0.5));
    float bayW = vFacadeSize.x / bays;
    float storeys = max(1.0, floor(upperH / facadeCell.y + 0.5));
    float storeyH = upperH / storeys;
    float u = vFacade.x / (bayW * facadeTile.x)
              + floor(vFacadeSeed * facadeTile.x) / facadeTile.x;
    float vu = (t - groundH) / (storeyH * facadeTile.y)
               + floor(fract(vFacadeSeed * 7.31) * facadeTile.y) / facadeTile.y;
    float vg = clamp(t / max(groundH, 0.01), 0.0, 1.0);
    vec4 upper = texture2D(map, vec2(u, vu));
    vec4 ground = texture2D(facadeGroundMap, vec2(u, vg));
    // The top of a wall is a roof, or under one: bare wall, no windows.
    vec4 roof = texture2D(map, vec2(0.004, 0.996));
    vec4 c = t < groundH ? ground : upper;
    diffuseColor *= mix(c, roof, vFacade.z);
  }
  #endif
  ")

(defn- facade-material
  "A wall material for one zone. `street?` false is the variant for a wall
  that starts above the street, which has no ground storey and is not sunk."
  [{:keys [upper ground spec]} street?]
  (let [{:keys [bay storey cols rows]} spec
        ground-h (if street? (:ground spec) 0.0)
        ^js m (three/MeshPhongMaterial. #js {:map upper :shininess 8})]
    (set! (.-onBeforeCompile m)
          (fn [^js shader]
            (let [^js u (.-uniforms shader)]
              (aset u "facadeGroundMap" #js {:value ground})
              (aset u "facadeCell" #js {:value (three/Vector2. bay storey)})
              (aset u "facadeTile" #js {:value (three/Vector2. cols rows)})
              (aset u "facadeGround" #js {:value ground-h})
              ;; Every building is sunk this far so it meets a slope on all
              ;; sides; see `worldgen/chunk-structures`.
              (aset u "facadeSink" #js {:value (if street? 0.6 0.0)}))
            (set! (.-vertexShader shader)
                  (-> (.-vertexShader shader)
                      (.replace "#include <common>"
                                "#include <common>
                                 varying vec3 vFacade;
                                 varying vec2 vFacadeSize;
                                 varying float vFacadeSeed;")
                      (.replace "#include <uv_vertex>" facade-vertex)))
            (set! (.-fragmentShader shader)
                  (-> (.-fragmentShader shader)
                      (.replace "#include <common>"
                                "#include <common>
                                 varying vec3 vFacade;
                                 varying vec2 vFacadeSize;
                                 varying float vFacadeSeed;
                                 uniform sampler2D facadeGroundMap;
                                 uniform vec2 facadeCell;
                                 uniform vec2 facadeTile;
                                 uniform float facadeGround;
                                 uniform float facadeSink;")
                      (.replace "#include <map_fragment>" facade-fragment)))))
    m))

(defn create [world scene textures]
  (atom {:world world :scene scene
         :geometries (shapes)
         :facades (mapv #(facade-material % true) (:facades textures))
         :uppers (mapv #(facade-material % false) (:facades textures))
         :plain (three/MeshPhongMaterial. #js {:color 0xffffff :shininess 6
                                               :flatShading true})
         :scratch (three/Object3D.)
         :colour (three/Color.)
         :chunks {}}))

(defn- group-parts
  "Bucket a chunk's parts by (shape, material).

  Walls are always boxes, so a facade group needs no shape in its key -- but the
  shape is kept anyway rather than assumed, because the day a zone grows a
  windowed gable this would otherwise draw it as a box without a word."
  [arr]
  (let [st worldgen/building-part-stride
        n  (/ (.-length arr) st)]
    (reduce (fn [acc i]
              (let [o (* i st)
                    prim (nth worldgen/building-prims (int (aget arr (+ o 7))))
                    mat  (aget arr (+ o 8))
                    plain? (neg? mat)]
                (update acc [prim (if plain? :plain (int mat))] (fnil conj [])
                        {:x (aget arr (+ o 0)) :y (aget arr (+ o 1)) :z (aget arr (+ o 2))
                         :yaw (aget arr (+ o 3))
                         :sx (aget arr (+ o 4)) :sy (aget arr (+ o 5))
                         :sz (aget arr (+ o 6))
                         ;; Left as it came. For a flat part this is a packed
                         ;; colour and for a wall it is a shade multiplier --
                         ;; see `worldgen/chunk-structures` -- and truncating
                         ;; it here turned every wall's shade into zero.
                         :tint (aget arr (+ o 9))})))
            {} (range n))))

(defn- build-group!
  [bs [prim mat] instances]
  (let [{:keys [^js scene geometries facades uppers ^js plain ^js scratch ^js colour]} @bs
        plain? (= :plain mat)
        upper? (and (not plain?) (>= mat worldgen/upper-mat))
        ^js m (three/InstancedMesh. (get geometries prim)
                                    (cond plain? plain
                                          upper? (nth uppers (- mat (int worldgen/upper-mat)))
                                          :else (nth facades mat))
                                    (count instances))]
    (doseq [[i {:keys [x y z yaw sx sy sz tint]}] (map-indexed vector instances)]
      (.set (.-position scratch) x y z)
      (.set (.-rotation scratch) 0 yaw 0)
      (.set (.-scale scratch) sx sy sz)
      (.updateMatrix scratch)
      (.setMatrixAt m i (.-matrix scratch))
      (if (or plain? (> tint 8.0))
        ;; A flat part's colour, or a painted wall's paint: both packed.
        (.setHex colour (int tint))
        ;; A wall's tint modulates its zone's facade rather than replacing it,
        ;; so it is one number applied to all three channels -- and it may be
        ;; greater than one, which is why it is set as a float rather than
        ;; packed into a hex.
        (.setRGB colour tint tint tint))
      (.setColorAt m i colour))
    ;; A chunk's instances span 256 m, which the unit shape's own bounding sphere
    ;; knows nothing about; leaving culling on makes whole blocks wink out when
    ;; the chunk origin leaves the frustum.
    (set! (.-frustumCulled m) false)
    ;; Buildings are what the shadows are for. They both cast and receive: a
    ;; block that shades the street but not its own side walls reads wrong.
    (set! (.-castShadow m) true)
    (set! (.-receiveShadow m) true)
    (set! (.-needsUpdate (.-instanceMatrix m)) true)
    (when-let [ic (.-instanceColor m)] (set! (.-needsUpdate ic) true))
    (.add scene m)
    m))

(defn- add-colliders!
  "One box per building, from the coarse footprint array."
  [bs arr]
  (let [{:keys [^js world]} @bs
        st worldgen/building-stride]
    (mapv (fn [i]
            (let [o (* i st)
                  x (aget arr (+ o 0)) y (aget arr (+ o 1)) z (aget arr (+ o 2))
                  hx (aget arr (+ o 3)) hz (aget arr (+ o 4))
                  half-h (* 0.5 (aget arr (+ o 5)))
                  yaw (aget arr (+ o 7))]
              (.createCollider
               world
               (-> (.cuboid RAPIER/ColliderDesc hx half-h hz)
                   (.setTranslation x (+ y half-h -0.6) z)
                   (.setRotation #js {:x 0.0 :y (js/Math.sin (* 0.5 yaw))
                                      :z 0.0 :w (js/Math.cos (* 0.5 yaw))})
                   (.setFriction 0.9)
                   (.setRestitution 0.05)))))
          (range (/ (.-length arr) st)))))

(defn add-chunk! [bs key boxes parts]
  (when (and boxes (pos? (.-length boxes)))
    (let [meshes (into {} (for [[k instances] (group-parts parts) :when (seq instances)]
                            [k (build-group! bs k instances)]))
          colliders (add-colliders! bs boxes)]
      (swap! bs assoc-in [:chunks key] {:meshes meshes :colliders colliders
                                        :count (/ (.-length boxes)
                                                  worldgen/building-stride)})
      meshes)))

(defn remove-chunk!
  "The shapes and the facade materials are shared and stay; an InstancedMesh
  owns its per-instance matrix and colour buffers and does not."
  [bs key]
  (let [{:keys [^js world ^js scene chunks]} @bs]
    (when-let [{:keys [meshes colliders]} (get chunks key)]
      (doseq [[_ ^js m] meshes]
        (.remove scene m)
        (.dispose m))
      (doseq [^js c colliders] (.removeCollider world c true)))
    (swap! bs update :chunks dissoc key)))

(defn stats [bs]
  {:standing (reduce + (map :count (vals (:chunks @bs))))
   :volumes  (reduce + (for [{:keys [meshes]} (vals (:chunks @bs))
                             [_ ^js m] meshes]
                         (.-count m)))
   :chunks (count (:chunks @bs))})
