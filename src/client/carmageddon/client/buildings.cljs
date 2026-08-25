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

(defn- geometries []
  {:box      (three/BoxGeometry. 1 1 1)
   :gable    (gable-geometry)
   ;; A four-sided cone is a pyramid, but its square base is on the diagonal --
   ;; turning it a quarter of a right angle and growing the radius by root two
   ;; puts the corners on the unit box, so scaling behaves like every other part.
   :pyramid  (doto (three/ConeGeometry. (/ (js/Math.sqrt 2) 2) 1 4)
               (.rotateY (/ js/Math.PI 4)))
   :cylinder (three/CylinderGeometry. 0.5 0.5 1 12)})

(defn create [world scene textures]
  (atom {:world world :scene scene
         :geometries (geometries)
         :facades (mapv (fn [t] (three/MeshPhongMaterial. #js {:map t :shininess 8}))
                        (:facades textures))
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
                         :tint (int (aget arr (+ o 9)))})))
            {} (range n))))

(defn- build-group!
  [bs [prim mat] instances]
  (let [{:keys [^js scene geometries facades ^js plain ^js scratch ^js colour]} @bs
        plain? (= :plain mat)
        ^js m (three/InstancedMesh. (get geometries prim)
                                    (if plain? plain (nth facades (mod mat (count facades))))
                                    (count instances))]
    (doseq [[i {:keys [x y z yaw sx sy sz tint]}] (map-indexed vector instances)]
      (.set (.-position scratch) x y z)
      (.set (.-rotation scratch) 0 yaw 0)
      (.set (.-scale scratch) sx sy sz)
      (.updateMatrix scratch)
      (.setMatrixAt m i (.-matrix scratch))
      (when plain?
        (.setHex colour tint)
        (.setColorAt m i colour)))
    ;; A chunk's instances span 256 m, which the unit shape's own bounding sphere
    ;; knows nothing about; leaving culling on makes whole blocks wink out when
    ;; the chunk origin leaves the frustum.
    (set! (.-frustumCulled m) false)
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
