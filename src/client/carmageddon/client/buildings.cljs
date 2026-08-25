(ns carmageddon.client.buildings
  "City blocks: static, solid, and unlike props not going anywhere.

  Buildings need no per-frame sync and no destruction bookkeeping, which is why
  they are separate from `props` rather than a flag on it -- the two have almost
  nothing in common at runtime.

  One `InstancedMesh` per zone per chunk. Since buildings started standing on
  real plots there are well over a hundred in a downtown chunk rather than ten,
  and a mesh apiece would be thousands of draw calls for nine shapes repeated.
  Instancing makes it at most nine per chunk.

  Every zone shares one unit cube, scaled per instance, and has its own facade
  texture. Geometry per building would mean tracking and disposing a few
  thousand GPU buffers as the player drives."
  (:require ["@dimforge/rapier3d-compat" :as RAPIER]
            ["three" :as three]
            [carmageddon.shared.worldgen :as worldgen]))

(defn create [world scene textures]
  (atom {:world world :scene scene
         :geometry (three/BoxGeometry. 1 1 1)
         :materials (mapv (fn [t]
                            (three/MeshPhongMaterial. #js {:map t :shininess 8}))
                          (:facades textures))
         :scratch (three/Object3D.)
         :chunks {}}))

(defn- read-buildings
  "Group a chunk's flat array by zone, ready to instance."
  [arr]
  (let [st worldgen/building-stride
        n  (/ (.-length arr) st)]
    (reduce (fn [acc i]
              (let [o (* i st)]
                (update acc (int (aget arr (+ o 6))) (fnil conj [])
                        {:x (aget arr (+ o 0)) :y (aget arr (+ o 1)) :z (aget arr (+ o 2))
                         :hx (aget arr (+ o 3)) :hz (aget arr (+ o 4))
                         :height (aget arr (+ o 5))
                         :yaw (aget arr (+ o 7))})))
            {} (range n))))

(defn- spawn-zone!
  [bs zone instances]
  (let [{:keys [^js world ^js scene ^js geometry materials ^js scratch]} @bs
        ^js m (three/InstancedMesh. geometry
                                    (nth materials (mod zone (count materials)))
                                    (count instances))
        colliders
        (mapv (fn [[i {:keys [x y z hx hz height yaw]}]]
                (let [half-h (* 0.5 height)
                      ;; Sunk slightly so a building on a slope meets the ground
                      ;; on every side rather than showing daylight under the
                      ;; downhill corner.
                      cy (+ y half-h -0.6)
                      qy (js/Math.sin (* 0.5 yaw))
                      qw (js/Math.cos (* 0.5 yaw))]
                  (.set (.-position scratch) x cy z)
                  (.set (.-rotation scratch) 0 yaw 0)
                  (.set (.-scale scratch) (* 2 hx) height (* 2 hz))
                  (.updateMatrix scratch)
                  (.setMatrixAt m i (.-matrix scratch))
                  (.createCollider
                   world
                   (-> (.cuboid RAPIER/ColliderDesc hx half-h hz)
                       (.setTranslation x cy z)
                       (.setRotation #js {:x 0.0 :y qy :z 0.0 :w qw})
                       (.setFriction 0.9)
                       (.setRestitution 0.05)))))
              (map-indexed vector instances))]
    ;; A chunk's instances span 256 m, which the unit cube's own bounding sphere
    ;; knows nothing about; leaving culling on makes whole blocks wink out when
    ;; the chunk origin leaves the frustum.
    (set! (.-frustumCulled m) false)
    (set! (.-needsUpdate (.-instanceMatrix m)) true)
    (.add scene m)
    {:mesh m :colliders colliders}))

(defn add-chunk! [bs key arr]
  (when (and arr (pos? (.-length arr)))
    (let [made (into {} (for [[zone instances] (read-buildings arr)
                              :when (seq instances)]
                          [zone (spawn-zone! bs zone instances)]))]
      (swap! bs assoc-in [:chunks key] made)
      made)))

(defn remove-chunk!
  "The unit cube and the facade materials are shared and stay; an InstancedMesh
  owns its per-instance matrix buffer and does not."
  [bs key]
  (let [{:keys [^js world ^js scene chunks]} @bs]
    (doseq [[_ {:keys [^js mesh colliders]}] (get chunks key)]
      (.remove scene mesh)
      (.dispose mesh)
      (doseq [^js c colliders] (.removeCollider world c true)))
    (swap! bs update :chunks dissoc key)))

(defn stats [bs]
  {:standing (reduce + (for [[_ zones] (:chunks @bs)
                             [_ {:keys [^js mesh]}] zones]
                         (.-count mesh)))
   :chunks (count (:chunks @bs))})
