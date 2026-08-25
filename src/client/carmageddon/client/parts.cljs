(ns carmageddon.client.parts
  "Instanced world volumes with optional colliders.

  One `InstancedMesh` per shape per chunk, a flat colour carried per instance,
  and a cuboid collider for every part the generator marked `solid`. That last
  flag is the whole reason this is one module rather than several: a bridge deck
  is solid and its piers are not, a tree trunk is solid and its canopy is not,
  and the distinction is per part, not per kind of thing.

  Bridges were the first user. A bridge is the one piece of road that is not
  terrain: everywhere else the ground is flattened to meet the carriageway and
  the heightfield collider is what the car drives on, but over a span the valley
  is left alone, so the deck has to carry its own collider or the car falls into
  the river. Trees and hedges arrived with the same shape of problem and the
  same answer."
  (:require ["@dimforge/rapier3d-compat" :as RAPIER]
            ["three" :as three]
            [carmageddon.client.buildings :as buildings]
            [carmageddon.shared.worldgen :as worldgen]))

(defn create [world scene]
  (atom {:world world :scene scene
         :geometries (buildings/shapes)
         :material (three/MeshPhongMaterial. #js {:color 0xffffff :shininess 8
                                                  :flatShading true})
         :scratch (three/Object3D.)
         :colour (three/Color.)
         :chunks {}}))

(defn- read-parts [arr]
  (let [st worldgen/part-stride
        n  (/ (.-length arr) st)]
    (mapv (fn [i]
            (let [o (* i st)]
              {:x (aget arr (+ o 0)) :y (aget arr (+ o 1)) :z (aget arr (+ o 2))
               :yaw (aget arr (+ o 3)) :pitch (aget arr (+ o 4))
               :sx (aget arr (+ o 5)) :sy (aget arr (+ o 6)) :sz (aget arr (+ o 7))
               :prim (nth worldgen/part-prims (int (aget arr (+ o 8))))
               :tint (int (aget arr (+ o 9)))
               :solid? (pos? (aget arr (+ o 10)))}))
          (range n))))

(defn- build-group! [bs prim instances]
  (let [{:keys [^js scene geometries ^js material ^js scratch ^js colour]} @bs
        ^js m (three/InstancedMesh. (get geometries prim) material (count instances))]
    (doseq [[i {:keys [x y z yaw pitch sx sy sz tint]}] (map-indexed vector instances)]
      (.set (.-position scratch) x y z)
      ;; YXZ order: pitch is applied in the part's own frame, after the yaw has
      ;; turned it along the span. With the default XYZ the deck would tilt
      ;; about a world axis and slide off the road.
      (.set (.-rotation scratch) (- pitch) yaw 0 "YXZ")
      (.set (.-scale scratch) sx sy sz)
      (.updateMatrix scratch)
      (.setMatrixAt m i (.-matrix scratch))
      (.setHex colour tint)
      (.setColorAt m i colour))
    (set! (.-frustumCulled m) false)
    (set! (.-needsUpdate (.-instanceMatrix m)) true)
    (when-let [ic (.-instanceColor m)] (set! (.-needsUpdate ic) true))
    (.add scene m)
    m))

(defn- add-colliders! [bs parts]
  (let [{:keys [^js world ^js scratch]} @bs]
    (into []
          (for [{:keys [x y z yaw pitch sx sy sz solid?]} parts :when solid?]
            (let [_ (.set (.-rotation scratch) (- pitch) yaw 0 "YXZ")
                  ;; Take the quaternion from the same object that placed the
                  ;; mesh, so what is drawn and what is collided with cannot
                  ;; disagree about which way a sloping deck leans.
                  ^js q (.-quaternion scratch)]
              (.createCollider
               world
               (-> (.cuboid RAPIER/ColliderDesc (* 0.5 sx) (* 0.5 sy) (* 0.5 sz))
                   (.setTranslation x y z)
                   (.setRotation #js {:x (.-x q) :y (.-y q) :z (.-z q) :w (.-w q)})
                   ;; A deck is road, so it wants the grip the terrain has.
                   (.setFriction 0.9)
                   (.setRestitution 0.05))))))))

(defn add-chunk! [bs key arr]
  (when (and arr (pos? (.-length arr)))
    (let [parts (read-parts arr)
          meshes (into {} (for [[prim group] (group-by :prim parts) :when (seq group)]
                            [prim (build-group! bs prim group)]))]
      (swap! bs assoc-in [:chunks key]
             {:meshes meshes
              :colliders (add-colliders! bs parts)
              :solid (count (filter :solid? parts))})
      meshes)))

(defn remove-chunk! [bs key]
  (let [{:keys [^js world ^js scene chunks]} @bs]
    (when-let [{:keys [meshes colliders]} (get chunks key)]
      (doseq [[_ ^js m] meshes]
        (.remove scene m)
        (.dispose m))
      (doseq [^js c colliders] (.removeCollider world c true)))
    (swap! bs update :chunks dissoc key)))

(defn stats [bs]
  {:parts (reduce + (for [{:keys [meshes]} (vals (:chunks @bs))
                          [_ ^js m] meshes]
                      (.-count m)))
   :solid (reduce + (map :solid (vals (:chunks @bs))))})
