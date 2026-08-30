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
  same answer.

  `solid` is three-valued. 0 is decoration, 1 is fixed scenery, and 2 is
  scenery that can be knocked out of the way: a fixed body that becomes a
  dynamic one the moment something hits it hard enough. Bridge parapets are the
  reason it exists. With solid parapets a span measured out as a corridor -- the
  harness drove a car along one at full throttle and full lock for five seconds
  and it never left the deck -- and the fall is half the point of a bridge."
  (:require ["@dimforge/rapier3d-compat" :as RAPIER]
            ["three" :as three]
            [carmageddon.client.buildings :as buildings]
            [carmageddon.client.overlay :as overlay]
            [carmageddon.shared.worldgen :as worldgen]))

(def ^:private smash-impulse 3.0)

(defn create
  ([world scene] (create world scene nil))
  ([world scene ov]
   (atom {:world world :scene scene
          :overlay ov
          :geometries (buildings/shapes)
          :material (three/MeshPhongMaterial. #js {:color 0xffffff :shininess 8
                                                   :flatShading true})
          :scratch (three/Object3D.)
          :colour (three/Color.)
          :chunks {}
          ;; collider handle -> {:key :idx}, for the parts that can be hit
          :by-collider {}
          ;; the ones that have been, and are still tumbling
          :loose []
          :smashed 0})))

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
               :solid (int (aget arr (+ o 10)))
               :idx i}))
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
    ;; Trees and bridge decks. A tree without a shadow sits on the grass
    ;; rather than in it.
    (set! (.-castShadow m) true)
    (set! (.-receiveShadow m) true)
    (set! (.-needsUpdate (.-instanceMatrix m)) true)
    (when-let [ic (.-instanceColor m)] (set! (.-needsUpdate ic) true))
    (.add scene m)
    m))

(defn- orient!
  "Set `scratch` to a part's rotation and return its quaternion.

  Read from the same object that places the mesh, so what is drawn and what is
  collided with cannot disagree about which way a sloping deck leans."
  [^js scratch yaw pitch]
  (.set (.-rotation scratch) (- pitch) yaw 0 "YXZ")
  (.-quaternion scratch))

(defn- shape
  [{:keys [sx sy sz]}]
  (-> (.cuboid RAPIER/ColliderDesc (* 0.5 sx) (* 0.5 sy) (* 0.5 sz))
      ;; A deck is road, so it wants the grip the terrain has.
      (.setFriction 0.9)
      (.setRestitution 0.05)))

(defn- add-fixed!
  "Colliders with no body behind them, for everything that will never move."
  [bs parts]
  (let [{:keys [^js world ^js scratch]} @bs]
    (into []
          (for [{:keys [x y z yaw pitch] :as p} parts :when (= 1 (:solid p))]
            (let [^js q (orient! scratch yaw pitch)]
              (.createCollider
               world
               (-> (shape p)
                   (.setTranslation x y z)
                   (.setRotation #js {:x (.-x q) :y (.-y q) :z (.-z q) :w (.-w q)}))))))))

(defn- add-breakable!
  "Fixed rigid bodies that can be promoted to dynamic ones.

  A body rather than a bare collider, because a bare collider cannot be given
  a velocity: the whole point is that the panel leaves with the car. It starts
  Fixed so it costs the solver nothing until something hits it, which is the
  same trick the traffic uses for its wrecks."
  [bs key parts prim-index]
  (let [{:keys [^js world ^js scratch overlay]} @bs
        gone (if overlay (overlay/destroyed overlay key :parts) #{})]
    (reduce
     (fn [acc {:keys [x y z yaw pitch idx] :as p}]
       (if (contains? gone idx)
         acc
         (let [^js q (orient! scratch yaw pitch)
               ^js body (.createRigidBody
                         world
                         (-> (.fixed RAPIER/RigidBodyDesc)
                             (.setTranslation x y z)
                             (.setRotation #js {:x (.-x q) :y (.-y q) :z (.-z q) :w (.-w q)})
                             (.setLinearDamping 0.15)
                             (.setAngularDamping 0.5)))
               ^js collider (.createCollider
                             world
                             (-> (shape p)
                                 (.setDensity 90.0)
                                 (.setActiveEvents (.-CONTACT_FORCE_EVENTS RAPIER/ActiveEvents))
                                 (.setContactForceEventThreshold 1500.0))
                             body)]
           (conj acc {:body body :collider collider :handle (.-handle collider)
                      :idx idx :prim (:prim p) :inst (get prim-index idx)
                      :scale [(:sx p) (:sy p) (:sz p)]}))))
     []
     (filter #(= 2 (:solid %)) parts))))

(defn- hide-instance!
  "Push an instance out of the world by collapsing it to nothing.

  Used for panels that were already knocked out before this chunk loaded.
  Removing it from the InstancedMesh instead would renumber everything after
  it, and the overlay records indices."
  [bs key idx]
  (let [{:keys [chunks ^js scratch]} @bs
        {:keys [meshes breakable]} (get chunks key)]
    (when-let [{:keys [prim inst]} (first (filter #(= idx (:idx %)) breakable))]
      (when-let [^js m (get meshes prim)]
        (.set (.-scale scratch) 0 0 0)
        (.updateMatrix scratch)
        (.setMatrixAt m inst (.-matrix scratch))
        (set! (.-needsUpdate (.-instanceMatrix m)) true)))))

(defn add-chunk! [bs key arr]
  (when (and arr (pos? (.-length arr)))
    (let [parts  (read-parts arr)
          groups (group-by :prim parts)
          ;; Where each part ended up inside its prim's InstancedMesh, so a
          ;; panel that comes loose can have its own matrix written each frame.
          prim-index (into {} (for [[_ group] groups
                                    [i p] (map-indexed vector group)]
                                [(:idx p) i]))
          meshes (into {} (for [[prim group] groups :when (seq group)]
                            [prim (build-group! bs prim group)]))
          breakable (add-breakable! bs key parts prim-index)]
      (swap! bs assoc-in [:chunks key]
             {:meshes meshes
              :colliders (add-fixed! bs parts)
              :breakable breakable
              :solid (count (filter #(pos? (:solid %)) parts))})
      (swap! bs update :by-collider into
             (map (fn [{:keys [handle idx]}] [handle {:key key :idx idx}]) breakable))
      ;; Anything already knocked out has no body; take its picture away too.
      (when-let [ov (:overlay @bs)]
        (doseq [idx (overlay/destroyed ov key :parts)]
          (hide-instance! bs key idx)))
      meshes)))

(defn breakable?
  "Whether that collider handle is a panel something can knock out."
  [bs handle]
  (contains? (:by-collider @bs) handle))

(defn- promote!
  "Turn a fixed panel loose, and send it the way the car was going."
  [{:keys [^js body]} [vx vy vz]]
  (.setBodyType body (.-Dynamic RAPIER/RigidBodyType) true)
  (.applyImpulse body #js {:x (* smash-impulse vx)
                           :y (+ 60.0 (* smash-impulse (js/Math.abs vy)))
                           :z (* smash-impulse vz)}
                 true))

(defn smash!
  "Knock the panel behind `handle` loose. Returns the delta to record and
  broadcast, or nil if it was not a panel or has already gone."
  [bs handle vel]
  (when-let [{:keys [key idx]} (get (:by-collider @bs) handle)]
    (let [{:keys [breakable]} (get (:chunks @bs) key)
          part (first (filter #(= idx (:idx %)) breakable))]
      (when (and part (not (:loose? part)))
        (promote! part vel)
        (when-let [ov (:overlay @bs)] (overlay/record! ov key :parts idx))
        (swap! bs (fn [s]
                    (-> s
                        (update-in [:chunks key :breakable]
                                   (fn [ps] (mapv #(if (= idx (:idx %)) (assoc % :loose? true) %) ps)))
                        (update :loose conj (assoc part :key key))
                        (update :smashed inc))))
        (let [[cx cz] key] {:cx cx :cz cz :index idx})))))

(defn smash-index!
  "The same, but by index -- this is how someone else's smash arrives."
  [bs key idx vel]
  (when-let [{:keys [breakable]} (get (:chunks @bs) key)]
    (when-let [part (first (filter #(and (= idx (:idx %)) (not (:loose? %))) breakable))]
      (smash! bs (:handle part) vel))))

(defn sync!
  "Follow the panels that have come loose.

  Only those: everything else is fixed, and rewriting a thousand instance
  matrices a frame to move four of them is how an instanced renderer stops
  being worth having."
  [bs]
  (let [{:keys [chunks ^js scratch loose]} @bs]
    (when (seq loose)
      (doseq [{:keys [^js body key prim inst scale]} loose]
        (when-let [^js m (get-in chunks [key :meshes prim])]
          (let [t (.translation body) r (.rotation body)
                [sx sy sz] scale]
            (.set (.-position scratch) (.-x t) (.-y t) (.-z t))
            (.set (.-quaternion scratch) (.-x r) (.-y r) (.-z r) (.-w r))
            (.set (.-scale scratch) sx sy sz)
            (.updateMatrix scratch)
            (.setMatrixAt m inst (.-matrix scratch))
            (set! (.-needsUpdate (.-instanceMatrix m)) true)))))))

(defn remove-chunk! [bs key]
  (let [{:keys [^js world ^js scene chunks]} @bs]
    (when-let [{:keys [meshes colliders breakable]} (get chunks key)]
      (doseq [[_ ^js m] meshes]
        (.remove scene m)
        (.dispose m))
      (doseq [^js c colliders] (.removeCollider world c true))
      (doseq [{:keys [^js body]} breakable] (.removeRigidBody world body))
      (swap! bs update :by-collider #(apply dissoc % (map :handle breakable)))
      (swap! bs update :loose (fn [l] (vec (remove #(= key (:key %)) l)))))
    (swap! bs update :chunks dissoc key)))

(defn stats [bs]
  {:parts (reduce + (for [{:keys [meshes]} (vals (:chunks @bs))
                          [_ ^js m] meshes]
                      (.-count m)))
   :solid (reduce + (map :solid (vals (:chunks @bs))))
   :smashed (:smashed @bs)})
