(ns carmageddon.client.peds
  "Pedestrians: the things that make this game the game it is.

  Alive, a pedestrian is a capsule with its rotations locked, walking on a
  slowly curving heading. Locked rotations are what keep it upright without any
  balance model at all -- it cannot fall over because it cannot rotate.

  Dead, the lock comes off and it becomes ordinary physics debris. That single
  switch is the whole 'ragdoll': no joints, no skeleton, and it reads far better
  than a capsule sliding along still standing up.

  Kills are recorded as a sparse per-chunk delta, exactly like smashed props, so
  driving away and coming back does not resurrect anyone."
  (:require ["@dimforge/rapier3d-compat" :as RAPIER]
            ["three" :as three]
            [carmageddon.shared.worldgen :as worldgen]))

(def ^:private half-height 0.42)
(def ^:private radius 0.26)
(def ^:private walk-every 3)     ; ticks between walk updates; velocity persists

(defn create [world scene]
  (atom {:world world :scene scene
         :geometry (doto (three/CapsuleGeometry. radius (* 2 half-height) 4 8))
         :material (three/MeshPhongMaterial. #js {:color 0xd8b48c :flatShading true
                                                  :shininess 2})
         :dead-material (three/MeshPhongMaterial. #js {:color 0x8c3a34 :flatShading true
                                                       :shininess 2})
         :chunks {}         ; [cx cz] -> [ped ...]
         :by-collider {}    ; handle -> {:key :idx}
         :deltas {}         ; [cx cz] -> #{killed index}
         :killed 0}))

(defn killed-in [ps key] (get (:deltas @ps) key #{}))

(defn- spawn-one! [ps key idx x y z heading speed]
  (let [{:keys [^js world ^js scene ^js geometry ^js material]} @ps
        ^js body (.createRigidBody
                  world
                  (-> (.dynamic RAPIER/RigidBodyDesc)
                      (.setTranslation x (+ y half-height radius) z)
                      (.setLinearDamping 0.1)))
        ^js collider (.createCollider
                      world
                      (-> (.capsule RAPIER/ColliderDesc half-height radius)
                          (.setDensity 90.0)
                          (.setFriction 0.6)
                          (.setActiveEvents (.-CONTACT_FORCE_EVENTS RAPIER/ActiveEvents))
                          (.setContactForceEventThreshold 600.0))
                      body)
        ^js mesh (three/Mesh. geometry material)]
    ;; Upright without a balance model: it cannot topple because it cannot turn.
    (.lockRotations body true true)
    (.add scene mesh)
    {:body body :mesh mesh :collider collider :handle (.-handle collider)
     :idx idx :heading heading :speed speed
     ;; A fixed per-pedestrian turn rate is enough to stop everyone walking in
     ;; parallel lines forever, and needs no extra randomness at runtime.
     :turn (* 0.35 (- (mod (* idx 0.61803) 1.0) 0.5))
     :alive? true}))

(defn add-chunk! [ps key arr]
  (when (and arr (pos? (.-length arr)))
    (let [gone (killed-in ps key)
          n    (/ (.-length arr) worldgen/ped-stride)
          made (reduce (fn [acc idx]
                         (if (contains? gone idx)
                           acc
                           (let [o (* idx worldgen/ped-stride)]
                             (conj acc (spawn-one! ps key idx
                                                   (aget arr (+ o 0)) (aget arr (+ o 1))
                                                   (aget arr (+ o 2)) (aget arr (+ o 3))
                                                   (aget arr (+ o 4)))))))
                       [] (range n))]
      (swap! ps (fn [st]
                  (-> st
                      (assoc-in [:chunks key] made)
                      (update :by-collider merge
                              (into {} (map (fn [p] [(:handle p) {:key key :idx (:idx p)}]) made))))))
      made)))

(defn- despawn! [ps {:keys [^js body ^js mesh handle]}]
  (let [{:keys [^js world ^js scene]} @ps]
    (.remove scene mesh)
    (.removeRigidBody world body)
    (swap! ps update :by-collider dissoc handle)))

(defn remove-chunk! [ps key]
  (doseq [p (get (:chunks @ps) key)] (despawn! ps p))
  (swap! ps update :chunks dissoc key))

(defn walk!
  "Drive the living. Called every `walk-every` ticks -- velocity persists between
  updates, so a lower rate is invisible and costs proportionally less with a few
  hundred pedestrians about.

  Heading is *derived* from the tick rather than accumulated, so this walks
  everyone without writing back any state. Storing it meant rebuilding the whole
  chunk map every few ticks, which allocated more than the physics did."
  [ps tick]
  (when (zero? (mod tick walk-every))
    (let [t (* tick (/ 1.0 60.0))]
      (doseq [[_ chunk] (:chunks @ps)
              p chunk
              :when (:alive? p)]
        (let [^js body (:body p)
              h  (+ (:heading p) (* (:turn p) t))
              sp (:speed p)
              v  (.linvel body)]
          (.setLinvel body #js {:x (* sp (js/Math.cos h))
                                :y (.-y v)
                                :z (* sp (js/Math.sin h))} true))))))

(defn kill-index!
  "Kill pedestrian `idx` of chunk `key`, recording the delta.

  Used by local impacts and by kills another player reported. The delta is
  recorded even when that chunk is not loaded here, so it stays dead when the
  chunk arrives."
  [ps key idx impulse]
  (swap! ps update-in [:deltas key] (fnil conj #{}) idx)
  (let [p (first (filter #(= idx (:idx %)) (get (:chunks @ps) key)))]
    (when (and p (:alive? p))
      (let [^js body (:body p)
            ^js mesh (:mesh p)]
        (.lockRotations body false true)
        (set! (.-material mesh) (:dead-material @ps))
        (.applyImpulse body
                       #js {:x (* 0.35 (nth impulse 0))
                            :y (+ 140.0 (js/Math.abs (* 0.2 (nth impulse 1))))
                            :z (* 0.35 (nth impulse 2))}
                       true))
      (swap! ps (fn [st]
                  (-> st
                      (update-in [:chunks key]
                                 (fn [v] (mapv #(if (= idx (:idx %)) (assoc % :alive? false) %) v)))
                      (update :killed inc))))))
  {:cx (first key) :cz (second key) :index idx})

(defn kill!
  "Turn a living pedestrian into debris. Returns the delta, or nil if `handle`
  was not a living pedestrian."
  [ps handle impulse]
  (when-let [{:keys [key idx]} (get (:by-collider @ps) handle)]
    (let [p (first (filter #(= idx (:idx %)) (get (:chunks @ps) key)))]
      (when (and p (:alive? p))
        (kill-index! ps key idx impulse)))))

(defn ped? [ps handle] (contains? (:by-collider @ps) handle))

(defn nearest-alive
  "World position of the closest living pedestrian to (x, z), or nil."
  [ps x z]
  (->> (mapcat val (:chunks @ps))
       (filter :alive?)
       (reduce (fn [best {:keys [^js body]}]
                 (let [t (.translation body)
                       dx (- (.-x t) x) dz (- (.-z t) z)
                       d (+ (* dx dx) (* dz dz))]
                   (if (or (nil? best) (< d (:d best)))
                     {:d d :pos [(.-x t) (.-y t) (.-z t)]}
                     best)))
               nil)
       :pos))

(defn sync! [ps]
  (doseq [[_ chunk] (:chunks @ps)
          {:keys [^js body ^js mesh]} chunk]
    (let [t (.translation body)
          r (.rotation body)]
      (.set (.-position mesh) (.-x t) (.-y t) (.-z t))
      (.set (.-quaternion mesh) (.-x r) (.-y r) (.-z r) (.-w r)))))

(defn stats [ps]
  (let [all (mapcat val (:chunks @ps))]
    {:alive (count (filter :alive? all))
     :bodies (count all)
     :killed (:killed @ps)}))
