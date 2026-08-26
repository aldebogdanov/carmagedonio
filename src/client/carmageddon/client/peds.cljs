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

(def ^:private walk-every 3)     ; ticks between walk updates; velocity persists

(def ^:private notice 20.0)      ; how far away a car is worth reacting to
(def ^:private panic 2.6)        ; multiplier on walking speed when it is
(def ^:private shamble 1.5)      ; zombies are quicker than a walk, slower than fear

(def kinds
  "One entry per `worldgen/ped-kinds`, in that order.

  Animals reuse the whole pedestrian machinery -- capsule, locked rotations,
  kill-into-debris, per-chunk delta -- because at runtime a cow is a pedestrian
  that happens to be shaped like a cow. The only things that differ are its
  size, its colour and whether it walks on a street or in a field."
  [{:name :person :half 0.42 :radius 0.26 :colour 0xd8b48c :prone? false}
   {:name :sheep  :half 0.34 :radius 0.24 :colour 0xe4e0d6 :prone? true}
   {:name :cow    :half 0.62 :radius 0.36 :colour 0x6d5443 :prone? true}
   {:name :deer   :half 0.52 :radius 0.28 :colour 0x8c6a44 :prone? true}
   {:name :dog    :half 0.26 :radius 0.17 :colour 0x59463a :prone? true}])

(def ^:private zombie-colour 0x6f8a4a)

(defn- kind-assets [outbreak?]
  (mapv (fn [{:keys [half radius colour prone? name]}]
          {:geometry (let [g (three/CapsuleGeometry. radius (* 2 half) 4 8)]
                       ;; Livestock are longer than they are tall.
                       (if prone? (doto g (.rotateX (/ js/Math.PI 2))) g))
           :material (three/MeshPhongMaterial.
                      #js {:color (if (and outbreak? (= :person name))
                                    zombie-colour colour)
                           :flatShading true :shininess 2})
           :half half :radius radius})
        kinds))

(defn create
  "`mode` is :normal or :outbreak. Outbreak does not change what is spawned --
  the same people are standing in the same places -- only what they do when a
  car comes near, and what colour they are while doing it."
  ([world scene] (create world scene :normal))
  ([world scene mode]
   (atom {:world world :scene scene
          :mode mode
          :outbreak? (= :outbreak mode)
          :assets (kind-assets (= :outbreak mode))
          :dead-material (three/MeshPhongMaterial. #js {:color 0x8c3a34 :flatShading true
                                                        :shininess 2})
          :chunks {}         ; [cx cz] -> [ped ...]
          :by-collider {}    ; handle -> {:key :idx}
          :deltas {}         ; [cx cz] -> #{killed index}
          :killed 0})))

(defn killed-in [ps key] (get (:deltas @ps) key #{}))

(defn- spawn-one! [ps key idx x y z heading speed kind]
  (let [{:keys [^js world ^js scene assets]} @ps
        {:keys [^js geometry ^js material half radius]} (nth assets kind)
        ^js body (.createRigidBody
                  world
                  (-> (.dynamic RAPIER/RigidBodyDesc)
                      (.setTranslation x (+ y half radius) z)
                      (.setLinearDamping 0.1)))
        ^js collider (.createCollider
                      world
                      (-> (.capsule RAPIER/ColliderDesc half radius)
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
     :idx idx :heading heading :speed speed :kind kind
     ;; A fixed per-pedestrian turn rate is enough to stop everyone walking in
     ;; parallel lines forever, and needs no extra randomness at runtime.
     :turn (* 0.20 (- (mod (* idx 0.61803) 1.0) 0.5))
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
                                                   (aget arr (+ o 4))
                                                   (int (aget arr (+ o 5))))))))
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

  A crowd that ignores the car bearing down on it is the single thing that most
  makes a street look like scenery, so anyone within `notice` metres reacts: in
  a normal world by running, in an outbreak by coming straight at it. Animals
  always run, having no opinion about the end of the world.

  The wandering heading is *derived* from the tick rather than accumulated, so
  this walks everyone without writing back any state. Storing it meant rebuilding
  the whole chunk map every few ticks, which allocated more than the physics did."
  [ps tick px pz]
  (when (zero? (mod tick walk-every))
    (let [{:keys [chunks outbreak?]} @ps
          t (* tick (/ 1.0 60.0))]
      (doseq [[_ chunk] chunks
              p chunk
              :when (:alive? p)]
        (let [^js body (:body p)
              tr (.translation body)
              dx (- (.-x tr) px)
              dz (- (.-z tr) pz)
              d  (js/Math.hypot dx dz)
              person? (zero? (:kind p))
              chase? (and outbreak? person?)
              [h sp] (if (< d notice)
                       ;; Toward or away, and quicker either way.
                       [(js/Math.atan2 (if chase? (- dz) dz)
                                       (if chase? (- dx) dx))
                        (* (:speed p) (if chase? shamble panic))]
                       [(+ (:heading p) (* (:turn p) t)) (:speed p)])
              v (.linvel body)]
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

(defn person? [ps handle]
  (when-let [{:keys [key idx]} (get (:by-collider @ps) handle)]
    (some (fn [p] (and (= idx (:idx p)) (zero? (:kind p))))
          (get (:chunks @ps) key))))

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
  (let [all (mapcat val (:chunks @ps))
        live (filter :alive? all)]
    {:alive (count live)
     :people (count (filter #(zero? (:kind %)) live))
     :animals (count (remove #(zero? (:kind %)) live))
     :mode (:mode @ps)
     :bodies (count all)
     :killed (:killed @ps)}))
