(ns carmageddon.client.props
  "Smashable roadside clutter: the things the game is actually about hitting.

  Props live and die with their chunk's physics payload, so they exist exactly
  where the player can reach them. Geometry and materials are shared per kind --
  there are a couple of hundred of these at any moment, and giving each its own
  would leak GPU resources the same way per-chunk materials did.

  Destruction is recorded in the shared overlay rather than by mutating the
  chunk. That keeps generation pure: a chunk is still a function of its seed,
  with a small record of what has happened to it. It is also exactly the payload
  multiplayer syncs, and exactly what a save file contains -- which is why there
  is one overlay for the whole world rather than a delta map per subsystem."
  (:require ["@dimforge/rapier3d-compat" :as RAPIER]
            ["three" :as three]
            [carmageddon.client.overlay :as overlay]
            [carmageddon.shared.worldgen :as worldgen]))

(defn- kind-assets []
  (mapv (fn [{:keys [half colour]}]
          (let [[hx hy hz] half]
            {:geometry (three/BoxGeometry. (* 2 hx) (* 2 hy) (* 2 hz))
             :material (three/MeshPhongMaterial. #js {:color colour :flatShading true
                                                      :shininess 4})}))
        worldgen/prop-kinds))

(defn create [world scene ov]
  (atom {:world world :scene scene
         :overlay ov
         :assets (kind-assets)
         :chunks {}        ; [cx cz] -> [{:body :mesh :collider-handle :idx} ...]
         :by-collider {}   ; collider handle -> {:key :idx}
         :wrecked 0}))

(defn destroyed [ps key] (overlay/destroyed (:overlay @ps) key :props))

(defn- spawn-one!
  [ps key idx x y z yaw kind scale]
  (let [{:keys [^js world ^js scene assets]} @ps
        {:keys [half density]} (nth worldgen/prop-kinds kind)
        [hx hy hz] (mapv #(* % scale) half)
        {:keys [geometry material]} (nth assets kind)
        ^js body (.createRigidBody
                  world
                  (-> (.dynamic RAPIER/RigidBodyDesc)
                      (.setTranslation x (+ y hy) z)
                      (.setRotation #js {:x 0.0 :y (js/Math.sin (/ yaw 2))
                                         :z 0.0 :w (js/Math.cos (/ yaw 2))})
                      (.setLinearDamping 0.2)
                      (.setAngularDamping 0.4)))
        ^js collider (.createCollider
                      world
                      (-> (.cuboid RAPIER/ColliderDesc hx hy hz)
                          (.setDensity density)
                          (.setFriction 0.7)
                          (.setRestitution 0.15)
                          (.setActiveEvents (.-CONTACT_FORCE_EVENTS RAPIER/ActiveEvents))
                          (.setContactForceEventThreshold 1000.0))
                      body)
        ^js mesh (three/Mesh. geometry material)]
    (.set (.-scale mesh) scale scale scale)
    (.set (.-position mesh) x (+ y hy) z)
    (.set (.-quaternion mesh) 0.0 (js/Math.sin (/ yaw 2)) 0.0 (js/Math.cos (/ yaw 2)))
    (.add scene mesh)
    {:body body :mesh mesh :collider collider :handle (.-handle collider) :idx idx}))

(defn sync!
  "Copy every prop's physics transform onto its mesh.

  Props are dynamic bodies -- the whole point is that they fly when hit -- so
  unlike static scenery they have to be followed every frame. Read directly
  rather than interpolated between ticks: props are secondary motion, and the
  extra pair of transform buffers is not worth it for something the player sees
  cartwheeling away."
  [ps]
  (doseq [[_ props] (:chunks @ps)
          {:keys [^js body ^js mesh]} props]
    (let [t (.translation body)
          r (.rotation body)]
      (.set (.-position mesh) (.-x t) (.-y t) (.-z t))
      (.set (.-quaternion mesh) (.-x r) (.-y r) (.-z r) (.-w r)))))

(defn add-chunk!
  "Spawn every prop in `arr` that has not already been destroyed."
  [ps key arr]
  (when (and arr (pos? (.-length arr)))
    (let [gone (destroyed ps key)
          n    (/ (.-length arr) worldgen/prop-stride)
          made (reduce
                (fn [acc idx]
                  (if (contains? gone idx)
                    acc
                    (let [o (* idx worldgen/prop-stride)]
                      (conj acc (spawn-one! ps key idx
                                            (aget arr (+ o 0)) (aget arr (+ o 1))
                                            (aget arr (+ o 2)) (aget arr (+ o 3))
                                            (int (aget arr (+ o 4))) (aget arr (+ o 5)))))))
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

(defn remove-chunk!
  "Drop a chunk's props. Geometry and materials are shared per kind, so nothing
  is disposed here -- only the per-prop bodies and scene nodes."
  [ps key]
  (doseq [p (get (:chunks @ps) key)] (despawn! ps p))
  (swap! ps update :chunks dissoc key))

(defn destroy-index!
  "Remove prop `idx` of chunk `key`, recording the delta. Used both by local
  impacts and by destruction another player reported -- a remote delta has to
  take effect here even if that chunk is not currently loaded, which is why the
  delta is recorded unconditionally."
  [ps key idx]
  (overlay/record! (:overlay @ps) key :props idx)
  (when-let [p (first (filter #(= idx (:idx %)) (get (:chunks @ps) key)))]
    (despawn! ps p)
    (swap! ps (fn [st]
                (-> st
                    (update-in [:chunks key] #(vec (remove (fn [q] (= idx (:idx q))) %)))
                    (update :wrecked inc)))))
  {:cx (first key) :cz (second key) :index idx})

(defn destroy!
  "Remove the prop owning `handle`. Returns the delta describing what was
  destroyed, or nil if the handle was not a prop.

  Whether a hit *counts* is a gameplay decision and lives in the caller, not
  here. Contact force alone is not the right signal: a prop settling on uneven
  ground, or resolving an overlap with its neighbour, routinely produces larger
  forces than a car clipping it, and gating on force alone destroyed a sixth of
  the world while the player was parked."
  [ps handle]
  (when-let [{:keys [key idx]} (get (:by-collider @ps) handle)]
    (when (first (filter #(= idx (:idx %)) (get (:chunks @ps) key)))
      (destroy-index! ps key idx))))

(defn prop? [ps handle] (contains? (:by-collider @ps) handle))

(defn nearest
  "World position of the closest live prop to (x, z), or nil. Used by tests and
  by the target-seeking AI in M4."
  [ps x z]
  (->> (mapcat val (:chunks @ps))
       (map (fn [{:keys [^js body]}]
              (let [t (.translation body)]
                [(.-x t) (.-y t) (.-z t)])))
       (reduce (fn [best [px py pz]]
                 (let [d (+ (* (- px x) (- px x)) (* (- pz z) (- pz z)))]
                   (if (or (nil? best) (< d (:d best)))
                     {:d d :pos [px py pz]}
                     best)))
               nil)
       :pos))

(defn stats [ps]
  {:live (reduce + (map count (vals (:chunks @ps))))
   :wrecked (:wrecked @ps)})
