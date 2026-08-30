(ns carmageddon.client.furniture
  "Traffic lights, signs, lamp posts and road markings.

  The generator emits *parts* rather than objects -- a traffic light is a pole
  instance plus a head instance -- and this draws one `InstancedMesh` per part
  per chunk. A city chunk carries about 150 pieces, so a mesh apiece would be
  thousands of draw calls for what is, visually, five shapes repeated. Instancing
  makes it five draws per chunk.

  Signal heads take a colour per instance, which is the other reason the parts
  are split: a pole and a lamp head never change, and a light that turns green
  must not drag every pole in the city with it.

  What a signal is showing is not stored anywhere. It is `worldgen/signal-state`
  of the world clock, so every client in a session sees the same lights with no
  traffic between them, and the server can answer for a junction it has never
  simulated."
  (:require ["@dimforge/rapier3d-compat" :as RAPIER]
            ["three" :as three]
            [carmageddon.shared.worldgen :as worldgen]))

;; Refreshed a few times a second, not per frame: an amber phase lasts 2.5 s and
;; nobody can see the difference, while rewriting an instance colour buffer at
;; display rate for every loaded chunk is real work.
(def ^:private signal-refresh-ms 250.0)

(def ^:private signal-colours
  {:green 0x27c24a :amber 0xe2a41c :red 0xd42a24})

(def ^:private sign-colours
  ;; :stop then :give-way, matching worldgen/sign-types.
  [0xc0271f 0xe8e4d8])

(defn- geometries
  "One shape per part. The pole is translated so its origin is at the foot,
  which lets a single unit-tall box become a 2.2 m sign post or a 6.4 m lamp
  column by scaling Y alone."
  []
  {:pole        (doto (three/BoxGeometry. 0.15 1.0 0.15) (.translate 0 0.5 0))
   :lamp-head   (three/BoxGeometry. 1.7 0.22 0.42)
   :signal-head (three/BoxGeometry. 0.42 1.15 0.34)
   :sign-face   (three/BoxGeometry. 0.92 0.92 0.07)
   ;; A crossing stripe runs *along* the direction of travel and the stripes are
   ;; spread across the carriageway, so the long axis is local Z.
   :marking     (three/BoxGeometry. 0.55 0.05 2.4)})

(defn- materials []
  {:pole        (three/MeshPhongMaterial. #js {:color 0x4c4c52 :shininess 10})
   :lamp-head   (three/MeshPhongMaterial. #js {:color 0x9ba1a8 :shininess 20})
   :signal-head (three/MeshPhongMaterial. #js {:color 0xffffff :shininess 30})
   :sign-face   (three/MeshPhongMaterial. #js {:color 0xffffff :shininess 12})
   :marking     (three/MeshPhongMaterial. #js {:color 0xd8d6c8 :shininess 2})})

(defn create [world scene]
  (atom {:world world :scene scene
         :geometries (geometries)
         :materials  (materials)
         :chunks {}                ; [cx cz] -> {:meshes {part mesh} :colliders [..] :signals [..]}
         :scratch (three/Object3D.)
         :colour  (three/Color.)
         :last-signal-ms 0.0}))

(defn- by-part
  "Group a chunk's flat furniture array into per-part vectors of instances.

  Kept as plain maps: this runs once per chunk load, not per frame, and the
  arithmetic that matters already happened in the generator."
  [arr]
  (let [st worldgen/furniture-stride
        n  (/ (.-length arr) st)]
    (reduce (fn [acc i]
              (let [o (* i st)
                    part (nth worldgen/furniture-parts (int (aget arr (+ o 4))))]
                (update acc part (fnil conj [])
                        {:x (aget arr (+ o 0)) :y (aget arr (+ o 1)) :z (aget arr (+ o 2))
                         :yaw (aget arr (+ o 3))
                         :size (aget arr (+ o 5))
                         :phase (int (aget arr (+ o 6)))
                         :offset (aget arr (+ o 7))})))
            {} (range n))))

(defn- build-instanced!
  [fs part instances]
  (let [{:keys [^js scene geometries materials ^js scratch ^js colour]} @fs
        ^js m (three/InstancedMesh. (get geometries part) (get materials part)
                                    (count instances))
        pole? (= :pole part)]
    ;; The instances of one chunk span 256 m, which the geometry's own bounding
    ;; sphere knows nothing about -- leaving culling on makes furniture wink out
    ;; when the chunk's origin leaves the frustum.
    (set! (.-frustumCulled m) false)
    ;; Road markings are painted on the carriageway and have no thickness to
    ;; cast with; everything else stands up.
    (set! (.-castShadow m) (not= :marking part))
    (set! (.-receiveShadow m) true)
    (doseq [[i {:keys [x y z yaw size phase]}] (map-indexed vector instances)]
      (.set (.-position scratch) x y z)
      (.set (.-rotation scratch) 0 yaw 0)
      (.set (.-scale scratch) 1 (if pole? size 1) 1)
      (.updateMatrix scratch)
      (.setMatrixAt m i (.-matrix scratch))
      (case part
        :signal-head (do (.setHex colour (:green signal-colours))
                         (.setColorAt m i colour))
        :sign-face   (do (.setHex colour (nth sign-colours (min phase (dec (count sign-colours)))))
                         (.setColorAt m i colour))
        nil))
    (set! (.-needsUpdate (.-instanceMatrix m)) true)
    (when-let [ic (.-instanceColor m)] (set! (.-needsUpdate ic) true))
    (.add scene m)
    m))

(defn- add-colliders!
  "Only the poles are solid. Lamp heads hang over the carriageway at 6 m, signs
  and markings are surfaces, and giving any of them a collider would put an
  invisible obstacle above the road."
  [fs poles]
  (let [{:keys [^js world]} @fs]
    (mapv (fn [{:keys [x y z size]}]
            (.createCollider
             world
             (-> (.cuboid RAPIER/ColliderDesc 0.09 (* 0.5 size) 0.09)
                 (.setTranslation x (+ y (* 0.5 size)) z)
                 (.setFriction 0.6)
                 (.setRestitution 0.1))))
          poles)))

(defn add-chunk! [fs key arr]
  (when (and arr (pos? (.-length arr)))
    (let [groups (by-part arr)
          meshes (into {} (for [[part instances] groups :when (seq instances)]
                            [part (build-instanced! fs part instances)]))
          colliders (add-colliders! fs (get groups :pole []))]
      (swap! fs assoc-in [:chunks key]
             {:meshes meshes
              :colliders colliders
              :signals (vec (get groups :signal-head []))})
      meshes)))

(defn remove-chunk!
  "Instanced meshes own their per-instance matrix and colour buffers, so unlike
  the shared per-kind geometry these really do have to be disposed."
  [fs key]
  (let [{:keys [^js world ^js scene chunks]} @fs]
    (when-let [{:keys [meshes colliders]} (get chunks key)]
      (doseq [[_ ^js m] meshes]
        (.remove scene m)
        (.dispose m))
      (doseq [^js c colliders] (.removeCollider world c true)))
    (swap! fs update :chunks dissoc key)))

(defn sync-signals!
  "Recolour every loaded signal head for the current world time.

  `now-ms` is wall clock on purpose. The lights are a function of it and of the
  junction's own offset, so two players in the same city see the same phase
  without anyone sending anything."
  [fs now-ms]
  (when (>= (- now-ms (:last-signal-ms @fs)) signal-refresh-ms)
    (let [{:keys [chunks ^js colour]} @fs
          t (/ now-ms 1000.0)]
      (doseq [[_ {:keys [meshes signals]}] chunks
              :let [^js m (get meshes :signal-head)]
              :when m]
        (doseq [[i {:keys [phase offset]}] (map-indexed vector signals)]
          (.setHex colour (get signal-colours (worldgen/signal-state t offset phase)))
          (.setColorAt m i colour))
        (when-let [ic (.-instanceColor m)] (set! (.-needsUpdate ic) true)))
      (swap! fs assoc :last-signal-ms now-ms))))

(defn stats [fs]
  {:pieces (reduce + (map (fn [{:keys [meshes]}]
                            (reduce + (map (fn [[_ ^js m]] (.-count m)) meshes)))
                          (vals (:chunks @fs))))
   :signals (reduce + (map (comp count :signals) (vals (:chunks @fs))))})
