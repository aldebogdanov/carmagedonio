(ns carmageddon.client.peds
  "Pedestrians: the things that make this game the game it is.

  Alive, a pedestrian is a capsule with its rotations locked, walking on a
  slowly curving heading. Locked rotations are what keep it upright without any
  balance model at all -- it cannot fall over because it cannot rotate.

  Dead, the lock comes off and it becomes ordinary physics debris. That single
  switch is the whole 'ragdoll': no joints, no skeleton, and it reads far better
  than a capsule sliding along still standing up.

  The capsule is still the physics. What is *drawn* over it is a small rig of
  boxes and spheres -- head, torso, hips, limbs -- animated by leaning the limbs
  about their pivots in time with how fast the figure is moving. Two things came
  out of that. The obvious one is that a person now walks instead of sliding
  along upright like a skittle. The less obvious one is that it is cheaper: the
  capsules were a `three/Mesh` each, and sharing geometry does not share a draw
  call, so two hundred pedestrians were two hundred draw calls. Every part of
  every figure now comes out of one of two instanced pools, and the whole crowd
  is two.

  Kills are recorded as a sparse per-chunk delta, exactly like smashed props, so
  driving away and coming back does not resurrect anyone."
  (:require ["@dimforge/rapier3d-compat" :as RAPIER]
            ["three" :as three]
            [carmageddon.client.figures :as fig]
            [carmageddon.client.fire :as fire]
            [carmageddon.client.overlay :as overlay]
            [carmageddon.shared.worldgen :as worldgen]))

(def ^:private walk-every 3)     ; ticks between walk updates; velocity persists

(def ^:private notice 20.0)      ; how far away a car is worth reacting to
;; Past this, a pedestrian is left alone entirely and Rapier puts it to sleep.
;; A sleeping body costs the solver almost nothing, and a person standing still
;; a hundred metres away is a person standing still. This is what makes a crowd
;; of four hundred affordable: with everyone walking, the physics step was 3.4 ms
;; of a 16 ms frame.
(def ^:private active 95.0)
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
   {:name :dog    :half 0.26 :radius 0.17 :colour 0x59463a :prone? true}
   ;; People who are somewhere for a reason. Same capsule, same rig, same way
   ;; of dying -- what differs is the wardrobe, which at the distance a
   ;; pedestrian is looked at is the entire read.
   {:name :suit         :half 0.42 :radius 0.26 :colour 0xd8b48c :prone? false}
   {:name :shopper      :half 0.42 :radius 0.26 :colour 0xd8b48c :prone? false}
   {:name :fan          :half 0.42 :radius 0.26 :colour 0xd8b48c :prone? false}
   {:name :drinker      :half 0.42 :radius 0.26 :colour 0xd8b48c :prone? false}
   {:name :streetwalker :half 0.42 :radius 0.26 :colour 0xd8b48c :prone? false}])

(def ^:private dead-colour 0x8c3a34)

;; Room for every figure in the streaming radius with its limbs, and a little
;; over. A full pool draws nothing extra rather than failing.
(def ^:private box-slots 8000)
(def ^:private sphere-slots 1600)

;; How fast the limbs swing per metre travelled. Two steps a metre reads as a
;; walk; much more and everyone is scurrying.
(def ^:private cadence 2.6)

;; --- rigs -------------------------------------------------------------------
;;
;; A rig is a list of parts in the figure's own frame, whose origin is the
;; centre of its physics capsule. A part is a box or a sphere; a *limb* also
;; carries a pivot, a length it hangs below that pivot, and a swing.
;;
;; This is the whole of the animation. There is no skeleton and no skinning: a
;; leg is a box that leans about the hip, which at the distance a pedestrian is
;; ever looked at is indistinguishable from one that bends -- and unlike a
;; skinned mesh it costs two matrix multiplies and no draw call of its own.

(def ^:private pi js/Math.PI)

(defn- limb [shape at size len swing phase tint]
  {:shape shape :at at :size size :drop (* -0.5 len) :swing swing
   :phase phase :tint tint})

(defn- rigid [shape at size tint]
  {:shape shape :at at :size size :drop 0.0 :swing 0.0 :phase 0.0 :tint tint})

(defn- person-rig
  "Head, torso, hips, two legs and two arms. Arms swing against the legs, which
  is the single detail that makes a walk read as a walk rather than a shuffle.

  `legs` is the surface the legs are painted, and it is the one place a person
  varies in shape rather than only in colour: bare legs read as a different
  silhouette from trousered ones at a distance where nothing else about a
  figure is legible."
  [legs]
  [(rigid :sphere [0.0 0.50 0.0] [0.27 0.29 0.27] :skin)
   (rigid :box [0.0 0.16 0.0] [0.36 0.50 0.24] :cloth)
   (rigid :box [0.0 -0.14 0.0] [0.34 0.18 0.24] :trouser)
   (limb :box [-0.10 -0.16 0.0] [0.13 0.50 0.15] 0.50 0.55 0.0 legs)
   (limb :box [0.10 -0.16 0.0] [0.13 0.50 0.15] 0.50 0.55 pi legs)
   (limb :box [-0.22 0.33 0.0] [0.11 0.42 0.12] 0.42 0.45 pi :cloth)
   (limb :box [0.22 0.33 0.0] [0.11 0.42 0.12] 0.42 0.45 0.0 :cloth)])

(defn- quadruped-rig
  "Body, head, tail and four legs, sized from the capsule the animal already
  has. Diagonal pairs move together, which is what a walking animal does and
  what stops four legs looking like two."
  [half radius]
  (let [r radius
        len (* 2.0 (+ half r))
        body-h (* 1.55 r)
        drop (* 0.5 body-h)               ; underside of the body
        leg-len (max 0.2 (- (+ half r) drop))
        lx (* 0.62 r)
        lz (* 0.52 half)
        nose (* -0.46 len)]
    [(rigid :sphere [0.0 0.0 0.0] [(* 1.8 r) body-h (* 0.86 len)] :hide)
     (rigid :sphere [0.0 (* 0.42 r) nose] [(* 1.15 r) (* 1.1 r) (* 1.25 r)] :head)
     (rigid :box [0.0 (* 0.45 r) (* 0.46 len)] [(* 0.22 r) (* 0.22 r) (* 0.5 r)] :head)
     (limb :box [(- lx) (- drop) (- lz)] [(* 0.4 r) leg-len (* 0.4 r)] leg-len 0.5 0.0 :head)
     (limb :box [lx (- drop) (- lz)] [(* 0.4 r) leg-len (* 0.4 r)] leg-len 0.5 pi :head)
     (limb :box [(- lx) (- drop) lz] [(* 0.4 r) leg-len (* 0.4 r)] leg-len 0.5 pi :head)
     (limb :box [lx (- drop) lz] [(* 0.4 r) leg-len (* 0.4 r)] leg-len 0.5 0.0 :head)]))

(defn- rig-for [{:keys [half radius prone? name]}]
  (if prone?
    (quadruped-rig half radius)
    (person-rig (if (= :streetwalker name) :skin :trouser))))

(defn- shade
  "Darken a colour, for the parts of a figure that are not its main surface."
  [hex f]
  (let [r (bit-and (bit-shift-right hex 16) 255)
        g (bit-and (bit-shift-right hex 8) 255)
        b (bit-and hex 255)
        m (fn [v] (bit-and (js/Math.round (* v f)) 255))]
    (bit-or (bit-shift-left (m r) 16) (bit-shift-left (m g) 8) (m b))))

(def ^:private skins
  [0xe8c39a 0xc79a6b 0x8d5f3d 0x5d3b28 0xf0d3b0 0xa87551])

(def ^:private wardrobes
  "Six changes of clothes per sort of person.

  A group of businessmen is not a group of people who happen to be standing
  outside a bank -- it is six dark suits, and that is the whole of the read at
  fifty metres. Every one of these is six entries so a group is varied without
  ever being mistakable for a different group."
  {:person       {:cloth   [0x3f5f8a 0x8a3f3f 0x2f6b52 0x8a7a3f 0x5b3f8a 0x39424d]
                  :trouser [0x2f3540 0x4a3f35 0x35404a 0x3f3a2f 0x2b2f36 0x453a3a]}
   ;; Charcoal, navy, and more charcoal.
   :suit         {:cloth   [0x2b3040 0x33384a 0x232838 0x3b3f52 0x262b3a 0x1e2230]
                  :trouser [0x232838 0x2b3040 0x1e2230 0x33384a 0x252a38 0x2b3040]}
   ;; Whatever was in the window. Bags are not modelled; the colours do it.
   :shopper      {:cloth   [0xd8734a 0xd9b23f 0x4aa3c8 0xc9527f 0x62b05a 0xdd8f3c]
                  :trouser [0x3a4250 0x5a4638 0x394a3f 0x4a3f52 0x3a4250 0x504438]}
   ;; A team, not a wardrobe: see `palette`, which picks these by chunk rather
   ;; than by person so a crowd outside the ground supports one side.
   :fan          {:cloth   [0xc0392b 0x2f6bb5 0x1f7a4a 0xe0a52b 0x7d3fa8 0xdedede]
                  :trouser [0x2b2f36 0x2b2f36 0x2b2f36 0x2b2f36 0x2b2f36 0x2b2f36]}
   :drinker      {:cloth   [0x8a6b4a 0x5c6b52 0x7a4a4a 0x4a5c6b 0x6b5c4a 0x555a52]
                  :trouser [0x3a352f 0x35392f 0x2f3538 0x3f3a35 0x35302b 0x38352f]}
   :streetwalker {:cloth   [0xd9407a 0xe05c2b 0xb03fc0 0xd93f4a 0xe0a02b 0x3fc0b0]
                  ;; Bare legs -- `rig-for` paints this kind's legs `:skin` --
                  ;; so the trouser entry only reaches the hips.
                  :trouser [0x2b2f36 0x40252f 0x2b2f36 0x35202b 0x2b2f36 0x3a2530]}})

(defn- palette
  "What each named surface of one kind is coloured. People get skin, clothes and
  trousers out of one seed colour so a crowd is not uniform; animals get their
  hide and a darker head and legs.

  Football supporters are the exception and it is deliberate: their shirt is
  drawn from the *chunk* rather than from the person, so everyone standing
  outside the ground is wearing the same one. Nothing has to be passed between
  them for that to work -- they all read the same key."
  [{:keys [name colour prone?]} idx key outbreak?]
  (let [mix (+ idx (* 31 (first key)) (* 17 (second key)))
        j   (mod (* mix 2654435761) 6)
        team (mod (+ (* 31 (first key)) (* 17 (second key))) 6)]
    (if prone?
      {:hide colour :head (shade colour 0.78)}
      (let [w (get wardrobes name (get wardrobes :person))
            pick (if (= :fan name) team j)]
        {:skin (if outbreak? 0x7d9a58 (nth skins j))
         :cloth (if outbreak? 0x53663a (nth (:cloth w) pick))
         :trouser (if outbreak? 0x3d4a2c (nth (:trouser w) pick))}))))

(defn create
  "`mode` is :normal or :outbreak. Outbreak does not change what is spawned --
  the same people are standing in the same places -- only what they do when a
  car comes near, and what colour they are while doing it."
  ([world scene ov] (create world scene ov :normal))
  ([world scene ov mode]
   (let [material (three/MeshPhongMaterial. #js {:color 0xffffff :flatShading true
                                                 :shininess 3})
         pools {:box (fig/pool scene (three/BoxGeometry. 1 1 1) material
                               box-slots {})
                :sphere (fig/pool scene (three/SphereGeometry. 0.5 7 5) material
                                  sphere-slots {})}
         rigs (mapv (comp fig/rig rig-for) kinds)]
     (atom {:world world :scene scene
            :overlay ov
            :mode mode
            :outbreak? (= :outbreak mode)
            ;; Two pools between them draw every person and every animal.
            :pools pools
            ;; One flattened rig per kind, and the pool mesh each of its parts
            ;; belongs to, resolved here rather than per part per frame.
            :rigs rigs
            :rig-meshes (mapv (fn [r]
                                (into-array (map #(:mesh (get pools (:shape %)))
                                                 (:parts r))))
                              rigs)
            ;; Scratch, allocated once.
            :body-m (three/Matrix4.)
            :local-m (three/Matrix4.)
            :out-m (three/Matrix4.)
            :qpos (three/Vector3.)
            :quat (three/Quaternion.)
            :one (three/Vector3. 1 1 1)
            :chunks {}         ; [cx cz] -> [ped ...]
            :by-collider {}    ; handle -> {:key :idx}
            :killed 0}))))

(defn killed-in [ps key] (overlay/destroyed (:overlay @ps) key :peds))

(defn- spawn-one! [ps key idx x y z heading speed kind]
  (let [{:keys [^js world pools rigs outbreak?]} @ps
        {:keys [half radius] :as spec} (nth kinds kind)
        rig (nth rigs kind)
        parts (:parts rig)
        pal (palette spec idx key outbreak?)
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
        ;; One slot per part, claimed once. Colours are written here and not
        ;; touched again until the figure dies, because a crowd's paintwork does
        ;; not change sixty times a second.
        slots (js/Int32Array. (count parts))]
    (dotimes [i (count parts)]
      (let [{:keys [shape tint]} (nth parts i)
            pool (get pools shape)
            slot (fig/claim! pool)]
        (aset slots i slot)
        (fig/set-colour! pool slot (get pal tint 0xffffff))))
    ;; Upright without a balance model: it cannot topple because it cannot turn.
    (.lockRotations body true true)
    {:body body :collider collider :handle (.-handle collider)
     :idx idx :heading heading :speed speed :kind kind
     :rig rig :meshes (nth (:rig-meshes @ps) kind) :slots slots
     ;; The walk cycle's own clock, advanced by distance travelled. A typed
     ;; array so it can be written without rebuilding the chunk's vector, which
     ;; is what happens to every other piece of per-pedestrian state here.
     :phase (js/Float32Array. 1)
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

(defn- despawn! [ps {:keys [^js body handle rig ^js slots]}]
  (let [{:keys [^js world pools]} @ps
        parts (:parts rig)]
    (dotimes [i (count parts)]
      (fig/release! (get pools (:shape (nth parts i))) (aget slots i)))
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
              d  (js/Math.hypot dx dz)]
         (when (< d active)
          (let [person? (zero? (:kind p))
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
                                  :z (* sp (js/Math.sin h))} true))))))))

(defn kill-index!
  "Kill pedestrian `idx` of chunk `key`, recording the delta.

  Used by local impacts and by kills another player reported. The delta is
  recorded even when that chunk is not loaded here, so it stays dead when the
  chunk arrives."
  [ps key idx impulse]
  (overlay/record! (:overlay @ps) key :peds idx)
  (let [p (first (filter #(= idx (:idx %)) (get (:chunks @ps) key)))]
    (when (and p (:alive? p))
      (let [^js body (:body p)
            {:keys [pools]} @ps
            ^js slots (:slots p)]
        (.lockRotations body false true)
        ;; Repainted rather than re-materialised: every figure shares one
        ;; material now, and the colour is per instance.
        (dotimes [i (count (:parts (:rig p)))]
          (fig/set-colour! (get pools (:shape (nth (:parts (:rig p)) i))) (aget slots i)
                           dead-colour))
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

(def ^:private burn-every 6)     ; ticks between checking the crowd for fire

(defn burn!
  "Kill anyone standing in fire.

  Checked ten times a second rather than sixty. Four hundred pedestrians
  against twenty pools is eight thousand distance tests, and nobody can tell
  the difference between catching fire now and catching fire a tenth of a
  second from now.

  Returns `[[delta owner] ...]` -- the caller scores the ones whose fire was
  the player's. Without that attribution the strongest play in the game would
  be to set an industrial estate alight and drive away from it."
  [ps fs tick]
  (when (and (zero? (mod tick burn-every))
             (pos? (:pools (fire/stats fs))))
    (let [victims (for [[key chunk] (:chunks @ps)
                        p chunk
                        :when (:alive? p)
                        :let [t (.translation ^js (:body p))
                              h (fire/heat-at fs (.-x t) (.-z t))]
                        :when h]
                    [key (:idx p) (:owner h)])]
      (mapv (fn [[key idx owner]]
              [(kill-index! ps key idx [0.0 6.0 0.0]) owner])
            (vec victims)))))

(defn kill-near!
  "Kill everyone within `r` of (x, z). Returns their deltas, each carrying the
  position it happened at.

  `:pos` is not part of the wire delta -- the encoder ignores what it does not
  know -- but a weapon that wants to draw itself needs to know where it landed,
  and the radius test has already read every one of these translations."
  [ps x z r]
  (let [r2 (* r r)
        victims (for [[key chunk] (:chunks @ps)
                      p chunk
                      :when (:alive? p)
                      :let [t (.translation ^js (:body p))
                            dx (- (.-x t) x) dz (- (.-z t) z)]
                      :when (< (+ (* dx dx) (* dz dz)) r2)]
                  [key (:idx p) [(.-x t) (.-y t) (.-z t)]])]
    (mapv (fn [[key idx pos]]
            (assoc (kill-index! ps key idx [0.0 5.0 0.0]) :pos pos))
          (vec victims))))

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

(defn sync!
  "Place every part of every figure, and advance the walk cycles by `dt`.

  The whole crowd is written into two instance buffers and flushed once. A dead
  figure keeps its pose and simply tumbles with the body it is drawn on, which
  is the same trick the capsule pulled and reads better with limbs on it."
  [ps dt]
  (let [{:keys [chunks pools ^js body-m ^js local-m ^js out-m
                ^js qpos ^js quat ^js one]} @ps]
    (doseq [[_ chunk] chunks
            {:keys [^js body rig ^js meshes ^js slots ^js phase alive?]} chunk]
      (let [^js lv (.linvel body)
            sp (js/Math.hypot (.-x lv) (.-z lv))]
        ;; Distance, not time: someone standing still stands still, and someone
        ;; running moves their legs faster without anybody storing a state.
        (when alive?
          (aset phase 0 (+ (aget phase 0) (* cadence sp dt))))
        (fig/body-matrix! body-m qpos quat one body)
        (fig/place-rig! rig meshes slots body-m local-m out-m (aget phase 0))))
    (doseq [[_ p] pools] (fig/flush! p))))

(defn stats [ps]
  (let [all (mapcat val (:chunks @ps))
        live (filter :alive? all)]
    {:alive (count live)
     :people (count (filter #(zero? (:kind %)) live))
     :animals (count (remove #(zero? (:kind %)) live))
     :mode (:mode @ps)
     :bodies (count all)
     :killed (:killed @ps)}))
