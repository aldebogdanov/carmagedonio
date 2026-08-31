(ns carmageddon.client.remote
  "Other players' cars.

  A proxy is a *kinematic* rigid body, not a dynamic one. Its position comes
  from the network, so letting the solver move it would fight the interpolation
  -- but it still needs a physical presence, because the whole point of a
  Carmageddon-shaped game is ramming people. Kinematic gives exactly that: it
  pushes dynamic bodies and is never pushed back.

  This means each client resolves collisions with proxies locally, and two
  clients will disagree slightly about a heavy shunt. That was accepted in M0
  when the server stopped being the physics authority; the alternative is
  rolling everyone's simulation back, which is not on the table for a browser
  game with no shared determinism.

  A proxy is drawn as the vehicle it actually is. The car frame always had a
  spare byte and it now carries the catalogue index, so someone driving a lorry
  arrives in your mirror as a lorry rather than as the reference saloon that
  every remote used to be."
  (:require ["@dimforge/rapier3d-compat" :as RAPIER]
            ["three" :as three]
            [carmageddon.client.cars :as cars]
            [carmageddon.client.figures :as fig]
            [carmageddon.shared.interp :as interp]))

(def ^:private stale-ms 4000)

;; A handful of players, a dozen parts each. Sized small deliberately: a pool
;; is a fixed cost whether or not anyone is connected.
(def ^:private box-slots 220)
(def ^:private wheel-slots 120)

(def ^:private tints
  {:glass 0x1d2733 :trim 0x2b2b2e})

(def ^:private paints
  "Everyone else, in colours that are nobody's catalogue paint and nobody's
  rival palette. A car in this game is either yours, the opposition's, traffic,
  or another player, and you have to be able to tell which at a glance."
  [0x38b6d8 0x39c46a 0xd8c23a 0xd85fa8 0x8f7ae8 0x2fb39b])

(defn- rig-for
  "One catalogue vehicle as a figure rig: hull, bodywork, four wheels."
  [kind]
  (let [spec (cars/spec kind)
        [hx hy hz] (:half spec)
        layout (cars/layout kind)
        conns (:connections layout)
        radii (:radii layout)
        widths (:widths layout)]
    (fig/rig
     (concat
      [{:shape :box :at [0.0 0.0 0.0] :size [(* 2 hx) (* 2 hy) (* 2 hz)] :tint :paint}]
      ;; The catalogue stores bodywork as half-extents, the same as the hull.
      (for [[x y z bx by bz tint] (:body spec)]
        {:shape :box :at [x y z] :size [(* 2 bx) (* 2 by) (* 2 bz)] :tint tint})
      (for [i (range 4)
            :let [[cx cy cz] (nth conns i)
                  r (nth radii i)]]
        {:shape :wheel :spin? true
         ;; Drawn at the mount rather than hanging on suspension travel: the
         ;; wire does not carry a remote car's suspension and never will, for
         ;; four numbers nobody can see from another car.
         :at [cx (- cy (* 0.15 r)) cz]
         :size [(nth widths i) (* 2 r) (* 2 r)]})))))

(defn create [world scene _textures]
  (let [material (three/MeshPhongMaterial. #js {:color 0xffffff :shininess 26
                                                :flatShading true})
        pools {:box (fig/pool scene (three/BoxGeometry. 1 1 1) material
                              box-slots {:receive? true})
               :wheel (fig/pool scene
                                (doto (three/CylinderGeometry. 0.5 0.5 1 10)
                                  (.rotateZ (/ js/Math.PI 2)))
                                material wheel-slots {})}
        rigs (into {} (for [k cars/kinds] [k (rig-for k)]))]
    (atom {:world world :scene scene
           :pools pools
           :rigs rigs
           :rig-meshes (into {} (for [[k r] rigs]
                                  [k (into-array (map #(:mesh (get pools (:shape %)))
                                                      (:parts r)))]))
           :body-m (three/Matrix4.)
           :local-m (three/Matrix4.)
           :out-m (three/Matrix4.)
           :qpos (three/Vector3.)
           ;; Not `:quat`: the sample being placed is also called that, and one
           ;; of them has to win the destructuring in `sync!`.
           :qrot (three/Quaternion.)
           :one (three/Vector3. 1 1 1)
           :players {}      ; player-id -> {:body :kind :slots :meshes :buffer :dist :at}
           :self nil})))

(defn set-self! [rs id] (swap! rs assoc :self id))
(defn self [rs] (:self @rs))

(defn- spawn! [rs id kind [x y z]]
  (let [{:keys [^js world pools rigs rig-meshes]} @rs
        rig (get rigs kind)
        parts (:parts rig)
        [hx hy hz] (cars/half kind)
        ^js body (.createRigidBody
                  world
                  (-> (.kinematicPositionBased RAPIER/RigidBodyDesc)
                      (.setTranslation x y z)))
        slots (js/Int32Array. (count parts))
        paint (nth paints (mod id (count paints)))]
    (.createCollider world
                     (-> (.cuboid RAPIER/ColliderDesc hx hy hz)
                         (.setFriction 0.8)
                         (.setRestitution 0.2))
                     body)
    (dotimes [i (count parts)]
      (let [{:keys [shape tint]} (nth parts i)
            pool (get pools shape)
            slot (fig/claim! pool)]
        (aset slots i slot)
        (fig/set-colour! pool slot (get tints tint paint))))
    {:body body :kind kind :rig rig :meshes (get rig-meshes kind) :slots slots
     :buffer (interp/buffer)
     ;; Distance covered, for the wheels. Accumulated from where the proxy has
     ;; been put rather than from a velocity: the interpolated path is what is
     ;; actually drawn, so it is what the wheels should agree with.
     :dist (js/Float32Array. 1)
     :last (js/Float32Array. 3)
     :placed? (js/Uint8Array. 1)}))

(defn- despawn! [rs {:keys [^js body rig ^js slots]}]
  (let [{:keys [^js world pools]} @rs
        parts (:parts rig)]
    (dotimes [i (count parts)]
      (fig/release! (get pools (:shape (nth parts i))) (aget slots i)))
    (.removeRigidBody world body)))

(defn observe!
  "Record a snapshot for a remote car. Frames about ourselves are ignored -- the
  server relays to peers only, but a reconnect can briefly race that."
  [rs now {:keys [id pos kind] :as car}]
  (when (not= id (:self @rs))
    (let [existing (get-in @rs [:players id])
          entry    (or existing (spawn! rs id (cars/kind-at (or kind 0)) pos))]
      (swap! rs assoc-in [:players id]
             (update entry :buffer interp/insert now (select-keys car [:pos :quat :damage]))))))

(defn forget! [rs id]
  (when-let [p (get-in @rs [:players id])]
    (despawn! rs p)
    (swap! rs update :players dissoc id)))

(defn sync!
  "Move every proxy to where it was `interp-delay-ms` ago, and drop anyone who
  has gone quiet."
  [rs now]
  (let [{:keys [players pools ^js body-m ^js local-m ^js out-m
                ^js qpos ^js qrot ^js one]} @rs]
    (doseq [[id {:keys [^js body kind rig ^js meshes ^js slots buffer
                        ^js dist ^js last ^js placed?]}] players]
      (if (interp/stale? buffer now stale-ms)
        (forget! rs id)
        (when-let [{:keys [pos quat]} (interp/sample-at buffer now)]
          (let [[x y z] pos
                [qx qy qz qw] quat]
            (when (pos? (aget placed? 0))
              (aset dist 0 (+ (aget dist 0)
                              (js/Math.hypot (- x (aget last 0)) (- z (aget last 2))))))
            (aset last 0 x) (aset last 1 y) (aset last 2 z)
            (aset placed? 0 1)
            (.setNextKinematicTranslation body #js {:x x :y y :z z})
            (.setNextKinematicRotation body #js {:x qx :y qy :z qz :w qw})
            (.set qpos x y z)
            (.set qrot qx qy qz qw)
            (.compose body-m qpos qrot one)
            (fig/place-rig! rig meshes slots body-m local-m out-m
                            (/ (aget dist 0) (:radius (cars/layout kind))))))))
    (fig/flush! (:box pools))
    (fig/flush! (:wheel pools))))

(defn count-players [rs] (count (:players @rs)))

(defn blips
  "Where the other players are, for the map."
  [rs]
  (mapv (fn [[id {:keys [^js body]}]]
          (let [t (.translation body)]
            {:id id :x (.-x t) :z (.-z t)}))
        (:players @rs)))
