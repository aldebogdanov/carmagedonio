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
  game with no shared determinism."
  (:require ["@dimforge/rapier3d-compat" :as RAPIER]
            ["three" :as three]
            [carmageddon.client.sim :as sim]
            [carmageddon.shared.interp :as interp]))

(def ^:private stale-ms 4000)

(defn create [world scene textures]
  (atom {:world world :scene scene
         :body-geometry (let [[hx hy hz] sim/chassis-half]
                          (three/BoxGeometry. (* 2 hx) (* 2 hy) (* 2 hz)))
         :material (three/MeshPhongMaterial. #js {:map (:body textures)
                                                  :color 0x88aaff :shininess 6})
         :players {}      ; player-id -> {:body :mesh :buffer}
         :self nil}))

(defn set-self! [rs id] (swap! rs assoc :self id))
(defn self [rs] (:self @rs))

(defn- spawn! [rs id [x y z]]
  (let [{:keys [^js world ^js scene ^js body-geometry ^js material]} @rs
        ^js body (.createRigidBody
                  world
                  (-> (.kinematicPositionBased RAPIER/RigidBodyDesc)
                      (.setTranslation x y z)))
        [hx hy hz] sim/chassis-half
        ^js mesh (three/Mesh. body-geometry material)]
    (.createCollider world
                     (-> (.cuboid RAPIER/ColliderDesc hx hy hz)
                         (.setFriction 0.8)
                         (.setRestitution 0.2))
                     body)
    (set! (.-castShadow mesh) true)
    (set! (.-receiveShadow mesh) true)
    (.add scene mesh)
    {:body body :mesh mesh :buffer (interp/buffer)}))

(defn- despawn! [rs {:keys [^js body ^js mesh]}]
  (let [{:keys [^js world ^js scene]} @rs]
    (.remove scene mesh)
    (.removeRigidBody world body)))

(defn observe!
  "Record a snapshot for a remote car. Frames about ourselves are ignored -- the
  server relays to peers only, but a reconnect can briefly race that."
  [rs now {:keys [id pos] :as car}]
  (when (not= id (:self @rs))
    (let [existing (get-in @rs [:players id])
          entry    (or existing (spawn! rs id pos))]
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
  (doseq [[id {:keys [^js body ^js mesh buffer]}] (:players @rs)]
    (if (interp/stale? buffer now stale-ms)
      (forget! rs id)
      (when-let [{:keys [pos quat]} (interp/sample-at buffer now)]
        (let [[x y z] pos
              [qx qy qz qw] quat]
          (.setNextKinematicTranslation body #js {:x x :y y :z z})
          (.setNextKinematicRotation body #js {:x qx :y qy :z qz :w qw})
          (.set (.-position mesh) x y z)
          (.set (.-quaternion mesh) qx qy qz qw))))))

(defn count-players [rs] (count (:players @rs)))
