(ns carmageddon.client.buildings
  "City blocks: static, solid, and unlike props not going anywhere.

  Buildings need no per-frame sync and no destruction bookkeeping, which is why
  they are separate from `props` rather than a flag on it -- the two have almost
  nothing in common at runtime.

  All of them share one unit-cube geometry, scaled per building, and one
  material per kind. Building geometry per instance would mean tracking and
  disposing a few hundred GPU buffers as the player drives."
  (:require ["@dimforge/rapier3d-compat" :as RAPIER]
            ["three" :as three]
            [carmageddon.shared.worldgen :as worldgen]))

(defn create [world scene textures]
  (atom {:world world :scene scene
         :geometry (three/BoxGeometry. 1 1 1)
         :materials (mapv (fn [t]
                            (three/MeshPhongMaterial. #js {:map t :shininess 8}))
                          (:facades textures))
         :chunks {}}))

(defn- spawn-one!
  [bs x y z hx hz height kind]
  (let [{:keys [^js world ^js scene ^js geometry materials]} @bs
        half-h (* 0.5 height)
        ;; Sunk slightly so a building on a slope meets the ground on every side
        ;; rather than showing daylight under the downhill corner.
        cy     (+ y half-h -0.6)
        ^js collider (.createCollider
                      world
                      (-> (.cuboid RAPIER/ColliderDesc hx half-h hz)
                          (.setTranslation x cy z)
                          (.setFriction 0.9)
                          (.setRestitution 0.05)))
        ^js mesh (three/Mesh. geometry (nth materials (mod kind (count materials))))]
    (.set (.-scale mesh) (* 2 hx) height (* 2 hz))
    (.set (.-position mesh) x cy z)
    (.add scene mesh)
    {:collider collider :mesh mesh}))

(defn add-chunk! [bs key arr]
  (when (and arr (pos? (.-length arr)))
    (let [n (/ (.-length arr) worldgen/building-stride)
          made (mapv (fn [i]
                       (let [o (* i worldgen/building-stride)]
                         (spawn-one! bs
                                     (aget arr (+ o 0)) (aget arr (+ o 1)) (aget arr (+ o 2))
                                     (aget arr (+ o 3)) (aget arr (+ o 4)) (aget arr (+ o 5))
                                     (int (aget arr (+ o 6))))))
                     (range n))]
      (swap! bs assoc-in [:chunks key] made)
      made)))

(defn remove-chunk! [bs key]
  (let [{:keys [^js world ^js scene chunks]} @bs]
    (doseq [{:keys [^js collider ^js mesh]} (get chunks key)]
      (.remove scene mesh)
      (.removeCollider world collider true))
    (swap! bs update :chunks dissoc key)))

(defn stats [bs]
  {:standing (reduce + (map count (vals (:chunks @bs))))
   :chunks (count (:chunks @bs))})
