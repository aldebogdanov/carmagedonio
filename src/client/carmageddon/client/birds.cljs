(ns carmageddon.client.birds
  "Birds. No physics, no collider, no chunk -- three flocks that keep station
  near the player and are a pure function of the clock.

  Everything else alive in this world is generated per chunk and streamed, which
  is right for anything the car can hit. Birds are the opposite case: nothing
  interacts with them, they are only ever seen against the sky, and tying them
  to chunks would mean loading and unloading a few dozen instances for something
  the player can never reach. Following the player instead costs one instanced
  draw for the whole world."
  (:require ["three" :as three]))

(def ^:private flocks 3)
(def ^:private per-flock 14)
(def ^:private bird-count (* flocks per-flock))

(def ^:private orbit-radius 170.0)     ; how far out a flock circles
(def ^:private formation 26.0)    ; how loosely the birds hold formation
(def ^:private base-alt 34.0)
(def ^:private top-alt 58.0)

(defn create [scene]
  (let [;; A flattened, tapered box reads as a bird from fifty metres up, which
        ;; is the only distance one is ever seen from.
        geom (three/BoxGeometry. 1.5 0.14 0.42)
        mat  (three/MeshPhongMaterial. #js {:color 0x33383f :flatShading true
                                            :shininess 0})
        ^js mesh (three/InstancedMesh. geom mat bird-count)]
    (set! (.-frustumCulled mesh) false)
    (.add scene mesh)
    {:mesh mesh :scene scene
     :scratch (three/Object3D.)}))

(defn update!
  "Fly. `t` is seconds; the whole flock is derived from it, so there is no state
  to advance and nothing to get out of step between frames."
  [{:keys [^js mesh ^js scratch]} t px pz]
  (dotimes [f flocks]
    (let [;; Each flock circles at its own rate and phase, and drifts.
          sp   (+ 0.055 (* 0.018 f))
          a    (+ (* t sp) (* f 2.4))
          cx   (+ px (* orbit-radius (js/Math.cos a)))
          cz   (+ pz (* orbit-radius (js/Math.sin a)))
          cy   (+ base-alt (* (- top-alt base-alt) (+ 0.5 (* 0.5 (js/Math.sin (+ (* t 0.11) f))))))
          ;; Flight direction is the tangent of the circle it is flying.
          head (+ a (/ js/Math.PI 2))]
      (dotimes [i per-flock]
        (let [phase (* i 1.7)
              ;; Loose formation: each bird holds a slightly different orbit.
              ox (* formation (js/Math.cos (+ (* t 0.5) phase)))
              oz (* formation (js/Math.sin (+ (* t 0.42) (* phase 1.3))))
              oy (* 5.0 (js/Math.sin (+ (* t 0.7) phase)))
              ;; Wingbeat as roll, which is all the animation a silhouette needs.
              roll (* 0.55 (js/Math.sin (+ (* t 6.0) phase)))
              n (+ (* f per-flock) i)]
          (.set (.-position scratch) (+ cx ox) (+ cy oy) (+ cz oz))
          (.set (.-rotation scratch) 0 head roll "YXZ")
          (.updateMatrix scratch)
          (.setMatrixAt mesh n (.-matrix scratch))))))
  (set! (.-needsUpdate (.-instanceMatrix mesh)) true))
