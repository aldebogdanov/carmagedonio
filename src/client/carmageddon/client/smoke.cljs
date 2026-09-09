(ns carmageddon.client.smoke
  "Plumes from chimneys.

  Smoke is the one thing in this world that is neither a volume nor a
  collider: it has no shape worth instancing accurately and nothing can ever
  hit it. So it gets its own module and its own pool rather than being more
  parts -- a handful of soft blobs per chimney, rising, spreading and
  vanishing, on a loop.

  No particle system, no lifetimes, no allocation. Each puff's position is a
  pure function of the clock and of which puff it is: `p` goes 0 to 1, the puff
  climbs, swells, and is scaled out of existence at the top, and the next one
  is already a sixth of the way up. That is what a chimney looks like from a
  car, and it costs one matrix per puff per frame."
  (:require ["three" :as three]
            [carmageddon.client.figures :as fig]
            [carmageddon.shared.worldgen :as worldgen]))

;; Ten rather than six, and each one only half again as big as it starts.
;; With six the column had daylight between the puffs and read as a handful of
;; grey rocks going up in single file; what makes it smoke is that they overlap
;; enough for the eye to stop counting them.
(def ^:private puffs 10)       ; per chimney
;; Measured: an industrial chunk has 8 to 16 chimneys and a city one about 3,
;; so a loaded estate is a couple of hundred emitters. This is that, doubled.
(def ^:private slots 3200)

;; How far the plume leans over as it rises, as a fraction of its height, and
;; how slowly that direction wanders. There is no wind in the simulation; this
;; is the whole of the weather as far as a chimney is concerned, and it exists
;; because a column of smoke going straight up reads as a column of smoke going
;; nowhere.
(def ^:private lean 0.42)
(def ^:private veer 0.035)     ; rad/s the lean direction turns

(defn create [^js scene]
  (atom {:scene scene
         :pool (fig/pool scene
                         ;; One subdivision. A bare icosahedron is a rock at
                         ;; any distance you can see the facets from, and the
                         ;; material is unlit so there is no shading to hide
                         ;; them behind.
                         (three/IcosahedronGeometry. 0.5 1)
                         (three/MeshBasicMaterial.
                          #js {:color 0x74716c :transparent true :opacity 0.26
                               :depthWrite false})
                         slots
                         ;; A shadow from a puff of smoke is a black spot
                         ;; skidding across the yard.
                         {:cast? false})
         :scratch (three/Object3D.)
         :chunks {}}))

(defn- read-emitters
  "The chunk's chimneys, with a slot claimed for each of their puffs."
  [pool arr]
  (let [st worldgen/smoke-stride
        n  (/ (.-length arr) st)]
    (vec (for [i (range n)
               :let [o (* i st)
                     ;; Two chimneys side by side must not puff in step. The
                     ;; offset comes from the position rather than a counter,
                     ;; so it survives the chunk being unloaded and reloaded.
                     x (aget arr (+ o 0))
                     z (aget arr (+ o 2))]]
           {:x x :y (aget arr (+ o 1)) :z z
            :r (aget arr (+ o 3))
            :rise (aget arr (+ o 4))
            :rate (aget arr (+ o 5))
            :phase (mod (* 0.017 (+ (* 7.3 x) (* 3.1 z))) 1.0)
            :slots (vec (for [_ (range puffs)] (fig/claim! pool)))}))))

(defn add-chunk! [ss key arr]
  (when (and arr (pos? (.-length arr)))
    (let [{:keys [pool]} @ss
          es (read-emitters pool arr)]
      (swap! ss assoc-in [:chunks key] es)
      (count es))))

(defn remove-chunk! [ss key]
  (let [{:keys [pool chunks]} @ss]
    (doseq [e (get chunks key), s (:slots e)]
      (when (nat-int? s) (fig/release! pool s)))
    (swap! ss update :chunks dissoc key)))

(defn update!
  "Put every puff where it should be at `t` seconds."
  [ss t]
  (let [{:keys [pool chunks ^js scratch]} @ss
        ;; One wind for the whole world, turning slowly. Everything leans the
        ;; same way, which is what makes it read as weather rather than as a
        ;; per-chimney quirk.
        a (* veer t)
        wx (js/Math.sin a)
        wz (js/Math.cos a)]
    (doseq [[_ es] chunks
            {:keys [x y z r rise rate phase slots]} es
            [i s] (map-indexed vector slots)
            :when (nat-int? s)]
      (let [p (mod (+ (* rate t) phase (/ (double i) puffs)) 1.0)
            h (* rise p)
            d (* lean h)
            ;; Grows all the way up and is taken away at the top rather than
            ;; faded: the material has one opacity for every instance, so the
            ;; only per-puff channel is its size.
            sc (* r (+ 0.8 (* 1.5 p)) (min 1.0 (* 3.0 (- 1.0 p))))]
        (.set (.-position scratch) (+ x (* wx d)) (+ y h) (+ z (* wz d)))
        (.set (.-scale scratch) sc sc sc)
        (.updateMatrix scratch)
        (fig/set-matrix! pool s (.-matrix scratch))))
    (fig/flush! pool)))

(defn stats [ss]
  {:chimneys (reduce + (map count (vals (:chunks @ss))))
   :puffs (fig/used (:pool @ss))})
