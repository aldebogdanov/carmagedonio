(ns carmageddon.client.chunks
  "Streams the infinite world in and out around the player.

  Deliberately knows nothing about three.js: it owns chunk lifetime and physics
  colliders, and calls out through `on-add` / `on-remove` for anything visual.
  That keeps the whole module runnable under Node, which is how the heightfield
  orientation below is verified rather than assumed.

  Generation goes through an injected `generate` fn returning a promise, so the
  manager does not care whether chunks come from a Worker or from this thread.
  The default is the Worker: chunk cost swings ~5x with hardware (~4 ms fast,
  ~19 ms slow) and a chunk is generated as one indivisible unit, so a per-frame
  budget can decline to start another but cannot stop one halfway. On slower
  machines that dropped a frame at every boundary crossing.

  Colliders cover a smaller radius than meshes: you can see much further than
  you can crash into, and heightfield colliders are the expensive half."
  (:require ["@dimforge/rapier3d-compat" :as RAPIER]
            [carmageddon.shared.constants :as k]
            [carmageddon.shared.worldgen :as worldgen]))

;; How many chunks may be in flight at once. The worker handles one at a time,
;; so this is really queue depth; keeping it small means a sharp change of
;; direction is not stuck behind a backlog of chunks nobody wants any more.
(def ^:private max-in-flight 3)

(defn add-collider!
  "Rapier heightfields are centred on the collider's translation and store
  heights column-major. `nrows`/`ncols` count cells, so a 33x33 vertex grid is
  a 32x32 cell heightfield."
  ([world data] (add-collider! world data (dec (:verts data))))
  ([^js world {:keys [cx cz verts heights size]} cells]
   (let [half (* 0.5 size)]
     (.createCollider
      world
      (-> (.heightfield RAPIER/ColliderDesc cells cells heights
                        #js {:x size :y 1.0 :z size})
          (.setTranslation (+ (* cx size) half) 0.0 (+ (* cz size) half))
          (.setFriction 1.0)
          (.setRestitution 0.05))))))

(defn- normalise
  "Worker results arrive as a plain JS object across the postMessage boundary;
  same-thread results are already a map. Both end up in one shape."
  [r]
  (if (map? r)
    r
    {:cx (unchecked-get r "cx") :cz (unchecked-get r "cz")
     :verts (unchecked-get r "verts") :size (unchecked-get r "size")
     :origin [(unchecked-get r "x0") (unchecked-get r "z0")]
     :heights (unchecked-get r "heights") :colors (unchecked-get r "colors")
     :props (unchecked-get r "props")
     :buildings (unchecked-get r "buildings")
     :building-parts (unchecked-get r "parts")
     :bridges (unchecked-get r "bridges")
     :flora (unchecked-get r "flora")
     :traffic (unchecked-get r "traffic")
     :peds (unchecked-get r "peds")
     :furniture (unchecked-get r "furniture")}))

(defn worker-generator
  "Generate chunks in a Web Worker. Requests are keyed by chunk coordinate so
  replies can be matched to their promise -- the worker answers in order, but
  relying on that would break the moment it grew a thread pool."
  [url]
  (let [w       (js/Worker. url)
        pending (atom {})]
    (set! (.-onmessage w)
          (fn [e]
            (let [d   (.-data e)
                  key [(unchecked-get d "cx") (unchecked-get d "cz")]]
              (when-let [resolve-fn (get @pending key)]
                (swap! pending dissoc key)
                (resolve-fn d)))))
    (fn [seed cx cz]
      (js/Promise.
       (fn [resolve _reject]
         (swap! pending assoc [cx cz] resolve)
         (.postMessage w #js {:seed seed :cx cx :cz cz}))))))

(defn main-thread-generator
  "Same-thread fallback, used by tests and by anything running without a DOM."
  []
  (fn [seed cx cz]
    (js/Promise.resolve (worldgen/chunk-data seed cx cz))))

(defn create
  [{:keys [seed world radius collider-radius generate on-add on-remove
           on-physics-add on-physics-remove]
    :or   {radius k/stream-radius
           collider-radius k/collider-radius}}]
  (atom {:seed seed :world world
         :radius radius :collider-radius collider-radius
         :generate (or generate (worker-generator "/js/worker.js"))
         :on-add on-add :on-remove on-remove
         ;; Physics payload (heightfield + props) comes and goes with the
         ;; collider radius, not the visual one, so props exist exactly where
         ;; the player can reach them.
         :on-physics-add on-physics-add :on-physics-remove on-physics-remove
         :loaded  {}         ; [cx cz] -> {:handle :collider :data}
         :loading #{}        ; [cx cz] currently being generated
         :queue   []         ; [cx cz] wanted, nearest first
         :centre  nil}))

(defn- wanted
  "Chunk coordinates within `radius` of the centre chunk, nearest first so the
  ground under the player exists before the horizon does."
  [[ccx ccz] radius]
  (->> (for [dx (range (- radius) (inc radius))
             dz (range (- radius) (inc radius))]
         [(+ ccx dx) (+ ccz dz) (+ (* dx dx) (* dz dz))])
       (filter (fn [[_ _ d2]] (<= d2 (* radius radius))))
       (sort-by peek)
       (mapv (fn [[x z _]] [x z]))))

(defn- unload! [mgr key]
  (let [{:keys [^js world loaded on-remove on-physics-remove]} @mgr
        {:keys [handle ^js collider]} (get loaded key)]
    (when collider
      (.removeCollider world collider true)
      (when on-physics-remove (on-physics-remove key)))
    (when (and on-remove handle) (on-remove handle))
    (swap! mgr update :loaded dissoc key)))

(defn- near-centre? [[cx cz] [ccx ccz] r]
  (<= (+ (* (- cx ccx) (- cx ccx)) (* (- cz ccz) (- cz ccz))) (* r r)))

(defn- reconcile-colliders!
  "Give physics to chunks that have come within collider range, and take it away
  from those that have left.

  Not optional: a chunk first loaded out at the visual radius has no collider,
  and without this it would never gain one as the player drove towards it. The
  inner ring silently empties out and the car falls through the world."
  [mgr]
  (let [{:keys [^js world loaded centre collider-radius
                on-physics-add on-physics-remove]} @mgr]
    (when centre
      (doseq [[key {:keys [^js collider data]}] loaded]
        (let [want (near-centre? key centre collider-radius)]
          (cond
            (and want (nil? collider))
            (do (swap! mgr assoc-in [:loaded key :collider] (add-collider! world data))
                (when on-physics-add (on-physics-add key data)))

            (and (not want) collider)
            (do (.removeCollider world collider true)
                (when on-physics-remove (on-physics-remove key))
                (swap! mgr assoc-in [:loaded key :collider] nil))

            :else nil))))))

(defn- install!
  "Attach a finished chunk. Silently drops it if the player has moved far enough
  that it is no longer wanted -- otherwise a burst of direction changes would
  leave chunks scattered behind."
  [mgr key data]
  (swap! mgr update :loading disj key)
  (let [{:keys [^js world on-add centre radius collider-radius loaded
                on-physics-add]} @mgr]
    (when (and centre
               (near-centre? key centre radius)
               (not (contains? loaded key)))
      ;; Only the inner ring gets physics; the rest is scenery until the player
      ;; is close enough for it to matter. `data` is retained so
      ;; `reconcile-colliders!` can build a collider later without regenerating.
      (let [near?    (near-centre? key centre collider-radius)
            collider (when near? (add-collider! world data))
            handle   (when on-add (on-add data))]
        (when (and near? on-physics-add) (on-physics-add key data))
        (swap! mgr update :loaded assoc key
               {:handle handle :collider collider :data data})))))

(defn- request!
  "Kick off generation for one chunk. Returns the promise."
  [mgr [cx cz :as key]]
  (swap! mgr update :loading conj key)
  (-> ((:generate @mgr) (:seed @mgr) cx cz)
      (.then (fn [r] (install! mgr key (normalise r))))
      (.catch (fn [e] (swap! mgr update :loading disj key)
                (js/console.error "chunk generation failed" (pr-str key) e)))))

(defn update!
  "Call once a frame with the player's world position. Recomputes the wanted set
  when the player changes chunk, then loads at most a couple per frame."
  [mgr x z]
  (let [{:keys [radius loaded centre]} @mgr
        c (worldgen/chunk-of x z)]
    (when (not= c centre)
      (let [want (wanted c radius)
            want-set (set want)]
        (doseq [key (keys loaded)]
          (when-not (want-set key) (unload! mgr key)))
        (swap! mgr assoc
               :centre c
               :queue (vec (remove #(contains? (:loaded @mgr) %) want)))
        (reconcile-colliders! mgr)))
    ;; Top the worker up. Generation no longer costs main-thread time, so the
    ;; only limit is how much work it is worth having outstanding.
    (loop []
      (let [{:keys [queue loading loaded]} @mgr]
        (when (and (seq queue) (< (count loading) max-in-flight))
          (let [key (first queue)]
            (swap! mgr update :queue subvec 1)
            (when-not (or (contains? loaded key) (contains? loading key))
              (request! mgr key))
            (recur)))))))

(defn stats [mgr]
  (let [{:keys [loaded queue loading centre]} @mgr]
    {:loaded (count loaded)
     :pending (+ (count queue) (count loading))
     :colliders (count (filter (comp some? :collider) (vals loaded)))
     :centre centre}))

(defn ensure-loaded!
  "Promise that resolves once the chunks the player could immediately touch are
  in, so they do not spawn over a hole.

  Only the physics radius is awaited. Waiting for the whole visual radius meant
  a much longer wait for ground that fog hides anyway; the rest streams in
  behind it."
  [mgr x z]
  (swap! mgr assoc :centre nil)
  (update! mgr x z)
  (let [{:keys [centre collider-radius]} @mgr
        near (filterv #(near-centre? % centre collider-radius) (:queue @mgr))]
    (swap! mgr update :queue #(vec (remove (set near) %)))
    (-> (js/Promise.all (into-array (map #(request! mgr %) near)))
        (.then (fn [_] (reconcile-colliders! mgr) (stats mgr))))))

