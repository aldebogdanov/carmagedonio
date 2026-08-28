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

(defn- worker-count
  "How many chunk workers to run.

  One less than the machine claims, so the main thread keeps a core to render
  on, and capped: a chunk is about five milliseconds of work and the queue is
  rarely more than a dozen deep, so more workers past this point buy nothing
  and cost a copy of the generator each."
  []
  (let [n (or (.-hardwareConcurrency js/navigator) 2)]
    (max 1 (min 4 (dec n)))))

(defn worker-generator
  "Generate chunks in a pool of Web Workers.

  Requests are keyed by chunk coordinate so replies can be matched to their
  promise: with a pool the answers genuinely do come back out of order, which is
  exactly what the keying was there to allow. Work is handed to whichever worker
  has the least outstanding, which with a queue this shallow is as good as any
  scheduler would manage."
  ([url] (worker-generator url (worker-count)))
  ([url n]
   (let [pending (atom {})
         load    (atom (vec (repeat n 0)))
         workers (mapv (fn [i]
                         (let [w (js/Worker. url)]
                           (set! (.-onmessage w)
                                 (fn [e]
                                   (let [d (.-data e)
                                         key [(unchecked-get d "cx")
                                              (unchecked-get d "cz")]]
                                     (swap! load update i dec)
                                     (when-let [resolve-fn (get @pending key)]
                                       (swap! pending dissoc key)
                                       (resolve-fn d)))))
                           w))
                       (range n))]
     (fn [seed cx cz]
       (js/Promise.
        (fn [resolve _reject]
          (let [i (apply min-key (fn [j] (nth @load j)) (range n))
                ^js w (nth workers i)]
            (swap! load update i inc)
            (swap! pending assoc [cx cz] resolve)
            (.postMessage w #js {:seed seed :cx cx :cz cz}))))))))

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
         :queue   []         ; [cx cz] wanted, most urgent first
         :centre  nil
         :last    nil}))     ; last position, for a heading

(def ^:private lookahead
  "How strongly to favour the direction of travel when ordering the queue.

  At 150 km/h the chunk ahead is needed several seconds before the one to the
  side, and both are the same distance away. This biases the ordering by where
  the car is going rather than only by where it is, which costs one dot product
  per chunk and is the difference between the world arriving before you and
  arriving with you."
  2.2)

(defn- wanted
  "Chunk coordinates within `radius` of the centre chunk, in the order they are
  wanted -- nearest first, and ahead before behind."
  ([centre radius] (wanted centre radius 0.0 0.0))
  ([[ccx ccz] radius vx vz]
   (let [sp (js/Math.hypot vx vz)
         [ux uz] (if (> sp 1.0) [(/ vx sp) (/ vz sp)] [0.0 0.0])
         bias (* lookahead (min 1.0 (/ sp 25.0)))]
     (->> (for [dx (range (- radius) (inc radius))
                dz (range (- radius) (inc radius))]
            (let [d2 (+ (* dx dx) (* dz dz))
                  ;; Distance along the heading, in chunks; negative behind.
                  along (+ (* dx ux) (* dz uz))]
              [(+ ccx dx) (+ ccz dz) d2
               (- (js/Math.sqrt d2) (* bias along))]))
          (filter (fn [[_ _ d2 _]] (<= d2 (* radius radius))))
          (sort-by peek)
          (mapv (fn [[x z _ _]] [x z]))))))

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

(def ^:private keep-slack
  "Chunks are loaded at `radius` and only dropped past `radius + slack`.

  Without the gap, driving along a chunk boundary loads and unloads the same
  ring continuously -- every crossing throws away work that is wanted again a
  second later. One chunk of hysteresis costs a few megabytes and removes the
  thrash entirely."
  1)

(defn update!
  "Call once a frame with the player's world position. Recomputes the wanted set
  when the player changes chunk, then tops up the workers."
  [mgr x z]
  (let [{:keys [radius loaded centre last]} @mgr
        c (worldgen/chunk-of x z)
        ;; Heading, from where the player was when this last ran. Frame-to-frame
        ;; movement is small, so this is smooth enough to order a queue by.
        [vx vz] (if last [(- x (nth last 0)) (- z (nth last 1))] [0.0 0.0])]
    (swap! mgr assoc :last [x z])
    (when (not= c centre)
      (let [want (wanted c radius (* 60.0 vx) (* 60.0 vz))
            want-set (set want)
            ;; Hysteresis: keep anything still within the slack ring, even
            ;; though it is not being asked for.
            keep? (fn [key] (or (want-set key)
                                (near-centre? key c (+ radius keep-slack))))]
        (doseq [key (keys loaded)]
          (when-not (keep? key) (unload! mgr key)))
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

