(ns carmageddon.client.api
  "Talking to the backend.

  Every call degrades to nil rather than throwing. The dev workflow serves the
  client from shadow-cljs on :8080, which has no API behind it, and the game
  must remain entirely playable in that setup -- the backend records runs, it
  does not gate them. A server that has to be running to play would make the
  fastest iteration loop the one that skips the server."
  (:require [cljs.reader :as reader]))

(def ^:private storage-key "carmagedonio/profile")

(defn- parse [^js res]
  (-> (.text res)
      (.then (fn [t]
               (when (and (.-ok res) (seq t))
                 (reader/read-string t))))))

(defn get-edn [path]
  (-> (js/fetch path)
      (.then parse)
      (.catch (fn [_] nil))))

(defn post-edn [path body]
  (-> (js/fetch path
                #js {:method "POST"
                     :headers #js {"content-type" "application/edn"}
                     :body (pr-str body)})
      (.then parse)
      (.catch (fn [_] nil))))

(defn- remembered-profile []
  (try (.getItem js/localStorage storage-key) (catch :default _ nil)))

(defn- remember-profile! [id]
  (try (.setItem js/localStorage storage-key id) (catch :default _ nil)))

(defn ensure-profile!
  "Reuse the profile this browser already has, or ask the server for one.

  Identity is a name and an id, nothing more -- there is no account and nothing
  to authenticate. When multiplayer needs real identities this is where they
  land, and until then pretending otherwise would be theatre."
  [name]
  (let [id (remembered-profile)]
    (if id
      (-> (get-edn (str "/api/profiles/" id))
          (.then (fn [p]
                   ;; The id may be stale if the store was reset under us.
                   (if p p (-> (post-edn "/api/profiles" {:name name})
                               (.then (fn [p] (when p (remember-profile! (:id p))) p)))))))
      (-> (post-edn "/api/profiles" {:name name})
          (.then (fn [p] (when p (remember-profile! (:id p))) p))))))

(defn ensure-world!
  "The world to play in. The seed it hands back is the entire world-sync
  protocol: everything else is derived.

  With an `id`, that world and no other -- which is how two people arrange to
  be in the same one: whoever starts it shares the link. Without, the first
  world the server knows about, or a new one."
  ([] (ensure-world! nil))
  ([id]
   (if id
     (-> (get-edn (str "/api/worlds/" id))
         (.then (fn [w]
                  ;; A link to a world this server has never heard of is worth
                  ;; saying so about rather than silently dropping the player
                  ;; into a different one.
                  (when-not w (js/console.warn "no such world:" id))
                  w)))
     (-> (get-edn "/api/worlds")
         (.then (fn [res]
                  (if-let [w (first (:worlds res))]
                    w
                    (post-edn "/api/worlds" {:name "carmagedonio"}))))))))

(defn list-worlds
  "Every room the server knows about, or nil when there is no server.

  The callback is a `fn`, not the keyword itself. `.then` accepts any value and
  *ignores* a non-callable one, passing the resolved value straight through --
  and a ClojureScript keyword is an object, not a JS function. So `(.then
  :worlds)` silently yielded the whole `{:worlds [...]}` map, which `vec` then
  turned into a list containing one map entry: the lobby showed a single
  nameless, seedless room on a server with no rooms at all."
  []
  (-> (get-edn "/api/worlds")
      (.then (fn [r] (when (map? r) (:worlds r))))))

(defn create-world!
  "Start a new room. `seed` may be nil, in which case the server picks one --
  which is the normal case: a seed is a world, and nobody has an opinion about
  which world until they have seen one."
  [{:keys [name seed mode]}]
  (post-edn "/api/worlds"
            (cond-> {:name (or name "carmagedonio") :mode (or mode :normal)}
              seed (assoc :seed seed))))

(defn world-players
  "How many people are in a room right now, from the server's own live session
  list rather than anything a client reported."
  [id]
  (-> (get-edn (str "/api/worlds/" id "/scoreboard"))
      (.then (fn [r] (count (:players r))))
      (.catch (fn [_] 0))))

(defn submit-run! [world-id profile-id result]
  (when (and world-id profile-id)
    (post-edn "/api/runs" (assoc result :world-id world-id :profile-id profile-id))))

(defn leaderboard [world-id]
  (get-edn (str "/api/worlds/" world-id "/leaderboard")))
