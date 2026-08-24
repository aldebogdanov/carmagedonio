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
  "Use the first world the server knows about, or create one. The seed it hands
  back is the entire world-sync protocol: everything else is derived."
  []
  (-> (get-edn "/api/worlds")
      (.then (fn [res]
               (if-let [w (first (:worlds res))]
                 w
                 (post-edn "/api/worlds" {:name "carmagedonio"}))))))

(defn submit-run! [world-id profile-id result]
  (when (and world-id profile-id)
    (post-edn "/api/runs" (assoc result :world-id world-id :profile-id profile-id))))

(defn leaderboard [world-id]
  (get-edn (str "/api/worlds/" world-id "/leaderboard")))
