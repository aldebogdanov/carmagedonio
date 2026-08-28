(ns carmageddon.server.store
  "Persistence, behind a protocol.

  The file-backed implementation is honest about what it is: a single EDN
  document rewritten atomically on every change. That is entirely adequate for
  one process and a few thousand runs, and it keeps the project free of database
  infrastructure it does not yet need. The protocol is the point -- when runs
  outgrow a file, a JDBC implementation drops in without anything above this
  namespace noticing.

  Writes go to a temporary file and are then renamed, so a crash mid-write
  leaves the previous state intact rather than a half-written document."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import (java.io File)
           (java.nio.file Files StandardCopyOption)
           (java.time Instant)))

(defprotocol Store
  (create-world!   [this world])
  (get-world       [this id])
  (set-overrides!  [this id overrides])
  (list-worlds     [this])
  (create-profile! [this profile])
  (get-profile     [this id])
  (submit-run!     [this run])
  (runs-for-profile [this profile-id])
  (leaderboard     [this world-id limit])
  (snapshot        [this]))

(defn- new-id [prefix]
  (str prefix "_" (subs (str (random-uuid)) 0 8)))

(defn- now [] (str (Instant/now)))

(def ^:private empty-db {:worlds {} :profiles {} :runs {}})

(defn- apply-op
  "Pure state transitions, so both implementations share the same semantics and
  the in-memory one is a genuine stand-in for the durable one in tests."
  [db op arg]
  (case op
    :world   (let [w (assoc arg :id (new-id "w") :created-at (now))]
               [(assoc-in db [:worlds (:id w)] w) w])
    :profile (let [p (assoc arg :id (new-id "p") :created-at (now))]
               [(assoc-in db [:profiles (:id p)] p) p])
    :run     (let [r (assoc arg :id (new-id "r") :submitted-at (now))]
               [(assoc-in db [:runs (:id r)] r) r])
    ;; Overrides are the authored part of a world: the places a seed alone
    ;; would not put there. Merged rather than replaced, so setting one
    ;; landmark does not silently drop the rest.
    :overrides (let [[id ov] arg
                     w (get-in db [:worlds id])]
                 (when w
                   (let [w' (update w :overrides merge ov)]
                     [(assoc-in db [:worlds id] w') w'])))))

(defn- top-runs [db world-id limit]
  (->> (vals (:runs db))
       (filter #(= world-id (:world-id %)))
       (sort-by (juxt (comp - :score) :elapsed))
       (take limit)
       vec))

(defn- store-on [state persist!]
  (reify Store
    (create-world! [_ world]
      (let [[db w] (apply-op @state :world world)]
        (reset! state db) (persist! db) w))
    (get-world [_ id] (get-in @state [:worlds id]))
    (set-overrides! [_ id overrides]
      (when-let [[db w] (apply-op @state :overrides [id overrides])]
        (reset! state db) (persist! db) w))
    (list-worlds [_] (vec (sort-by :created-at (vals (:worlds @state)))))
    (create-profile! [_ profile]
      (let [[db p] (apply-op @state :profile profile)]
        (reset! state db) (persist! db) p))
    (get-profile [_ id] (get-in @state [:profiles id]))
    (submit-run! [_ run]
      (let [[db r] (apply-op @state :run run)]
        (reset! state db) (persist! db) r))
    (runs-for-profile [_ profile-id]
      (->> (vals (:runs @state))
           (filter #(= profile-id (:profile-id %)))
           (sort-by :submitted-at)
           reverse
           vec))
    (leaderboard [_ world-id limit] (top-runs @state world-id limit))
    (snapshot [_] @state)))

(defn in-memory
  "Non-durable store. Used by tests, and by anything that wants a scratch server."
  ([] (in-memory empty-db))
  ([db] (store-on (atom db) (fn [_]))))

(defn- write-atomically! [^File file db]
  (let [tmp (File/createTempFile "carm-store" ".edn" (.getParentFile (.getAbsoluteFile file)))]
    (spit tmp (pr-str db))
    (Files/move (.toPath tmp) (.toPath (.getAbsoluteFile file))
                (into-array java.nio.file.CopyOption
                            [StandardCopyOption/REPLACE_EXISTING
                             StandardCopyOption/ATOMIC_MOVE]))))

(defn file-backed
  "Durable store in a single EDN file, loaded on start."
  [path]
  (let [file (io/file path)
        db   (if (.exists file)
               (edn/read-string (slurp file))
               empty-db)
        state (atom db)]
    (io/make-parents file)
    (store-on state (fn [db] (write-atomically! file db)))))
