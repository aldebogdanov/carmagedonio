(ns carmageddon.server.main
  "Process entry point: open a store, mount the API, serve the client.

  The server does not and will not simulate. No JVM physics engine agrees with
  Rapier bit-for-bit, so clients own their own vehicle; this process owns the
  things a client should not be trusted with -- the rules, the scores, and the
  record of what happened. See `carmageddon.server.api`."
  (:require [carmageddon.server.api :as api]
            [carmageddon.server.store :as store]
            [clojure.edn :as edn]
            [org.httpkit.server :as http])
  (:gen-class))

(defonce ^:private server (atom nil))
(defonce ^:private db (atom nil))

(def ^:private default-db-path "data/carmagedonio.edn")

(defn the-store [] @db)

(defn stop! []
  (when-let [s @server]
    (http/server-stop! s)
    (reset! server nil)))

(defn start!
  ([] (start! (or (some-> (System/getenv "PORT") Integer/parseInt) 3000)))
  ([port] (start! port (or (System/getenv "CARM_DB") default-db-path)))
  ([port db-path]
   (stop!)
   (reset! db (store/file-backed db-path))
   (reset! server (http/run-server (api/handler @db)
                                   {:port port :legacy-return-value? false}))
   (println (str "carmagedonio server on http://localhost:" port
                 "  (store: " db-path ")"))
   @server))

(defn -main [& args]
  ;; No argument means "whatever $PORT says, else 3000", which is what the
  ;; three-arity `start!` already implements. Passing 3000 in here instead --
  ;; which is what this did -- made $PORT dead for anyone running the jar,
  ;; including the systemd unit that deploys it.
  (if-let [p (some-> (first args) edn/read-string)]
    (start! p)
    (start!))
  @(promise))
