(ns user
  "REPL entry point: `clojure -M:dev -r`, then (go)."
  (:require [carmageddon.server.main :as server]
            [carmageddon.server.store :as store]))

(defn go []   (server/start!))
(defn halt [] (server/stop!))
(defn db []   (store/snapshot (server/the-store)))
