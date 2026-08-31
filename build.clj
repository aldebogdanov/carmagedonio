(ns build
  "Produces `target/server.jar`: one runnable artifact carrying the server, the
  shared world generator, and the compiled client.

  The client is built here rather than by the release workflow because the two
  halves are not independent -- `carmageddon.shared.worldgen` is compiled into
  both, and a jar whose server and whose JavaScript came from different commits
  generates two different worlds. One command, one commit, one artifact.

  Run with `clojure -T:build uber`."
  (:require [clojure.tools.build.api :as b]))

(def ^:private class-dir "target/classes")
(def ^:private uber-file "target/server.jar")
(def ^:private main-ns 'carmageddon.server.main)

(defn- basis
  "The server's classpath. `:server` carries http-kit, reitit and ring plus
  `src/server`; the base deps carry the shared code and `resources`, which is
  where the compiled client and index.html live -- the API serves them straight
  off the classpath, so packaging them is the whole of 'shipping the client'."
  []
  (b/create-basis {:project "deps.edn" :aliases [:server]}))

(defn client
  "`:advanced` build of the browser client, into resources/public/js.

  The output directory is wiped first. A dev build leaves several hundred
  `cljs-runtime` files there, and `release` does not clean up after it -- left
  alone they are megabytes of dead source maps and namespace files inside the
  jar."
  [_]
  (b/delete {:path "resources/public/js"})
  (let [{:keys [exit]} (b/process {:command-args ["npx" "shadow-cljs" "release" "client"]})]
    (when-not (zero? exit)
      (throw (ex-info "client build failed" {:exit exit})))))

(defn uber [_]
  (client nil)
  (b/delete {:path "target"})
  (b/copy-dir {:src-dirs (:paths (basis)) :target-dir class-dir})
  ;; AOT only the entry point: `-main` has to exist as a class for `java -jar`
  ;; to find it, and everything it needs is pulled in transitively.
  (b/compile-clj {:basis (basis) :ns-compile [main-ns] :class-dir class-dir})
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis (basis)
           :main main-ns})
  (println "built" uber-file))
