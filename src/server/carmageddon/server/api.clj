(ns carmageddon.server.api
  "HTTP surface.

  The interesting endpoint is `POST /api/runs`. The server cannot reproduce a
  client's physics -- that is settled -- so it does not pretend to. What it can
  do is check the client's arithmetic against the shared rules: that the score
  matches the tally, that a claimed win actually reached the target, and that
  the run did not last longer than the clock could have allowed. A client that
  edits its score without faking a coherent run around it is rejected here.

  That is the shape server authority takes in this game, and it is why the rules
  live in .cljc."
  (:require [carmageddon.server.session :as session]
            [carmageddon.server.store :as store]
            [carmageddon.shared.rules :as rules]
            [carmageddon.shared.wire :as wire]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [malli.core :as m]
            [malli.error :as me]
            [org.httpkit.server :as http]
            [reitit.ring :as ring]
            [ring.middleware.not-modified :as not-modified]
            [ring.util.response :as resp]))

;; --- wire ------------------------------------------------------------------

(defn- edn-response
  ([body] (edn-response 200 body))
  ([status body]
   {:status status
    :headers {"content-type" "application/edn; charset=utf-8"}
    :body (pr-str body)}))

(defn- read-body [req]
  (when-let [b (:body req)]
    (try
      (let [s (slurp b)]
        (when (seq s) (edn/read-string s)))
      (catch Exception _ ::unreadable))))

(defn- bad-request [errors] (edn-response 400 {:error :invalid :details errors}))
(defn- not-found [what] (edn-response 404 {:error :not-found :what what}))

(defn- validated
  "Parse and schema-check a request body, or return the failure response."
  [req schema f]
  (let [body (read-body req)]
    (cond
      (= ::unreadable body) (bad-request [{:body :unreadable}])
      (nil? body)           (bad-request [{:body :missing}])
      (not (m/validate schema body))
      (bad-request (me/humanize (m/explain schema body)))
      :else (f body))))

;; --- schemas ---------------------------------------------------------------

(def NewWorld
  [:map
   [:name {:optional true} :string]
   [:seed {:optional true} :int]
   ;; A mode changes how a world behaves, never how it generates: the same seed
   ;; builds the same city either way, and only what is walking about differs.
   [:mode {:optional true} [:enum :normal :outbreak]]])

(def Overrides
  "The authored part of a world -- the places a seed alone would not put there.

  Keyed by chunk, because that is the unit a client streams and therefore the
  unit it can cheaply check. This is the answer to wanting a server-held graph
  of the world: the graph itself is derivable from the seed and needs no
  storage, and what genuinely cannot be derived is the handful of places
  somebody decided should be somewhere in particular."
  [:map-of [:tuple :int :int]
   [:map
    [:landmark {:optional true} :keyword]
    [:force-district {:optional true} :keyword]
    [:name {:optional true} :string]]])

(def NewProfile
  [:map [:name [:string {:min 1 :max 40}]]])

(def NewRun
  [:map
   [:world-id :string]
   [:profile-id :string]
   [:score :int]
   [:peds :int]
   [:props :int]
   [:cars {:optional true} :int]
   [:wrecks :int]
   ;; Optional so a client from before coins existed still validates. The
   ;; score is recomputed from the tally either way, and a missing field
   ;; counts as zero.
   [:coins {:optional true} :int]
   [:nuggets {:optional true} :int]
   [:elapsed number?]
   [:state [:enum :won :lost :running]]])

;; --- handlers --------------------------------------------------------------

(defn- random-seed []
  ;; int32 range: worldgen's PRNG is 32-bit, so a seed outside it would be
  ;; truncated and two "different" worlds could turn out identical.
  (- (rand-int 2147483647) 1073741824))

;; --- multiplayer socket -----------------------------------------------------

(defn- send-frame! [ch ^bytes frame]
  ;; http-kit takes String, byte[] or InputStream -- a ByteBuffer throws.
  (http/send! ch frame))

(defn- ws-handler
  "One socket per player. Frames are binary and defined in
  `carmageddon.shared.wire`; anything unrecognised is ignored rather than
  treated as an error, so a newer client talking to an older server degrades
  instead of disconnecting."
  [st sessions]
  (fn [req]
    (let [world-id (get-in req [:params "world"] (get-in req [:path-params :world]))
          world    (store/get-world st world-id)]
      (if-not world
        {:status 404 :body "unknown world"}
        (http/as-channel
         req
         {:on-open
          (fn [ch]
            (let [p (session/join! sessions world-id ch send-frame!)]
              (send-frame! ch (wire/encode-welcome (:id p) (:seed world)))))

          :on-receive
          (fn [ch data]
            (let [ba (cond
                       (bytes? data) data
                       (instance? java.nio.ByteBuffer data)
                       (let [^java.nio.ByteBuffer b data
                             a (byte-array (.remaining b))]
                         (.get b a) a)
                       :else nil)]
              (when-let [msg (and ba (wire/decode ba))]
                (case (:type msg)
                  :state (session/handle-state! sessions ch msg)
                  ;; The bytes that arrived, not a re-encoding of them: see
                  ;; `session/handle-delta!`.
                  :delta (session/handle-delta! sessions ch msg ba)
                  nil))))

          :on-close
          (fn [ch _status]
            (when-let [p (session/leave! sessions ch)]
              ;; Tell the room before forgetting them, so proxies disappear
              ;; instead of freezing where they stood.
              (doseq [other (vals (:players @sessions))]
                (when (= (:world-id other) (:world-id p))
                  (send-frame! (:ch other) (wire/encode-bye (:id p)))))))})))))

(defn routes [st sessions]
  [["/"
    ;; Served, not redirected. The resource handler's answer for "/" is a 302
    ;; to /index.html built without the query string, so an invite link --
    ;; http://host/?world=w_6d562e17 -- arrived at the lobby with the room it
    ;; names thrown away. Which is the entire purpose of the link.
    {:get (fn [_]
            (-> (resp/resource-response "public/index.html")
                (resp/content-type "text/html; charset=utf-8")))}]

   ["/api/health"
    {:get (fn [_] (edn-response {:ok true :service "carmagedonio"}))}]

   ["/api/rules"
    ;; So a client can show the scoring table without hardcoding a second copy.
    {:get (fn [_] (edn-response {:scoring rules/scoring
                                 :start-seconds rules/start-seconds
                                 :target-kills rules/target-kills}))}]

   ["/api/worlds"
    {:get  (fn [_] (edn-response {:worlds (store/list-worlds st)}))
     :post (fn [req]
             (validated req NewWorld
                        (fn [body]
                          (edn-response 201
                            (store/create-world!
                             st {:name (:name body "unnamed")
                                 :seed (or (:seed body) (random-seed))
                                 :mode (:mode body :normal)
                                 :overrides {}})))))}]

   ["/api/worlds/:id"
    {:get (fn [req]
            (if-let [w (store/get-world st (get-in req [:path-params :id]))]
              (edn-response w)
              (not-found :world)))}]

   ["/api/worlds/:id/overrides"
    {:get (fn [req]
            (if-let [w (store/get-world st (get-in req [:path-params :id]))]
              (edn-response {:world-id (:id w) :overrides (:overrides w {})})
              (not-found :world)))
     :post (fn [req]
             (let [id (get-in req [:path-params :id])]
               (if-not (store/get-world st id)
                 (not-found :world)
                 (let [body (read-body req)]
                   (cond
                     (= ::unreadable body) (bad-request [{:body :unreadable}])
                     (not (m/validate Overrides body))
                     (bad-request (me/humanize (m/explain Overrides body)))
                     :else (edn-response
                            {:world-id id
                             :overrides (:overrides (store/set-overrides! st id body))}))))))}]

   ["/api/worlds/:id/leaderboard"
    {:get (fn [req]
            (let [id (get-in req [:path-params :id])]
              (if (store/get-world st id)
                (edn-response {:world-id id :runs (store/leaderboard st id 20)})
                (not-found :world))))}]

   ["/api/profiles"
    {:post (fn [req]
             (validated req NewProfile
                        (fn [body]
                          (edn-response 201 (store/create-profile! st body)))))}]

   ["/api/profiles/:id"
    {:get (fn [req]
            (if-let [p (store/get-profile st (get-in req [:path-params :id]))]
              (edn-response p)
              (not-found :profile)))}]

   ["/api/profiles/:id/runs"
    {:get (fn [req]
            (let [id (get-in req [:path-params :id])]
              (if (store/get-profile st id)
                (edn-response {:profile-id id :runs (store/runs-for-profile st id)})
                (not-found :profile))))}]

   ["/api/worlds/:id/scoreboard"
    ;; Live scores, counted by the server from the deltas it accepted.
    {:get (fn [req]
            (let [id (get-in req [:path-params :id])]
              (if (store/get-world st id)
                (edn-response {:world-id id :players (session/scoreboard sessions id)})
                (not-found :world))))}]

   ["/ws/:world" {:get (ws-handler st sessions)}]

   ["/api/runs"
    {:post
     (fn [req]
       (validated req NewRun
                  (fn [body]
                    (cond
                      (nil? (store/get-world st (:world-id body)))     (not-found :world)
                      (nil? (store/get-profile st (:profile-id body))) (not-found :profile)
                      :else
                      (if-let [problems (rules/verify body)]
                        (edn-response 422 {:error :run-rejected :problems problems})
                        (edn-response 201 (store/submit-run! st body)))))))}]])

;; --- caching ----------------------------------------------------------------
;;
;; Assets come out of the uberjar, and a jar entry has no useful modification
;; time: Ring reported `Last-Modified: Thu, 01 Jan 1970 00:00:01 GMT` on every
;; one of them, with no `Cache-Control` at all.
;;
;; That combination is worse than no caching. With neither `Cache-Control` nor
;; `Expires`, a browser falls back to heuristic freshness -- typically a tenth
;; of the age of the document. An age of fifty-six years makes that five and a
;; half, so a browser that had loaded the game once would not ask for it again
;; this decade. Deploys landed correctly on the server and changed nothing at
;; all for anyone who had already played.

(defn- build-tag
  "A short content hash of the compiled client, computed once at boot.

  Every cacheable response is validated against this, so it changes exactly
  when a new build is deployed -- which is the only moment a cached asset is
  wrong. Falls back to the boot time when there is no compiled client on the
  classpath, which is the case in a REPL and is still correct: a restart is
  the coarsest thing that can change what is served."
  []
  (or (when-let [r (io/resource "public/js/main.js")]
        (with-open [in (io/input-stream r)]
          (let [digest (java.security.MessageDigest/getInstance "SHA-256")
                buf (byte-array 65536)]
            (loop []
              (let [n (.read in buf)]
                (when (pos? n)
                  (.update digest buf 0 n)
                  (recur))))
            (subs (.toString (BigInteger. 1 (.digest digest)) 16) 0 12))))
      (str "boot-" (System/currentTimeMillis))))

(defn- cacheable?
  "Static assets only. The API answers change without the build changing, and
  the WebSocket route's response is not a response at all."
  [uri]
  (let [u (or uri "")]
    (not (or (str/starts-with? u "/api") (str/starts-with? u "/ws")))))

(defn wrap-asset-cache
  "Give static responses an honest validator and take away the dishonest one.

  `no-cache` does not mean 'do not cache' -- it means 'cache it, but ask before
  using it'. With the ETag below, that ask is a conditional request answered
  with 304 and no body, so a returning player pays one round trip rather than
  three megabytes, and gets the new build the moment there is one."
  [handler tag]
  (let [etag (str "\"" tag "\"")]
    (fn [req]
      (let [response (handler req)]
        (if (and response (= 200 (:status response)) (cacheable? (:uri req)))
          (-> response
              (update :headers dissoc "last-modified" "Last-Modified")
              (assoc-in [:headers "etag"] etag)
              (assoc-in [:headers "cache-control"] "no-cache"))
          response)))))

(defn handler
  ([st] (handler st (session/create)))
  ([st sessions]
   ;; `wrap-not-modified` sits outside, so it sees the ETag the wrapper below
   ;; has just set and can answer the conditional request with a 304.
   (not-modified/wrap-not-modified
    (wrap-asset-cache
     (ring/ring-handler
      (ring/router (routes st sessions))
      (ring/routes
       (ring/create-resource-handler {:path "/" :root "public"})
       (ring/create-default-handler)))
     (build-tag)))))
