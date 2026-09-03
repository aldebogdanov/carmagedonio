(ns carmageddon.server.api-test
  (:require [carmageddon.server.api :as api]
            [carmageddon.server.store :as store]
            [carmageddon.shared.rules :as rules]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing use-fixtures]]))

(def ^:dynamic *handler* nil)
(def ^:dynamic *store* nil)

(use-fixtures :each
  (fn [t]
    (let [st (store/in-memory)]
      (binding [*store* st, *handler* (api/handler st)]
        (t)))))

(defn- req
  ([method uri] (req method uri nil))
  ([method uri body]
   (let [r (*handler* (cond-> {:request-method method :uri uri}
                        body (assoc :body (java.io.ByteArrayInputStream.
                                           (.getBytes (pr-str body) "UTF-8")))))]
     (update r :body #(when % (edn/read-string %))))))

(defn- new-world []   (:body (req :post "/api/worlds" {:name "test" :seed 4242})))
(defn- new-profile [] (:body (req :post "/api/profiles" {:name "sasha"})))

(deftest health-and-rules
  (is (= 200 (:status (req :get "/api/health"))))
  (testing "the scoring table is served so clients need not hardcode a copy"
    (let [{:keys [status body]} (req :get "/api/rules")]
      (is (= 200 status))
      (is (= rules/scoring (:scoring body)))
      (is (= rules/target-kills (:target-kills body))))))

(deftest worlds-round-trip
  (let [{:keys [status body]} (req :post "/api/worlds" {:name "hello" :seed 99})]
    (is (= 201 status))
    (is (= 99 (:seed body)))
    (is (string? (:id body)))
    (is (= body (:body (req :get (str "/api/worlds/" (:id body))))))
    (is (= 1 (count (:worlds (:body (req :get "/api/worlds"))))))))

(deftest world-without-a-seed-gets-one-in-int32-range
  (testing "worldgen's PRNG is 32-bit; a larger seed would silently truncate"
    (dotimes [_ 25]
      (let [seed (:seed (:body (req :post "/api/worlds" {})))]
        (is (<= -2147483648 seed 2147483647))))))

(deftest missing-things-are-404
  (is (= 404 (:status (req :get "/api/worlds/nope"))))
  (is (= 404 (:status (req :get "/api/profiles/nope"))))
  (is (= 404 (:status (req :get "/api/worlds/nope/leaderboard")))))

(deftest bad-bodies-are-rejected
  (is (= 400 (:status (req :post "/api/profiles" {}))))
  (is (= 400 (:status (req :post "/api/profiles" {:name ""}))))
  (is (= 400 (:status (req :post "/api/runs" {:world-id "x"})))))

(deftest a-consistent-run-is-accepted-and-recorded
  (let [w (new-world), p (new-profile)
        tally {:peds 3 :props 2 :wrecks 0}
        run (merge tally {:world-id (:id w) :profile-id (:id p)
                          :score (rules/score-for tally)
                          :elapsed 40.0 :state :lost})
        {:keys [status body]} (req :post "/api/runs" run)]
    (is (= 201 status))
    (is (string? (:id body)))
    (is (= 1 (count (:runs (:body (req :get (str "/api/profiles/" (:id p) "/runs")))))))
    (is (= [(rules/score-for tally)]
           (mapv :score (:runs (:body (req :get (str "/api/worlds/" (:id w) "/leaderboard")))))))))

(deftest an-inflated-score-is-rejected
  (testing "the server recomputes rather than believing the client"
    (let [w (new-world), p (new-profile)
          run {:world-id (:id w) :profile-id (:id p)
               :peds 1 :props 0 :wrecks 0 :score 999999
               :elapsed 20.0 :state :lost}
          {:keys [status body]} (req :post "/api/runs" run)]
      (is (= 422 status))
      (is (= :run-rejected (:error body)))
      (is (some #(= :score (:field %)) (:problems body)))))
  (testing "and nothing is stored"
    (is (empty? (:runs (store/snapshot *store*))))))

(deftest an-impossibly-long-run-is-rejected
  (let [w (new-world), p (new-profile)
        run {:world-id (:id w) :profile-id (:id p)
             :peds 0 :props 0 :wrecks 0 :score 0
             :elapsed 9999.0 :state :lost}]
    (is (= 422 (:status (req :post "/api/runs" run))))))

(deftest runs-must-belong-to-a-real-world-and-profile
  (let [p (new-profile)
        run {:world-id "w_nope" :profile-id (:id p)
             :peds 0 :props 0 :wrecks 0 :score 0 :elapsed 5.0 :state :lost}]
    (is (= 404 (:status (req :post "/api/runs" run))))))

(deftest leaderboard-is-ordered-by-score
  (let [w (new-world), p (new-profile)]
    (doseq [n [1 7 3]]
      (let [tally {:peds n :props 0 :wrecks 0}]
        (req :post "/api/runs" (merge tally {:world-id (:id w) :profile-id (:id p)
                                             :score (rules/score-for tally)
                                             :elapsed 30.0 :state :lost}))))
    (is (= [7 3 1] (mapv :peds (:runs (:body (req :get (str "/api/worlds/" (:id w) "/leaderboard")))))))))

(deftest overrides-are-the-authored-part-of-a-world
  (testing "the graph a seed cannot derive: the handful of places somebody
            decided should be somewhere in particular"
    (let [w  (new-world)
          id (:id w)]
      (is (= :normal (:mode w)) "a world defaults to a normal one")
      (is (= {} (:overrides w)))

      (testing "setting one"
        (let [r (req :post (str "/api/worlds/" id "/overrides")
                     {[12 -4] {:landmark :stadium :name "The Bowl"}})]
          (is (= 200 (:status r)))
          (is (= :stadium (get-in r [:body :overrides [12 -4] :landmark])))))

      (testing "and another does not drop the first"
        (req :post (str "/api/worlds/" id "/overrides")
             {[40 40] {:force-district :downtown}})
        (let [r (req :get (str "/api/worlds/" id "/overrides"))]
          (is (= #{[12 -4] [40 40]} (set (keys (get-in r [:body :overrides])))))))

      (testing "nonsense is rejected"
        (is (= 400 (:status (req :post (str "/api/worlds/" id "/overrides")
                                 {:not-a-chunk {:landmark :x}})))))

      (testing "and an unknown world is a 404"
        (is (= 404 (:status (req :get "/api/worlds/w_nope/overrides"))))))))

(deftest a-world-can-be-created-in-outbreak
  (let [w (:body (req :post "/api/worlds" {:name "outbreak" :mode :outbreak}))]
    (is (= :outbreak (:mode w)))
    (testing "which changes behaviour, not generation -- the seed still decides
              what gets built"
      (is (int? (:seed w))))))

(deftest static-assets-carry-an-honest-validator
  (testing "a jar entry has no modification time, and Ring reported the epoch.
            With no Cache-Control beside it a browser caches heuristically --
            a tenth of the document's age, which for 1970 is five years -- so
            a deploy changed nothing for anyone who had already played."
    (let [r (*handler* {:request-method :get :uri "/"})]
      (is (= 200 (:status r)))
      (is (nil? (get-in r [:headers "last-modified"]))
          "the epoch must not be offered as a validator")
      (is (= "no-cache" (get-in r [:headers "cache-control"])))
      (is (string? (get-in r [:headers "etag"])))))
  (testing "and the validator actually validates: a second visit is a 304"
    (let [etag (get-in (*handler* {:request-method :get :uri "/"}) [:headers "etag"])
          again (*handler* {:request-method :get :uri "/"
                            :headers {"if-none-match" etag}})]
      (is (= 304 (:status again)))))
  (testing "API answers change without the build changing, so they are not tagged"
    (let [r (*handler* {:request-method :get :uri "/api/health"})]
      (is (= 200 (:status r)))
      (is (nil? (get-in r [:headers "etag"]))))))
