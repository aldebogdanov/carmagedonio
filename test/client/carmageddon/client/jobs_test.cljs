(ns carmageddon.client.jobs-test
  "Work, tested as a state machine: put the car somewhere, advance time, and
  see what the job says happened."
  (:require [cljs.test :refer-macros [deftest is testing]]
            [carmageddon.client.jobs :as jobs]))

(def seed 20260823)

(defn- seek!
  "Take and abandon jobs until `kind` comes up. The kinds are cycled from an
  order shuffled per run, so which one is offered first varies."
  [js kind x z]
  (loop [n 0]
    (when (and (< n 12) (not= kind (:kind (jobs/current js))))
      (jobs/tick! js 10.0 x z 0.0)
      (when-let [j (jobs/current js)]
        (when (not= kind (:kind j)) (jobs/abandon! js)))
      (recur (inc n))))
  (jobs/current js))

(defn- fresh
  "A job state with one job already offered, and the job. Offers happen on a
  delay, so this winds that off first."
  [x z]
  (let [js (jobs/create seed)]
    (jobs/tick! js 10.0 x z 0.0)
    [js (jobs/current js)]))

(defn- drive-to!
  "Put the car on a stop and advance one tick. Returns whatever was awarded."
  ([js stop] (drive-to! js stop 0.0))
  ([js stop damage]
   (jobs/tick! js (/ 1.0 60.0) (:x stop) (:z stop) damage)))

(deftest a-job-is-somewhere-else
  (let [[_ job] (fresh 0.0 0.0)]
    (is (some? job) "no job could be made at the origin")
    (testing "it has at least a pickup and a drop-off, and they are not the same place"
      (let [stops (:stops job)]
        (is (<= 2 (count stops)))
        (is (= (count stops) (count (distinct (map (juxt :x :z) stops)))))))
    (testing "and every stop is a real landmark with a name"
      (doseq [s (:stops job)]
        (is (keyword? (:kind s)))
        (is (string? (:label s)))))
    (testing "and no two stops read as the same instruction"
      ;; Districts a kilometre apart can both hold a stadium, and "pick up at
      ;; the stadium, drop at the stadium" is a docket nobody can follow. The
      ;; generator prefers a kind it has not used yet.
      (let [labels (map :label (:stops job))]
        (is (= (count labels) (count (distinct labels)))
            (str "stops read " (vec labels)))))
    (testing "the route is long enough to be a drive"
      ;; The whole point of a job is that it is somewhere you were not going.
      (is (> (:route job) 400.0) (str "route only " (:route job) " m")))))

(deftest a-job-is-not-done-until-you-get-there
  ;; The first version destructured the stop into `x` and `z` -- the same names
  ;; the player's position arrives under -- so it measured the stop against
  ;; itself, found zero, and completed every job on the tick it was offered.
  ;; Every other assertion in this file passed while it did.
  (let [[js job] (fresh 0.0 0.0)
        far (jobs/tick! js (/ 1.0 60.0) 0.0 0.0 0.0)]
    (is (empty? far) "a job paid out without the car going anywhere")
    (is (= 0 (:at (jobs/current js))) "and it advanced a stop while parked")
    (testing "the target is the pickup until the pickup is reached"
      (is (:pickup? (jobs/target js))))))

(deftest driving-the-route-pays
  (let [[js job] (fresh 0.0 0.0)
        stops (:stops job)]
    (testing "arriving at the pickup takes the cargo but pays nothing yet"
      (is (empty? (drive-to! js (first stops))))
      (is (= 1 (:at (jobs/current js))))
      (is (not (:pickup? (jobs/target js)))))
    (testing "and the last stop pays exactly the job's own kind"
      (let [awards (loop [i 1 out []]
                     (if (>= i (count stops))
                       out
                       (recur (inc i) (into out (drive-to! js (nth stops i))))))]
        (is (some #{(:kind job)} awards)
            (str "finished a " (:kind job) " and was paid " awards))
        (is (nil? (jobs/current js)) "the job did not clear on completion")
        (is (= 1 (:done (jobs/stats js))))))))

(deftest fragile-cargo-punishes-the-usual-driving
  ;; The one job that forbids what the rest of the game rewards. Worth testing
  ;; because it is the only rule here that watches something other than
  ;; position.
  (let [js (jobs/create seed)
        job (seek! js :fragile 0.0 0.0)
        ;; Every job turned down on the way here counts as a failure, so the
        ;; baseline is whatever it is by now.
        before (:failed (jobs/stats js))]
    (is (= :fragile (:kind job)) "never got offered a fragile job")
    (drive-to! js (first (:stops job)) 0.30)
    (is (= 1 (:at (jobs/current js))) "did not pick the cargo up")
    (testing "a scrape is survivable"
      (jobs/tick! js (/ 1.0 60.0) 0.0 0.0 0.38)
      (is (some? (jobs/current js))))
    (testing "a shunt is not"
      (jobs/tick! js (/ 1.0 60.0) 0.0 0.0 0.62)
      (is (nil? (jobs/current js)) "carried a broken load to the door")
      (is (= (inc before) (:failed (jobs/stats js)))))))

(deftest an-express-can-be-lost-by-being-slow
  (let [js (jobs/create seed)]
    (let [job (seek! js :express 0.0 0.0)
          before (:failed (jobs/stats js))]
      (is (= :express (:kind job)))
      (is (pos? (:deadline job)) "an express with no deadline is a delivery")
      (testing "the clock runs down while it is live"
        (jobs/tick! js 1.0 0.0 0.0 0.0)
        (is (< (:deadline (jobs/current js)) (:deadline job))))
      (testing "and running out loses it"
        (jobs/tick! js 10000.0 0.0 0.0 0.0)
        (jobs/tick! js (/ 1.0 60.0) 0.0 0.0 0.0)
        (is (nil? (jobs/current js)))
        (is (= (inc before) (:failed (jobs/stats js))))))))

(deftest three-in-a-row-pays-a-bonus
  (let [js (jobs/create seed)]
    (let [awards
          (loop [done 0 out []]
            (if (= 3 done)
              out
              (do (jobs/tick! js 10.0 0.0 0.0 0.0)
                  (let [job (jobs/current js)
                        got (reduce (fn [acc s] (into acc (drive-to! js s)))
                                    [] (:stops job))]
                    (recur (inc done) (into out got))))))]
      (is (= 3 (:done (jobs/stats js))))
      (is (= 1 (count (filter #{:streak} awards)))
          (str "three jobs in a row paid " (count (filter #{:streak} awards))
               " streak bonuses"))))
  (testing "and a failure in the middle resets it"
    (let [js (jobs/create seed)]
      (jobs/tick! js 10.0 0.0 0.0 0.0)
      (doseq [s (:stops (jobs/current js))] (drive-to! js s))
      (jobs/tick! js 10.0 0.0 0.0 0.0)
      (jobs/abandon! js)
      (is (zero? (:streak (jobs/stats js)))))))
