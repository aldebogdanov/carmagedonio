(ns carmageddon.shared.rules-test
  "The rules run on both sides, so they are tested on both sides. A divergence
  here would mean the server rejecting runs the client considers legitimate."
  (:require [clojure.test :refer [deftest is testing]]
            [carmageddon.shared.rules :as rules]))

(deftest score-is-derived-not-remembered
  (is (= 0 (rules/score-for {})))
  (is (= 230 (rules/score-for {:peds 1})))
  (is (= 925 (rules/score-for {:props 1 :wrecks 1})))
  (is (= (+ (* 3 230) (* 2 25) 900)
         (rules/score-for {:peds 3 :props 2 :wrecks 1}))))

(deftest seconds-earned-tracks-the-table
  (is (= 3.0 (rules/seconds-earned {:peds 1})))
  (is (= 12.4 (rules/seconds-earned {:props 1 :wrecks 1}))))

(deftest verify-accepts-a-consistent-run
  (let [tally {:peds 3 :props 2 :wrecks 0}
        run (assoc tally :score (rules/score-for tally) :elapsed 42.0 :state :lost)]
    (is (nil? (rules/verify run)))))

(deftest verify-rejects-an-inflated-score
  (testing "the whole point of the server recomputing"
    (let [tally {:peds 1 :props 0 :wrecks 0}
          run (assoc tally :score 999999 :elapsed 10.0 :state :lost)
          problems (rules/verify run)]
      (is (some? problems))
      (is (some #(= :score (:field %)) problems)))))

(deftest verify-rejects-a-claimed-win-below-target
  (let [tally {:peds 1 :props 0 :wrecks 0}
        run (assoc tally :score (rules/score-for tally) :elapsed 10.0 :state :won)]
    (is (some #(= :state (:field %)) (rules/verify run)))))

(deftest verify-rejects-a-run-longer-than-its-clock
  (testing "you cannot play for ten minutes on a ninety second timer"
    (let [tally {:peds 0 :props 0 :wrecks 0}
          run (assoc tally :score 0 :elapsed 600.0 :state :lost)
          problems (rules/verify run)]
      (is (some #(= :longer-than-clock-allowed (:problem %)) problems))))
  (testing "but time bought by kills does extend it"
    (let [tally {:peds 20 :props 0 :wrecks 0}
          run (assoc tally :score (rules/score-for tally) :elapsed 140.0 :state :lost)]
      (is (nil? (rules/verify run))))))

(deftest verify-rejects-negative-tallies
  (let [run {:peds -5 :props 0 :wrecks 0 :score -1150 :elapsed 10.0 :state :lost}]
    (is (some #(= :tally (:field %)) (rules/verify run)))))

(deftest a-run-at-the-clock-cap-is-allowed
  (testing "the last fixed tick can carry a run a fraction past the cap"
    (let [tally {:peds 0 :props 0 :wrecks 0}
          run (assoc tally :score 0 :elapsed rules/start-seconds :state :lost)]
      (is (nil? (rules/verify run))))))
