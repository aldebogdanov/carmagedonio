(ns carmageddon.shared.overlay-test
  "The overlay is the only thing about a world that is not recomputable, so it
  is the only thing whose loss cannot be undone. These check the three
  properties that matter: it accumulates, it survives a round trip, and merging
  two of them never loses anything."
  (:require [clojure.test :refer [deftest is testing]]
            [carmageddon.shared.overlay :as ov]))

(def seed 20260823)

(deftest an-empty-overlay-says-nothing-is-gone
  (let [o (ov/empty-overlay seed)]
    (is (= #{} (ov/destroyed o [0 0] :props)))
    (is (not (ov/destroyed? o [0 0] :props 3)))
    (is (= 0 (ov/count-destroyed o :peds)))
    (is (ov/valid? o))))

(deftest destruction-accumulates
  (let [o (-> (ov/empty-overlay seed)
              (ov/record [0 0] :props 3)
              (ov/record [0 0] :props 7)
              (ov/record [0 0] :peds 1)
              (ov/record [1 -2] :cars 0))]
    (is (= #{3 7} (ov/destroyed o [0 0] :props)))
    (is (ov/destroyed? o [0 0] :peds 1))
    (is (not (ov/destroyed? o [0 0] :peds 2)))
    (is (= 2 (ov/count-destroyed o :props)))
    (is (= 1 (ov/count-destroyed o :cars)))
    (testing "recording the same thing twice is not two things"
      (is (= o (ov/record o [0 0] :props 3))))))

(deftest chunks-that-lost-nothing-are-not-saved
  (let [o (-> (ov/empty-overlay seed)
              (ov/record [4 4] :props 1)
              (assoc-in [:chunks [9 9] :props] #{})
              (assoc-in [:chunks [8 8]] {}))]
    (is (= 3 (count (:chunks o))))
    (is (= 1 (count (:chunks (ov/prune o))))
        "an empty chunk entry is not a record of anything")))

(deftest a-saved-overlay-reads-back
  (let [o (-> (ov/empty-overlay seed :outbreak)
              (ov/record [0 0] :props 3)
              (ov/record [-7 12] :peds 5)
              (ov/visit [0 0])
              (ov/set-vehicle {:pos [1.0 2.0 3.0] :damage 0.4})
              (ov/set-tally {:peds 2 :props 9 :cars 1 :wrecks 0}))
        back (ov/read-edn (ov/->edn o))]
    (is (some? back) "it should read at all")
    (is (= (:seed o) (:seed back)))
    (is (= :outbreak (:mode back)))
    (is (= #{3} (ov/destroyed back [0 0] :props)))
    (is (= #{5} (ov/destroyed back [-7 12] :peds))
        "negative chunk coordinates survive being a map key")
    (is (= (:vehicle o) (:vehicle back)))
    (is (= (:tally o) (:tally back)))
    (testing "sets come back as sets, so `contains?` still means what it means"
      (is (ov/destroyed? back [0 0] :props 3)))))

(deftest nonsense-is-not-an-overlay
  (testing "a save from an older version is a thing to start fresh from, not an
            error to interrupt somebody with"
    (is (nil? (ov/read-edn "not edn at all {{{")))
    (is (nil? (ov/read-edn "42")))
    (is (nil? (ov/read-edn (pr-str {:version 999 :seed 1 :chunks {}}))))
    (is (nil? (ov/read-edn (pr-str {:version ov/version :seed "no" :chunks {}}))))))

(deftest merging-never-loses-anything
  (let [a (-> (ov/empty-overlay seed)
              (ov/record [0 0] :props 1)
              (ov/record [0 0] :peds 4)
              (ov/visit [0 0]))
        b (-> (ov/empty-overlay seed)
              (ov/record [0 0] :props 2)
              (ov/record [5 5] :cars 0)
              (ov/visit [5 5])
              (ov/set-vehicle {:damage 0.9}))
        m (ov/merge-overlays a b)]
    (testing "destruction is a union: nothing can be un-smashed"
      (is (= #{1 2} (ov/destroyed m [0 0] :props)))
      (is (= #{4} (ov/destroyed m [0 0] :peds)))
      (is (= #{0} (ov/destroyed m [5 5] :cars))))
    (is (= #{[0 0] [5 5]} (:visited m)))
    (testing "single-valued things take the later one"
      (is (= {:damage 0.9} (:vehicle m))))))

(deftest overlays-for-different-worlds-do-not-merge
  (is (thrown? #?(:clj Exception :cljs :default)
               (ov/merge-overlays (ov/empty-overlay 1) (ov/empty-overlay 2)))))

(deftest the-record-of-a-long-run-stays-small
  (testing "this is the whole argument for holding it in memory: a run that
            smashes five hundred things across fifty chunks still fits in a few
            kilobytes, which a world never would"
    (let [o (reduce (fn [acc i]
                      (ov/record acc [(mod i 50) (- (mod i 7) 3)]
                                 (nth [:props :peds :cars] (mod i 3))
                                 (quot i 3)))
                    (ov/empty-overlay seed)
                    (range 500))
          bytes (:bytes (ov/stats o))]
      (is (= 500 (+ (ov/count-destroyed o :props)
                    (ov/count-destroyed o :peds)
                    (ov/count-destroyed o :cars))))
      (is (< bytes 12000) (str "overlay was " bytes " bytes")))))
