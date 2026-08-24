(ns carmageddon.shared.prng-test
  "Golden-value tests. The numbers below were produced on the JVM and are
  asserted verbatim in CLJS too -- that cross-platform agreement is the entire
  point of the namespace, so if this file ever needs 'updating' for one runtime,
  something is broken, not stale."
  (:require [clojure.test :refer [deftest is testing]]
            [carmageddon.shared.prng :as r]))

(deftest primitives-are-32-bit
  (is (= -1 (r/i32 4294967295)))
  (is (= -2 (r/imul32 2147483647 2)) "32x32 multiply must wrap, not lose precision")
  (is (= -67153019 (r/imul32 123456789 987654321)))
  (is (= -2147483648 (r/shl32 1 31)))
  (is (= 2147483647 (r/shr32 -1 1)))
  (is (= -1 (r/rotl32 -1 7))))

(deftest known-stream
  (testing "seed 12345"
    (let [s (r/make 12345)]
      (is (= [3902314842 4204794438 1627385387 764742663
              875831667 2666085690 370433204 3975402661]
             (vec (repeatedly 8 #(r/next-u32! s)))))))
  (testing "seed 0 is not degenerate"
    (let [s (r/make 0)]
      (is (= [110658709 3509158356 4176261580 3567758056]
             (vec (repeatedly 4 #(r/next-u32! s))))))))

(deftest doubles-in-unit-interval
  (let [s (r/make 999)]
    (is (= [0.5108152783941478 0.9832550880964845
            0.056088274577632546 0.9413071239832789]
           (vec (repeatedly 4 #(r/next-double! s))))))
  (let [s (r/make 4)]
    (is (every? #(and (<= 0.0 %) (< % 1.0))
                (repeatedly 2000 #(r/next-double! s))))))

(deftest bounded-ints-stay-in-range
  (let [s (r/make 77)]
    (is (every? #(and (<= 0 %) (< % 13))
                (repeatedly 2000 #(r/next-int! s 13))))))

(deftest spatial-seeds
  (testing "stable across calls"
    (is (= 1890360800 (r/chunk-seed 42 3 -7)))
    (is (= 940498319 (r/chunk-seed 42 3 -7 1))))
  (testing "salt decorrelates streams within a chunk"
    (is (not= (r/chunk-seed 42 3 -7 0) (r/chunk-seed 42 3 -7 1))))
  (testing "neighbouring chunks do not collide"
    (let [seeds (for [x (range -4 5), z (range -4 5)] (r/chunk-seed 1 x z))]
      (is (= (count seeds) (count (distinct seeds))))))
  (testing "edge seed is canonical -- both neighbours derive the same value"
    (is (= (r/edge-seed 42 0 0 0 1) (r/edge-seed 42 0 1 0 0)))
    (is (= (r/edge-seed 42 5 3 4 3) (r/edge-seed 42 4 3 5 3)))
    (is (= 1806538267 (r/edge-seed 42 0 0 0 1))))
  (testing "different edges differ"
    (is (not= (r/edge-seed 42 0 0 0 1) (r/edge-seed 42 0 0 1 0)))))

(deftest shuffle-is-deterministic
  (let [in [:a :b :c :d :e :f]]
    (is (= [:b :d :c :e :a :f] (r/shuffled (r/make 7) in)))
    (is (= (r/shuffled (r/make 7) in) (r/shuffled (r/make 7) in)))
    (is (= (set in) (set (r/shuffled (r/make 7) in))))))
