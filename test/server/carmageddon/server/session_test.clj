(ns carmageddon.server.session-test
  "Validation is the server's whole job in a game where it cannot simulate, so
  it is tested directly rather than through a socket."
  (:require [carmageddon.server.session :as s]
            [carmageddon.shared.rules :as rules]
            [carmageddon.shared.wire :as wire]
            [clojure.test :refer [deftest is testing]]))

(defn- car [& {:keys [pos vel id] :or {pos [0.0 1.0 0.0] vel [0.0 0.0 0.0] id 1}}]
  {:id id :pos pos :quat [0.0 0.0 0.0 1.0] :vel vel :damage 0.0})

(deftest a-normal-snapshot-is-plausible
  (is (nil? (s/implausible nil (car :vel [20.0 0.0 -10.0]) 1000)))
  (testing "moving a sane distance between snapshots"
    (let [prev (assoc (car :pos [0.0 1.0 0.0]) :at 1000)]
      (is (nil? (s/implausible prev (car :pos [1.5 1.0 0.0] :vel [30.0 0 0]) 1040))))))

(deftest impossible-speed-is-rejected
  (let [{:keys [reason limit]} (s/implausible nil (car :vel [400.0 0.0 0.0]) 1000)]
    (is (= :impossible-speed reason))
    (is (= s/max-speed limit))))

(deftest teleports-are-rejected
  (testing "covering a kilometre in one snapshot interval"
    (let [prev (assoc (car :pos [0.0 1.0 0.0]) :at 1000)
          bad  (car :pos [1000.0 1.0 0.0] :vel [10.0 0 0])]
      (is (= :teleport (:reason (s/implausible prev bad 1040)))))))

(deftest a-long-gap-forgives-a-large-jump
  (testing "a reconnecting client should not be punished for the gap"
    (let [prev (assoc (car :pos [0.0 1.0 0.0]) :at 1000)
          far  (car :pos [900.0 1.0 0.0] :vel [10.0 0 0])]
      (is (nil? (s/implausible prev far (+ 1000 60000)))))))

(deftest garbage-numbers-are-rejected
  (is (= :not-finite (:reason (s/implausible nil (car :pos [##NaN 0.0 0.0]) 1000))))
  (is (= :not-finite (:reason (s/implausible nil (car :vel [##Inf 0.0 0.0]) 1000)))))

;; --- relay ------------------------------------------------------------------

(defn- fake-socket [inbox] (fn [ch frame] (swap! inbox conj [ch frame])))

(deftest snapshots-are-relayed-to-peers-only
  (let [sessions (s/create)
        inbox (atom [])
        send! (fake-socket inbox)
        a (s/join! sessions "w1" :a send!)
        b (s/join! sessions "w1" :b send!)
        c (s/join! sessions "w2" :c send!)]
    (is (= [1 2 3] [(:id a) (:id b) (:id c)]))
    (is (= :relayed (s/handle-state! sessions :a {:tick 5 :cars [(car :vel [10.0 0 0])]})))
    (testing "only the peer in the same world hears it"
      (is (= [:b] (mapv first @inbox))))
    (testing "and it carries the server's id for the sender, not the client's"
      (is (= 1 (:id (first (:cars (wire/decode (second (first @inbox))))))))) ))

(deftest an-implausible-snapshot-is-dropped-not-relayed
  (let [sessions (s/create)
        inbox (atom [])
        send! (fake-socket inbox)]
    (s/join! sessions "w1" :a send!)
    (s/join! sessions "w1" :b send!)
    (is (= :impossible-speed
           (s/handle-state! sessions :a {:tick 1 :cars [(car :vel [400.0 0 0])]})))
    (is (empty? @inbox) "nothing should reach the other player")))

(deftest leaving-removes-the-player
  (let [sessions (s/create)
        send! (fake-socket (atom []))]
    (s/join! sessions "w1" :a send!)
    (is (= 1 (s/player-count sessions)))
    (is (= 1 (:id (s/leave! sessions :a))))
    (is (zero? (s/player-count sessions)))))

;; --- authority --------------------------------------------------------------

(deftest the-server-counts-kills-itself
  (let [sessions (s/create)
        inbox (atom [])
        send! (fake-socket inbox)]
    (s/join! sessions "w1" :a send!)
    (s/join! sessions "w1" :b send!)
    (dotimes [i 3]
      (s/handle-delta! sessions :a {:cx 0 :cz 0 :kind :ped :index i}))
    (s/handle-delta! sessions :a {:cx 1 :cz 0 :kind :prop :index 9})
    (s/handle-delta! sessions :a {:cx 1 :cz 0 :kind :car :index 4})
    ;; A bridge parapet is clutter, here as on the client.
    (s/handle-delta! sessions :a {:cx 1 :cz 0 :kind :barrier :index 7})
    (testing "tally is derived from accepted deltas, never reported"
      ;; Only the counted fields are named. The tally is seeded from
      ;; `rules/tally-fields`, so asserting the whole map here would mean this
      ;; test failing every time a scoring category is added -- which is the
      ;; opposite of what the next assertion is for.
      (is (= {:peds 3 :props 2 :cars 1}
             (select-keys (s/tally sessions :a) [:peds :props :cars])))
      (is (zero? (:wrecks (s/tally sessions :a)))))
    (testing "every field the rules can score is present from the start"
      ;; Otherwise adding a category silently scores nil for everyone who
      ;; joined before the first one of them happened.
      (is (= (set (keys rules/tally-fields)) (set (keys (s/tally sessions :a))))))
    (testing "and the score comes from the same shared rules the client uses"
      (let [board (s/scoreboard sessions "w1")]
        (is (= (rules/score-for {:peds 3 :props 2 :cars 1}) (:score (first board))))
        (is (= 2 (count board)))))
    (testing "peers are told about every destruction, scored or not"
      (is (= 6 (count (filter #(= :b (first %)) @inbox)))))))

(deftest an-unknown-delta-kind-is-relayed-but-not-scored
  ;; A client newer than this process will send kinds it has never heard of.
  ;; Relaying them keeps the room consistent; scoring them would be inventing
  ;; points for something the rules do not describe.
  (let [sessions (s/create)
        inbox (atom [])
        send! (fake-socket inbox)]
    (s/join! sessions "w1" :a send!)
    (s/join! sessions "w1" :b send!)
    ;; A frame from the future: a delta whose kind byte this build has no name
    ;; for. Built by poking a real one, because that is exactly what a newer
    ;; client would put on the wire.
    (let [^bytes frame (wire/encode-delta {:cx 4 :cz -2 :kind :prop :index 7})]
      (aset-byte frame 9 (byte 99))
      (let [msg (wire/decode frame)]
        (is (nil? (:kind msg)) "this build should not recognise it")
        (s/handle-delta! sessions :a msg frame)))
    (is (= 0 (rules/score-for (s/tally sessions :a))) "and should not score it")
    (testing "but the peer still hears it, byte for byte"
      (is (= 1 (count (filter #(= :b (first %)) @inbox))))
      (is (= 99 (bit-and 0xff (aget ^bytes (second (first @inbox)) 9)))))))

(deftest scoreboard-is-per-world-and-ranked
  (let [sessions (s/create)
        send! (fake-socket (atom []))]
    (s/join! sessions "w1" :a send!)
    (s/join! sessions "w1" :b send!)
    (s/join! sessions "w2" :c send!)
    (dotimes [i 5] (s/handle-delta! sessions :b {:cx 0 :cz 0 :kind :ped :index i}))
    (dotimes [i 2] (s/handle-delta! sessions :a {:cx 0 :cz 0 :kind :ped :index i}))
    (let [board (s/scoreboard sessions "w1")]
      (is (= [5 2] (mapv :peds board)) "ranked by score")
      (is (= 2 (count board)) "w2's player is not in w1's board"))))

(deftest coins-are-tallied-and-power-ups-are-not
  (testing "a coin is a pickup in the overlay but its own kind on the wire,
            because the server tallies from these bytes and one :pickup for
            everything left it unable to tell a crate of nitro from points"
    (let [sessions (s/create)
          send! (fn [_ _])]
      (s/join! sessions "w1" :a send!)
      (s/handle-delta! sessions :a {:cx 0 :cz 0 :kind :coin :index 1})
      (s/handle-delta! sessions :a {:cx 0 :cz 0 :kind :coin :index 2})
      (s/handle-delta! sessions :a {:cx 0 :cz 0 :kind :nugget :index 3})
      ;; Holding a nitro is not a score.
      (s/handle-delta! sessions :a {:cx 0 :cz 0 :kind :pickup :index 4})
      (let [t (s/tally sessions :a)]
        (is (= 2 (:coins t)))
        (is (= 1 (:nuggets t)))
        (is (= {:peds 0 :props 0 :cars 0 :wrecks 0}
               (select-keys t [:peds :props :cars :wrecks]))))
      (testing "and the scoreboard scores them through the shared rules"
        (let [board (s/scoreboard sessions "w1")]
          (is (= (rules/score-for (s/tally sessions :a))
                 (:score (first board)))))))))
