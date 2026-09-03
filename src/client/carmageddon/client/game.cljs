(ns carmageddon.client.game
  "Scoring, the clock, and how a run ends.

  Deliberately separate from `sim`: none of this is physics, and keeping it out
  means the measurement harness can drive the vehicle without a game attached.
  It is also where the server will take over in M6 -- rules are exactly the part
  a client should not be trusted with, which is why they live behind one
  namespace with a narrow surface rather than being sprinkled through the loop."
  (:require [carmageddon.shared.constants :as k]
            [carmageddon.shared.rules :as rules]))

(def ^:private scoring rules/scoring)

(defn create []
  (atom {:remaining rules/start-seconds
         :elapsed   0.0
         :score     0
         :peds      0
         :props     0
         :cars      0
         :wrecks    0
         :coins     0
         :nuggets   0
         :state     :running
         ;; Why it ended, for the cluster to say. Not part of the submitted
         ;; run: the rules only know :won and :lost, and a wreck is a loss.
         :ending    nil}))

(defn- award! [game kind]
  (let [{:keys [points seconds]} (get scoring kind)]
    (swap! game (fn [g]
                  (-> g
                      (update :score + points)
                      (update :remaining + seconds))))))

(defn ped-killed!  [game] (award! game :ped)  (swap! game update :peds inc))
(defn car-wrecked! [game] (award! game :car)  (swap! game update :cars inc))
(defn prop-wrecked! [game] (award! game :prop) (swap! game update :props inc))
(defn opponent-wrecked! [game] (award! game :wreck) (swap! game update :wrecks inc))
(defn coin-taken!   [game] (award! game :coin)   (swap! game update :coins inc))
(defn nugget-taken! [game] (award! game :nugget) (swap! game update :nuggets inc))

(defn wrecked!
  "The player's car is finished. Ends the run.

  A car at 100% damage used to keep driving, which made the damage bar an
  instrument that measured nothing: it filled up and then the game carried on
  exactly as before."
  [game]
  (when (= :running (:state @game))
    (swap! game assoc :state :lost :ending :wreck)))

(defn tick!
  "Advance the clock by one simulation tick. Runs off the fixed timestep rather
  than wall time so a stutter cannot cost the player seconds."
  [game]
  (when (= :running (:state @game))
    (swap! game
           (fn [g]
             (let [r (- (:remaining g) k/dt)
                   g (-> g
                         (assoc :remaining (max 0.0 r))
                         (update :elapsed + k/dt))]
               (cond
                 (rules/won? g)              (assoc g :state :won :ending :target)
                 (<= r 0.0)                  (assoc g :state :lost :ending :time)
                 :else g))))))

(defn running? [game] (= :running (:state @game)))

(defn summary [game]
  (let [{:keys [remaining score peds props cars wrecks coins nuggets state
                ending elapsed]} @game]
    {:state state
     :ending ending
     :remaining remaining
     :elapsed elapsed
     :score score
     :peds peds
     :target rules/target-kills
     :props props
     :cars cars
     :wrecks wrecks
     :coins coins
     :nuggets nuggets}))

(defn result
  "The run as it will be submitted. Score is recomputed from the tally rather
  than read out of the running total, so what is sent is what the rules say --
  the server checks exactly this."
  [game]
  (let [{:keys [elapsed state] :as g} @game
        tally (select-keys g (keys rules/tally-fields))]
    (assoc tally
           :score (rules/score-for tally)
           :elapsed elapsed
           :state state)))
