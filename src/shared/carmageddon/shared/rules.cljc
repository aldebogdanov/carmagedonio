(ns carmageddon.shared.rules
  "What a run is worth, and what makes one plausible.

  Shared rather than client-side because the server has to be able to *recompute*
  a submitted result instead of believing it. A score that only the client knows
  how to derive is a score the server can only accept on trust, which is the
  whole problem M6 has to avoid.

  This is the natural home for rules generally: the client uses it to run the
  game, the server uses it to check the client's arithmetic, and neither has a
  private copy that could drift."
  (:require [carmageddon.shared.constants :as k]))

(def scoring
  "Points, and seconds bought. Pedestrians are worth real time; scenery is worth
  almost none, so smashing crates cannot substitute for playing."
  {:ped   {:points 230 :seconds 3.0}
   :prop  {:points 25  :seconds 0.4}
   :wreck {:points 900 :seconds 12.0}})

(def start-seconds 90.0)

;; UNPLAYTESTED. An AI hunting pedestrians manages roughly one kill every eight
;; seconds, which puts 40 out of reach; a human aiming at a crowd should be much
;; quicker. This wants a real session to settle, not more arithmetic.
(def target-kills 25)

(defn score-for
  "The only correct score for a given tally. Both sides derive it from here."
  [{:keys [peds props wrecks] :or {peds 0 props 0 wrecks 0}}]
  (+ (* peds   (get-in scoring [:ped :points]))
     (* props  (get-in scoring [:prop :points]))
     (* wrecks (get-in scoring [:wreck :points]))))

(defn seconds-earned [{:keys [peds props wrecks] :or {peds 0 props 0 wrecks 0}}]
  (+ (* peds   (get-in scoring [:ped :seconds]))
     (* props  (get-in scoring [:prop :seconds]))
     (* wrecks (get-in scoring [:wreck :seconds]))))

(defn won? [{:keys [peds] :or {peds 0}}] (>= peds target-kills))

(defn max-elapsed
  "The longest a run with this tally could legitimately have lasted: the clock
  it started with, plus every second it earned."
  [tally]
  (+ start-seconds (seconds-earned tally)))

(defn verify
  "Check a submitted run against the rules. Returns nil if it is consistent, or
  a vector of problems.

  Deliberately about *arithmetic and time*, not physics. The server cannot
  reproduce the client's simulation -- that is a settled architectural decision
  -- so it checks the things it can: that the score matches the tally, that the
  outcome matches the target, and that the run did not last longer than the
  clock could possibly have allowed. Those catch a client that edits its score
  without also faking a coherent run around it."
  [{:keys [score peds props wrecks elapsed state] :as run}]
  (let [tally    (select-keys run [:peds :props :wrecks])
        expected (score-for tally)
        cap      (max-elapsed tally)
        problems
        (cond-> []
          (not= score expected)
          (conj {:field :score :expected expected :got score})

          (and (= state :won) (not (won? tally)))
          (conj {:field :state :problem :claimed-win-below-target
                 :target target-kills :got (or peds 0)})

          (and (= state :lost) (won? tally))
          (conj {:field :state :problem :claimed-loss-at-or-above-target})

          (neg? (or elapsed 0))
          (conj {:field :elapsed :problem :negative})

          ;; A tick of slack: the clock advances in fixed steps and the last one
          ;; can carry a run a fraction past the theoretical cap.
          (> (or elapsed 0) (+ cap k/dt))
          (conj {:field :elapsed :problem :longer-than-clock-allowed
                 :cap cap :got elapsed})

          (some neg? (remove nil? [peds props wrecks]))
          (conj {:field :tally :problem :negative}))]
    (when (seq problems) problems)))
