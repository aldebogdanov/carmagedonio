(ns carmageddon.client.jobs
  "Work: somewhere to be, and something to take there.

  Everything in this game up to now paid for destruction, which makes an
  enormous world into a small one -- there is no reason to leave the block you
  are in, because the next block has the same crowd in it. A job is the other
  half: a reason to be somewhere specific, chosen for you, far enough away that
  getting there is the game.

  The two halves are deliberately in tension. Hunting is close-quarters and
  ruinous to the bodywork; a parcel run is the opposite, and a fragile one
  actively forbids the driving the rest of the game rewards. Which of the two
  you are playing at any moment is a choice, and the clock is the currency both
  are priced in.

  Jobs are generated from where the player *is*, not from the seed, and that is
  a decision rather than an oversight. A seeded job chain would be the same for
  everyone in a room, which sounds better than it is: it would also be the same
  four kilometres away from whoever happened to spawn on the wrong side of the
  map. Nothing here needs to be shared -- a run's score is its own -- and the
  server checks the tally, not the itinerary."
  (:require [carmageddon.shared.rules :as rules]
            [carmageddon.shared.worldgen :as worldgen]))

;; How close counts as arrived. A landmark occupies a whole lattice cell, so
;; this is roughly "on it" rather than "near it" -- and it has to be generous
;; enough to hit at speed, because a delivery you have to three-point-turn into
;; is a delivery nobody enjoys.
(def ^:private arrive 42.0)

;; Districts to search out from the player's own. Each is a kilometre, so this
;; reaches about three and a half.
(def ^:private search 4)

(def ^:private offer-gap 3.0)      ; seconds between finishing one and being given the next

(def kinds
  "The four kinds of work, and what shape each one is.

  `band` is how far apart the stops are put, in metres, and it is the only
  thing that prices effort: the payout in `rules/scoring` is fixed per kind so
  the server can recompute it, so the way an express comes to be worth more
  than a delivery is that it is further, not that it pays more per metre.

  `stops` is how many places you have to be, counting the pickup."
  {:delivery {:label "DELIVERY" :stops 2 :band [600.0 1500.0]
              :blurb "take it there"}
   :express  {:label "EXPRESS"  :stops 2 :band [900.0 2200.0]
              :blurb "against the clock"}
   :fragile  {:label "FRAGILE"  :stops 2 :band [700.0 1700.0]
              :blurb "do not break it"}
   :tour     {:label "ROUND"    :stops 3 :band [500.0 1300.0]
              :blurb "three drops"}})

;; What is in the box. Pure flavour, and worth the two hundred bytes: "deliver
;; the package" is an errand and "deliver twelve dozen eggs" is a story about
;; what happens when you do not.
(def ^:private cargo
  {:delivery ["a crate of fireworks" "unmarked bills" "a grandfather clock"
              "a prize marrow" "one hundred balloons" "somebody's washing"
              "a stolen garden gnome" "a wardrobe" "a very large mirror"
              "the mayor's spare suit" "a vending machine" "an anvil"]
   :express  ["blood plasma" "a heart in a cool box" "court documents"
              "the only copy" "a pizza, going cold" "an antidote"
              "the last flight's luggage" "a replacement part"]
   :fragile  ["twelve dozen eggs" "a wedding cake" "a tank of goldfish"
              "an urn, occupied" "a chandelier" "a sheet of plate glass"
              "a violin worth more than the car" "wet cement, setting"]
   :tour     ["the morning post" "three deliveries" "a paper round"
              "the laundry" "a case of samples" "the collection tins"]})

;; A fragile job survives this much damage taken while carrying, and no more.
;; Two-tenths is a proper shunt, not a scrape: the rule has to be strict enough
;; to change how the car is driven and loose enough to survive a kerb.
(def ^:private fragile-allowance 0.20)

;; Consecutive completions that earn the bonus.
(def ^:private streak-length 3)

(defn- dist [ax az bx bz] (js/Math.hypot (- ax bx) (- az bz)))

(defn- landmarks-around
  "Every landmark in the districts around a point, nearest first."
  [seed x z]
  (let [[cx cz] (worldgen/chunk-of x z)
        [dx dz] (worldgen/district-of cx cz)]
    (sort-by :away
             (for [ddx (range (- search) (inc search))
                   ddz (range (- search) (inc search))
                   :let [lm (worldgen/landmark seed (+ dx ddx) (+ dz ddz))]
                   :when lm]
               (assoc lm :away (dist x z (:x lm) (:z lm)))))))

(defn- stop-of [lm]
  {:x (:x lm) :z (:z lm) :kind (:kind lm)
   :label (or (worldgen/landmark-labels (:kind lm)) (name (:kind lm)))})

(defn- pick-chain
  "`n` landmarks, each `lo`..`hi` metres from the last, starting near (x, z).

  Greedy rather than exhaustive: take the nearest sensible pickup, then the
  nearest sensible next stop from there, and so on. A search that tried to
  optimise the whole route would sometimes find nothing at all, and a job
  offered late is worse than a job that is slightly too short."
  [seed x z n [lo hi]]
  (loop [chain []
         [fx fz] [x z]
         ;; The pickup wants to be near enough to start on and far enough to
         ;; be a drive. Every stop after it uses the kind's own band.
         [blo bhi] [180.0 (* 0.8 hi)]]
    (if (= n (count chain))
      chain
      (let [used (set (map (juxt :x :z) chain))
            kinds-used (set (map :kind chain))
            near (->> (landmarks-around seed fx fz)
                      (remove #(used [(:x %) (:z %)]))
                      (filter #(<= blo (:away %) bhi)))
            ;; Prefer a stop of a kind not already on the docket. Two
            ;; different stadiums a kilometre apart are two different places
            ;; and one instruction: "pick up at the stadium, drop at the
            ;; stadium" is a job nobody can follow. Only a preference -- out in
            ;; open country there may be nothing else within range, and a
            ;; confusing job beats no job.
            fresh (remove #(kinds-used (:kind %)) near)]
        (when-let [lm (first (concat fresh near))]
          (recur (conj chain (stop-of lm)) [(:x lm) (:z lm)] [lo hi]))))))

(defn- route-length
  "How far the whole itinerary is, starting from where the player stands."
  [x z stops]
  (first (reduce (fn [[total ax az] {:keys [x z]}]
                   [(+ total (dist ax az x z)) x z])
                 [0.0 x z]
                 stops)))

(defn- offer
  "A new job starting from (x, z), or nil if the world around here has nothing
  far enough away to make one out of."
  [{:keys [seed n order] :as _state} x z]
  ;; Cycled rather than drawn at random. Four kinds and a fair coin gives three
  ;; deliveries in a row often enough to be the thing a player remembers; the
  ;; order is shuffled once per run, so it is varied without being streaky.
  (let [kind (nth order (mod n (count order)))
        {:keys [stops band]} (kinds kind)]
    (when-let [chain (pick-chain seed x z stops band)]
      (let [;; The whole route, so an express deadline is priced on the drive
            ;; rather than on the last leg of it.
            total (route-length x z chain)]
        {:kind kind
         :id n
         :cargo (rand-nth (cargo kind))
         :stops (vec chain)
         :at 0
         ;; Eighteen metres a second is what a city drive actually averages
         ;; once junctions, traffic and getting it wrong are counted; the
         ;; constant is the time it takes to read the map and set off.
         :deadline (when (= :express kind) (+ 25.0 (/ total 18.0)))
         :damage0 nil
         :route total}))))

(defn create
  "`on-event` is called with [what job] as things happen: :offered, :picked-up,
  :done, :failed."
  ([seed] (create seed nil))
  ([seed on-event]
   (atom {:seed seed
          :on-event on-event
          :order (shuffle (keys kinds))
          :job nil
          :n 0
          :wait offer-gap
          :streak 0
          :done 0
          :failed 0})))

(defn current [js] (:job @js))

(defn target
  "Where the player has to be next, or nil: `{:x :z :label :kind :pickup?}`."
  [js]
  (when-let [{:keys [stops at]} (:job @js)]
    (assoc (nth stops at) :pickup? (zero? at))))

(defn- fire! [js what job]
  (when-let [f (:on-event @js)] (f what job)))

(defn- fail! [js job why]
  (swap! js #(-> % (assoc :job nil :wait offer-gap :streak 0)
                 (update :failed inc)))
  (fire! js :failed (assoc job :why why)))

(defn abandon!
  "Give the current job up. Breaks the streak, same as failing it: the point of
  a streak is that it is hard to keep."
  [js]
  (when-let [job (:job @js)]
    (fail! js job :abandoned)))

(defn tick!
  "Advance work by `dt`. Returns a vector of `rules/scoring` keys to award --
  the caller does the scoring, because `game/award!` is the only thing allowed
  to decide what anything is worth.

  `damage` is the player's damage, 0 to 1, which is what a fragile job is
  watching."
  [js dt x z damage]
  (let [{:keys [job wait]} @js]
    (cond
      ;; Nothing on. Count down to the next offer.
      (nil? job)
      (do (swap! js update :wait - dt)
          (when (<= (:wait @js) 0.0)
            (if-let [j (offer @js x z)]
              (do (swap! js #(-> % (assoc :job j) (update :n inc)))
                  (fire! js :offered j))
              ;; Nowhere to go from here -- try again shortly rather than
              ;; never. It happens in the middle of open country, and driving
              ;; a few hundred metres fixes it.
              (swap! js assoc :wait 4.0)))
          [])

      :else
      (let [{:keys [kind stops at deadline damage0]} job
            stop (nth stops at)
            ;; `x` and `z` are the *player*. The first version destructured the
            ;; stop into the same names, so this measured the stop against
            ;; itself, found zero, and delivered every job the instant it was
            ;; offered.
            here? (< (dist x z (:x stop) (:z stop)) arrive)]
        (cond
          ;; Out of time on an express.
          (and deadline (<= deadline 0.0))
          (do (fail! js job :late) [])

          ;; Broke it.
          (and (= :fragile kind) damage0
               (> (- damage damage0) fragile-allowance))
          (do (fail! js job :broken) [])

          :else
          (do
            (when deadline (swap! js update-in [:job :deadline] - dt))
            (if-not here?
              []
              (let [last? (= (inc at) (count stops))]
                (if-not last?
                  (do (swap! js update :job
                             #(-> % (update :at inc)
                                  (assoc :damage0 (or (:damage0 %) damage))))
                      (fire! js (if (zero? at) :picked-up :dropped)
                             (:job @js))
                      [])
                  ;; Done.
                  (let [n (inc (:streak @js))
                        streak? (zero? (mod n streak-length))]
                    (swap! js #(-> % (assoc :job nil :wait offer-gap :streak n)
                                   (update :done inc)))
                    (fire! js :done job)
                    (if streak? [kind :streak] [kind])))))))))))

(defn attach!
  "The one control work needs: give up on this one. Returns a detach fn.

  Its own listener rather than a `Command` field, for the same reason the map's
  keys are: what a driver has decided about their paperwork is not part of the
  input the simulation consumes and must never reach the wire."
  [js]
  (let [on-key (fn [^js e]
                 (when (= "KeyJ" (.-code e))
                   (.preventDefault e)
                   (abandon! js)))]
    (.addEventListener js/window "keydown" on-key)
    (fn detach! [] (.removeEventListener js/window "keydown" on-key))))

(defn stats [js]
  (let [{:keys [done failed streak job]} @js]
    {:done done :failed failed :streak streak :working? (some? job)}))
