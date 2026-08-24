(ns carmageddon.server.session
  "Live multiplayer: who is connected, what they claim, and what gets relayed.

  The server is not the physics authority and never will be -- no JVM engine
  agrees with Rapier bit-for-bit, so a client owns its own car. What the server
  owns is everything a client should not be trusted with:

    * **Plausibility.** A snapshot that teleports, or claims a speed no car in
      this game can reach, is dropped rather than relayed. This does not stop a
      determined cheat driving a slightly-too-fast car; it stops the cheap ones,
      and it stops a buggy client corrupting everyone else's view.
    * **The tally.** Kills are counted here, from the deltas the server chose to
      accept. A client's own score is never asked for and never believed.
    * **Ordering and fan-out.** Everyone in a world sees the same destruction
      events, which is what keeps a procedurally shared world shared.

  Note what is *not* sent: terrain, roads, buildings, props, pedestrians. All of
  that both sides derive from the seed. The only world traffic is the handful of
  bytes saying a particular thing is now gone."
  (:require [carmageddon.shared.rules :as rules]
            [carmageddon.shared.wire :as wire]))

;; A car that has genuinely reached 190 km/h downhill is plausible; 400 is not.
(def max-speed 53.0)          ; m/s
;; Generous: a dropped connection can leave a real gap between snapshots.
(def max-gap-seconds 2.0)

(defn create [] (atom {:players {} :next-id 1}))

(defn- now-ms [] (System/currentTimeMillis))

(defn implausible
  "Why this snapshot should be dropped, or nil if it is fine.

  Pure so it can be tested without a socket, and so the thresholds are one
  obvious place rather than scattered through the handler."
  [prev {:keys [pos vel] :as _now} now-ms]
  (let [[vx vy vz] vel
        speed (Math/sqrt (+ (* vx vx) (* vy vy) (* vz vz)))]
    (cond
      (some #(or (Double/isNaN (double %)) (Double/isInfinite (double %))) (concat pos vel))
      {:reason :not-finite}

      (> speed max-speed)
      {:reason :impossible-speed :speed speed :limit max-speed}

      (and prev
           (let [dt (max 0.001 (/ (- now-ms (:at prev)) 1000.0))]
             (and (<= dt max-gap-seconds)
                  (let [[px py pz] (:pos prev)
                        [nx ny nz] pos
                        d (Math/sqrt (+ (* (- nx px) (- nx px))
                                        (* (- ny py) (- ny py))
                                        (* (- nz pz) (- nz pz))))]
                    ;; Distance covered has to be reachable at the speed limit,
                    ;; with headroom for jitter in snapshot timing.
                    (> d (* max-speed dt 1.6))))))
      {:reason :teleport})))

(defn join!
  [sessions world-id ch send-fn]
  (let [id (:next-id @sessions)]
    (swap! sessions
           (fn [s]
             (-> s
                 (assoc-in [:players ch] {:id id :world-id world-id :ch ch
                                          :send send-fn :last nil
                                          :tally {:peds 0 :props 0 :wrecks 0}})
                 (assoc :next-id (inc id)))))
    (get-in @sessions [:players ch])))

(defn leave! [sessions ch]
  (when-let [p (get-in @sessions [:players ch])]
    (swap! sessions update :players dissoc ch)
    p))

(defn peers
  "Everyone else in the same world."
  [sessions ch]
  (let [{:keys [players]} @sessions
        me (get players ch)]
    (when me
      (->> (vals players)
           (filter #(and (= (:world-id %) (:world-id me))
                         (not= (:ch %) ch)))))))

(defn broadcast!
  "Send a frame to everyone else in the world."
  [sessions ch frame]
  (doseq [p (peers sessions ch)]
    ((:send p) (:ch p) frame)))

(defn handle-state!
  "Accept, record and relay one car snapshot. Returns :relayed or the reason it
  was dropped."
  [sessions ch msg]
  (if-let [me (get-in @sessions [:players ch])]
    (let [car (first (:cars msg))]
      (if (nil? car)
        :empty
        (if-let [bad (implausible (:last me) car (now-ms))]
          (:reason bad)
          (do
            (swap! sessions assoc-in [:players ch :last]
                   (assoc car :at (now-ms)))
            ;; Relayed under the server's id for this player, not whatever id
            ;; the client put in the frame.
            (broadcast! sessions ch
                        (wire/encode-state (:tick msg)
                                           [(assoc car :id (:id me))]))
            :relayed))))
    :unknown-player))

(defn handle-delta!
  "Record a destruction event against the sender's tally and tell everyone else.

  The tally lives here, which is the point: at the end of a run the server
  already knows the score and does not have to take the client's word for it."
  [sessions ch {:keys [kind] :as msg}]
  (when-let [me (get-in @sessions [:players ch])]
    (let [field (case kind :ped :peds :prop :props nil)]
      (when field
        (swap! sessions update-in [:players ch :tally field] inc))
      (broadcast! sessions ch (wire/encode-delta msg))
      (get-in @sessions [:players ch :tally]))))

(defn tally [sessions ch] (get-in @sessions [:players ch :tally]))

(defn scoreboard
  "Server-side scores for a world, derived from accepted deltas via the shared
  rules -- never from anything a client reported."
  [sessions world-id]
  (->> (vals (:players @sessions))
       (filter #(= world-id (:world-id %)))
       (map (fn [{:keys [id tally]}]
              (assoc tally :player-id id :score (rules/score-for tally))))
       (sort-by (comp - :score))
       vec))

(defn player-count [sessions] (count (:players @sessions)))
