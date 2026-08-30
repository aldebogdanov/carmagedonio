(ns carmageddon.client.rivals
  "The opposition: what they chase, and what happens when they lose you.

  Steering itself is `ai`; this is the policy layer above it. It exists because
  of a bug that only shows up a kilometre from the spawn: the rivals were told
  to hunt the nearest pedestrian *to themselves*, so once the player drove away
  they simply stayed where they were, circling the crowd they started next to.
  From the player's side the field silently emptied.

  Two rules fix it, and both are the standard answer in open-world racing:

    * **Hunt the player.** Inside `hunt-radius` a rival abandons the crowd and
      comes for you. That is what makes them opposition rather than scenery.
    * **Leash.** A rival that has been out of contact for a few seconds is
      picked up and put back down on a road near the player. Chasing across a
      streamed world does not work -- the chunks it would have to drive through
      are not loaded, so it would be steering against terrain that does not
      exist."
  (:require [carmageddon.client.ai :as ai]
            [carmageddon.client.peds :as peds]
            [carmageddon.client.sim :as sim]
            [carmageddon.client.vehicle :as vehicle]
            [carmageddon.shared.worldgen :as worldgen]))

(def ^:private hunt-radius 130.0)   ; m -- inside this, forget the crowd
(def ^:private leash 300.0)         ; m -- beyond this a rival is out of the game
(def ^:private leash-ticks 180)     ; ... for three seconds before it is moved
(def ^:private respawn-min 70.0)    ; m -- far enough to be off-camera behind
(def ^:private respawn-max 120.0)   ; m -- and inside the collider radius
(def ^:private wreck-damage 0.9)    ; past this a rival is out and worth points

(defn create
  "One controller per rival, plus the bookkeeping for leashing and wrecks."
  [seed n]
  {:seed        seed
   :controllers (vec (repeatedly n ai/controller))
   :lost        (js/Int32Array. n)     ; ticks each rival has been out of contact
   :wrecked     (atom #{})})

(defn count-of [{:keys [controllers]}] (count controllers))

(defn alive [{:keys [controllers wrecked]}] (- (count controllers) (count @wrecked)))

(defn- dist [ax az bx bz] (js/Math.hypot (- ax bx) (- az bz)))

(defn commands
  "One `Command` per rival, in vehicle order.

  Target priority is the whole point: the player first if they are anywhere
  near, then the nearest pedestrian, then just keep driving. Without the first
  clause a rival that has caught up with the player still ignores them."
  [{:keys [controllers]} sim peds-state tick]
  (let [vs (sim/vehicles sim)
        px (sim/player-x sim)
        pz (sim/player-z sim)]
    (mapv (fn [i]
            (let [v   (nth vs (inc i))
                  [x _ z] (vehicle/chassis-position v)
                  ctl (nth controllers i)
                  tgt (cond
                        (< (dist x z px pz) hunt-radius) [px 0.0 pz]
                        :else
                        (or (ai/target ctl tick (* 7 i)
                                       #(peds/nearest-alive peds-state x z))
                            (let [[fx _ fz] (vehicle/heading v)]
                              [(+ x (* 60 fx)) 0.0 (+ z (* 60 fz))])))]
              (ai/->command tick
                            (ai/drive-toward ctl
                                             {:x x :z z
                                              :forward (vehicle/heading v)
                                              :speed (vehicle/forward-speed v)}
                                             tgt))))
          (range (count controllers)))))

(defn- respawn-spot
  "Somewhere on a road `respawn-min`..`respawn-max` behind the player.

  Falls back to bare terrain if the search finds nothing close enough. In open
  country the nearest street can be most of a kilometre away, and dropping a car
  there would only put it straight back outside the leash -- and quite possibly
  outside the loaded colliders, where it would fall through the world."
  [seed px pz heading]
  (let [;; Behind, give or take a quadrant, so a leashed rival does not
        ;; materialise in front of the windscreen.
        bearing (+ heading js/Math.PI (* 1.6 (- (js/Math.random) 0.5)))
        d  (+ respawn-min (* (- respawn-max respawn-min) (js/Math.random)))
        tx (+ px (* d (js/Math.sin bearing)))
        tz (+ pz (* d (js/Math.cos bearing)))
        on-road (worldgen/road-point-near seed tx tz 2)]
    (if (and on-road
             (let [[rx _ rz] (:pos on-road)]
               (< (dist rx rz px pz) (* 1.6 respawn-max))))
      (let [[dx dz] (:dir on-road)
            [rx _ rz] (:pos on-road)
            ;; A street has two directions and the generator picks one
            ;; arbitrarily. Face down whichever of them heads towards the
            ;; player, so the rival sets off after them instead of away.
            sgn (if (neg? (+ (* dx (- px rx)) (* dz (- pz rz)))) -1.0 1.0)]
        {:pos (:pos on-road) :yaw (js/Math.atan2 (* (- sgn) dx) (* (- sgn) dz))})
      {:pos [tx (+ 1.4 (worldgen/height-at seed tx tz)) tz]
       :yaw (+ bearing js/Math.PI)})))

(defn leash!
  "Pick up rivals that have lost touch and put them back in the game.

  Deliberately not instant: a rival briefly beyond the leash because the player
  is doing 200 km/h down an expressway will catch up on its own, and teleporting
  it every time the gap opened would be visible from the mirror."
  [{:keys [seed controllers ^js lost wrecked]} sim heading]
  (let [px (sim/player-x sim)
        pz (sim/player-z sim)
        vs (sim/vehicles sim)]
    (dotimes [i (count controllers)]
      (when-not (contains? @wrecked i)
        (let [[x _ z] (vehicle/chassis-position (nth vs (inc i)))]
          (if (> (dist x z px pz) leash)
            (aset lost i (inc (aget lost i)))
            (aset lost i 0))
          (when (> (aget lost i) leash-ticks)
            (aset lost i 0)
            (let [{:keys [pos yaw]} (respawn-spot seed px pz heading)]
              (sim/place-vehicle! sim (inc i) pos yaw))))))))

(defn score-wrecks!
  "A rival past the damage threshold is out of the race and worth points, once.
  Returns how many newly wrecked, so the caller can score them."
  [{:keys [controllers wrecked]} sim]
  (let [vs (sim/vehicles sim)]
    (reduce (fn [n i]
              (if (and (not (contains? @wrecked i))
                       (> (vehicle/damage (nth vs (inc i))) wreck-damage))
                (do (swap! wrecked conj i) (inc n))
                n))
            0
            (range (count controllers)))))

(defn blips
  "Where every rival is, for the map. `:out` ones are wrecked and no longer
  chasing anyone, but they are still worth drawing -- a wreck is a landmark."
  [{:keys [controllers wrecked]} sim]
  (let [vs (sim/vehicles sim)]
    (mapv (fn [i]
            (let [v (nth vs (inc i))
                  [x _ z] (vehicle/chassis-position v)
                  [fx _ fz] (vehicle/heading v)]
              {:x x :z z
               :heading (js/Math.atan2 fx fz)
               :out (contains? @wrecked i)
               :damage (vehicle/damage v)}))
          (range (count controllers)))))
