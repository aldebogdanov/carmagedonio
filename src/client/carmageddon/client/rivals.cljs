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

;; --- how a rival fights -----------------------------------------------------
;;
;; Driving at the player and holding the throttle down produces a car that
;; arrives, fails to kill anybody, and then grinds along the player's flank at
;; walking pace for the rest of the run. It is not threatening and it is not
;; interesting.
;;
;; So a rival runs a three-state loop instead: get room, charge through, peel
;; off, repeat. The charge is the point -- it commits, aims where the player is
;; *going* rather than where they are, and does not lift off for the corner.
;;
;;   :approach  too far to strike. Close normally, sensibly.
;;   :charge    inside striking range. Full commit at an intercept point.
;;   :peel      just struck, or ground to a halt. Drive away for a moment to
;;              make room for the next one.
(def ^:private charge-from 62.0)   ; m -- start a run from about here
(def ^:private peel-until 11.0)    ; m -- got this close, so break off
(def ^:private peel-secs 2.4)
;; Closer than this and going nowhere means jammed against the player rather
;; than driving away from them. Steering out of that does not work -- there is
;; a car in the way -- so it reverses.
(def ^:private jam-dist 9.0)
(def ^:private jam-speed 3.0)
(def ^:private charge-secs 6.0)    ; a run that has not landed by now has missed
(def ^:private lead-speed 22.0)    ; m/s assumed closing speed when aiming ahead
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
   ;; Tactical state per rival, and how long it has been in it. Two typed
   ;; arrays rather than a map: this is read and written for every rival on
   ;; every tick, and the alternative allocates.
   :mode        (js/Int8Array. n)      ; 0 approach, 1 charge, 2 peel
   :mode-t      (js/Float32Array. n)
   ;; Nobody drives like anybody else. Derived from the index rather than
   ;; drawn, so a rival behaves the same way every time you meet it.
   :nerve       (mapv #(+ 0.75 (* 0.5 (mod (* 0.6180339 (inc %)) 1.0))) (range n))
   :wrecked     (atom #{})})

(def ^:const mode-approach 0)
(def ^:const mode-charge 1)
(def ^:const mode-peel 2)

(defn count-of [{:keys [controllers]}] (count controllers))

(defn alive [{:keys [controllers wrecked]}] (- (count controllers) (count @wrecked)))

(defn- dist [ax az bx bz] (js/Math.hypot (- ax bx) (- az bz)))

(defn- advance-mode!
  "Move rival `i` between approach, charge and peel, and say which it is in.

  Transitions are on distance and on the clock, never on having actually
  connected: a rival that misses has to break off and line up again, and a
  rival that hits is in the same position as one that missed."
  [{:keys [^js mode ^js mode-t nerve]} i d dt]
  (let [m (aget mode i)
        t (+ (aget mode-t i) dt)
        n (nth nerve i)
        go (fn [m'] (aset mode i m') (aset mode-t i 0.0) m')]
    (aset mode-t i t)
    (cond
      (= m mode-peel)
      ;; Room made, or long enough spent trying. The distance test comes first
      ;; because a peel that ends on the clock while still on the player's
      ;; bumper goes straight back into a charge it has no room for.
      (if (or (> d (* 2.2 peel-until)) (> t (* n peel-secs 2.0)))
        (go mode-approach)
        m)

      (= m mode-charge)
      ;; Broken off either because the run is spent or because it is now close
      ;; enough that anything further is a shunting match.
      (if (or (> t (* n charge-secs)) (< d peel-until)) (go mode-peel) m)

      :else
      (if (< d (* n charge-from)) (go mode-charge) m))))

(defn commands
  "One `Command` per rival, in vehicle order.

  Target priority is the whole point: the player first if they are anywhere
  near, then the nearest pedestrian, then just keep driving. Without the first
  clause a rival that has caught up with the player still ignores them."
  [{:keys [controllers] :as rs} sim peds-state tick]
  (let [vs (sim/vehicles sim)
        px (sim/player-x sim)
        pz (sim/player-z sim)
        [pvx _ pvz] (sim/player-velocity sim)]
    (mapv (fn [i]
            (let [v   (nth vs (inc i))
                  [x _ z] (vehicle/chassis-position v)
                  ctl (nth controllers i)
                  d   (dist x z px pz)
                  far? (>= d hunt-radius)
                  m   (if far?
                        mode-approach
                        (advance-mode! rs i d (/ 1.0 60.0)))
                  tgt (cond
                        far?
                        (or (ai/target ctl tick (* 7 i)
                                       #(peds/nearest-alive peds-state x z))
                            (let [[fx _ fz] (vehicle/heading v)]
                              [(+ x (* 60 fx)) 0.0 (+ z (* 60 fz))]))

                        ;; Where the player will be, not where they are. A car
                        ;; aimed at a moving target's current position always
                        ;; arrives behind it, which is exactly what tailgating
                        ;; looks like.
                        (= m mode-charge)
                        (let [t (/ d lead-speed)]
                          [(+ px (* pvx t)) 0.0 (+ pz (* pvz t))])

                        ;; Away, and to one side, so the next run comes in at
                        ;; an angle rather than back down the same line.
                        (= m mode-peel)
                        (let [ax (- x px) az (- z pz)
                              len (max 1e-3 (js/Math.hypot ax az))]
                          [(+ px (* (/ ax len) 60.0) (* (/ (- az) len) 25.0))
                           0.0
                           (+ pz (* (/ az len) 60.0) (* (/ ax len) 25.0))])

                        :else [px 0.0 pz])]
              (if (and (= m mode-peel)
                       (< d jam-dist)
                       (< (js/Math.abs (vehicle/forward-speed v)) jam-speed))
                ;; Back out. Measured: without this the rival that starts the
                ;; run directly behind the player never escapes -- it sat at a
                ;; median of three metres and two km/h for forty seconds,
                ;; cycling peel and charge against the player's bumper, which
                ;; is the exact behaviour the tactics were added to end.
                (ai/->command tick {:throttle 0.0 :brake 1.0 :reverse? true
                                    :steer (if (pos? (- x px)) 1.0 -1.0)})
                (ai/->command tick
                              (ai/drive-toward ctl
                                               {:x x :z z
                                                :forward (vehicle/heading v)
                                                :speed (vehicle/forward-speed v)}
                                               tgt
                                               {:commit? (= m mode-charge)})))))
          (range (count controllers)))))

(defn modes
  "What each rival is doing, for the HUD and for tests."
  [{:keys [^js mode]}]
  (mapv #(nth [:approach :charge :peel] (aget mode %)) (range (.-length mode))))

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
        (let [v (nth vs (inc i))
              [x _ z] (vehicle/chassis-position v)
              d (dist x z px pz)
              ;; Welded to the player counts as out of contact, and is fed into
              ;; the same counter. Measured: one rival in three spent its whole
              ;; run at a median of four metres and two km/h, grinding along
              ;; the player's flank -- it could not steer out (there is a car
              ;; in the way) and could not reverse out (the player was driving
              ;; into it). Being picked up and put back sixty metres away is
              ;; the only exit, and it is the one that produces another charge.
              jammed? (and (< d jam-dist)
                           (< (js/Math.abs (vehicle/forward-speed v)) jam-speed))]
          (if (or (> d leash) jammed?)
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
               ;; The direction it is pointing, not a map bearing. Which way is
               ;; up on a map is the map's business, and the last time this
               ;; namespace had an opinion about it every arrowhead pointed
               ;; backwards.
               :fx fx :fz fz
               :out (contains? @wrecked i)
               :damage (vehicle/damage v)}))
          (range (count controllers)))))
