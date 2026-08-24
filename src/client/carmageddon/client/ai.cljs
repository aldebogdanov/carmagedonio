(ns carmageddon.client.ai
  "Steering. Produces the same `Command` a keyboard does, which is the point of
  having made input a value back in M0 -- an opponent drives through exactly the
  code path a player does, and so does a test.

  Three things separate this from naive seek-the-target steering, and all three
  came from watching the naive version fail:

    * **Slow down for the corner.** Aiming at full throttle regardless of angle
      means arriving too fast to turn, every time. Desired speed falls with how
      far off-heading the target is.
    * **Reverse when stuck.** Nose against a building at zero speed, a seeking
      controller just holds the throttle on forever. Sustained low speed while
      trying to move triggers a timed reverse.
    * **Give up on unreachable targets.** Otherwise the car grinds against
      whatever is between it and something it will never get to."
  (:require [carmageddon.client.input :as input]))

(def ^:private stuck-speed 1.8)        ; m/s below which we might be stuck
(def ^:private stuck-ticks 50)         ; ... for this long before reversing
(def ^:private reverse-ticks 40)
(def ^:private max-speed 26.0)         ; m/s the controller will ask for

(defn controller []
  (atom {:slow 0 :reversing 0 :target nil}))

(def ^:private retarget-ticks 30)

(defn target
  "Pick something to chase, but not every tick.

  Finding the nearest pedestrian is a linear scan over everything loaded, and
  doing it per driver per tick dominated the whole simulation. A target from a
  third of a second ago is indistinguishable at driving speeds. `stagger` keeps
  the drivers from all re-scanning on the same tick."
  [ctl tick stagger find-fn]
  (let [{:keys [target]} @ctl]
    (if (or (nil? target) (zero? (mod (+ tick stagger) retarget-ticks)))
      (let [t (find-fn)]
        (swap! ctl assoc :target t)
        t)
      target)))

(defn- clamp [v lo hi] (max lo (min hi v)))

(defn drive-toward
  "Return the input map that heads `state` toward `[tx _ tz]`.

  `state` is {:x :z :forward [fx _ fz] :speed}. Kept as plain numbers rather
  than a telemetry map because this runs per opponent per tick."
  [ctl {:keys [x z forward speed]} [tx _ tz]]
  (let [[fx _ fz] forward
        dx (- tx x) dz (- tz z)
        dist (js/Math.hypot dx dz)
        ;; Signed angle from where we point to where we want to be, in the
        ;; same convention the steering input uses: positive means "turn right".
        ;; Getting this backwards makes the car circle its target forever at
        ;; full lock, which is exactly what it did -- verified by measuring
        ;; whether each sign closes the distance, not by reasoning about it.
        ang (js/Math.atan2 (- (* fx dz) (* fz dx))
                           (+ (* fx dx) (* fz dz)))
        abs-ang (js/Math.abs ang)
        {:keys [slow reversing]} @ctl]
    (cond
      ;; Backing out of whatever we drove into.
      (pos? reversing)
      (do (swap! ctl update :reversing dec)
          ;; Opposite lock while backing out, so the nose swings towards the
          ;; target rather than further into whatever we hit.
          {:throttle 0.0 :brake 1.0 :steer (if (pos? ang) -1.0 1.0) :reverse? true})

      :else
      (let [_ (if (and (< (js/Math.abs speed) stuck-speed) (> dist 4.0))
                (swap! ctl update :slow inc)
                (swap! ctl assoc :slow 0))
            stuck? (> (:slow @ctl) stuck-ticks)]
        (if stuck?
          (do (swap! ctl assoc :slow 0 :reversing reverse-ticks)
              {:throttle 0.0 :brake 1.0 :steer 0.0 :reverse? true})
          ;; Ask for a speed the corner can actually be taken at. A target 90
          ;; degrees off wants walking pace; dead ahead wants everything.
          (let [want (* max-speed (clamp (- 1.0 (/ abs-ang 2.2)) 0.15 1.0))
                want (min want (+ 4.0 (* 0.9 dist)))   ; and slow down on arrival
                err  (- want speed)
                steer (clamp (* ang 1.6) -1.0 1.0)]
            {:throttle (if (pos? err) (clamp (* err 0.35) 0.0 1.0) 0.0)
             :brake    (if (neg? err) (clamp (* (- err) 0.25) 0.0 1.0) 0.0)
             :steer    steer
             ;; A hard turn while sliding is worth a stab of handbrake.
             :handbrake (and (> abs-ang 1.5) (> speed 16.0))}))))))

(defn ->command
  "Turn an input map into the Command the simulation consumes."
  [tick {:keys [throttle brake steer handbrake reverse?]}]
  (input/->Command tick
                   (if reverse? 0.0 (or throttle 0.0))
                   ;; Reverse is modelled as brake: the tyre model runs the
                   ;; wheels backwards under sustained braking from a standstill,
                   ;; so there is no separate gear to represent.
                   (if reverse? 1.0 (or brake 0.0))
                   (or steer 0.0)
                   (boolean handbrake)
                   false))
