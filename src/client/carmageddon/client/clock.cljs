(ns carmageddon.client.clock
  "Fixed-timestep driver.

  The sim advances in whole ticks of exactly `k/dt`; the display draws whenever
  the browser lets it and interpolates the remainder. This separation is not a
  nicety -- variable-dt physics is non-reproducible, makes collision response
  frame-rate dependent, and cannot be reconciled against a server. Getting it
  wrong here would be expensive to unpick later, so it is the first thing built."
  (:require [carmageddon.shared.constants :as k]))

(defn start!
  "Run the loop. `on-tick` gets (tick dt), `on-frame` gets alpha in [0,1).
  Returns a stop fn.

  `schedule`/`cancel` default to requestAnimationFrame but are injectable: a
  headless test (or a browser tab that has been backgrounded, where rAF is
  suspended) can drive the loop from a timer instead. `schedule` must call its
  callback with a monotonic millisecond timestamp."
  [{:keys [on-tick on-frame on-stats schedule cancel]
    :or   {schedule #(js/requestAnimationFrame %)
           cancel   #(js/cancelAnimationFrame %)}}]
  (let [acc      (volatile! 0.0)
        tick     (volatile! 0)
        last-ms  (volatile! nil)
        raf      (volatile! nil)
        running  (volatile! true)
        frames   (volatile! 0)
        window0  (volatile! 0.0)]
    (letfn [(frame [now]
              (when @running
                (vreset! raf (schedule frame))
                (let [prev    @last-ms
                      elapsed (if prev
                                (min (/ (- now prev) 1000.0) k/max-frame-seconds)
                                0.0)]
                  (vreset! last-ms now)
                  (vswap! acc + elapsed)
                  ;; Consume whole ticks, capped so a slow frame cannot make the
                  ;; next frame slower still.
                  (loop [steps 0]
                    (when (and (>= @acc k/dt) (< steps k/max-steps-per-frame))
                      (on-tick @tick k/dt)
                      (vswap! tick inc)
                      (vswap! acc - k/dt)
                      (recur (inc steps))))
                  ;; Hit the cap and still behind: drop the debt. The world runs
                  ;; briefly in slow motion, which is survivable; a spiral is not.
                  (when (>= @acc k/dt)
                    (vreset! acc 0.0))
                  (on-frame (/ @acc k/dt))
                  (when on-stats
                    (vswap! frames inc)
                    ;; Start the stats window on the first frame, not at zero --
                    ;; otherwise the first report divides by the whole page
                    ;; uptime and reports ~0 fps.
                    (when (zero? @window0) (vreset! window0 now))
                    (when (>= (- now @window0) 500.0)
                      (on-stats {:fps  (/ (* 1000.0 @frames) (- now @window0))
                                 :tick @tick})
                      (vreset! frames 0)
                      (vreset! window0 now))))))]
      (vreset! raf (schedule frame))
      (fn stop! []
        (vreset! running false)
        (when-let [h @raf] (cancel h))))))
