(ns carmageddon.client.input
  "Keyboard -> tick-stamped command.

  Even in single player, nothing touches the raw keyboard state directly: the
  sim only ever consumes a `Command`. That indirection is what makes input
  buffering, replay and server reconciliation possible later without touching
  the sim, so it is worth the tiny cost now.")

(defrecord Command [tick throttle brake steer handbrake reset])

(def ^:private held (js/Set.))

(def ^:private bindings
  {"KeyW" :fwd   "ArrowUp"    :fwd
   "KeyS" :back  "ArrowDown"  :back
   "KeyA" :left  "ArrowLeft"  :left
   "KeyD" :right "ArrowRight" :right
   "Space" :handbrake
   "KeyR"  :reset})

(defn- down? [action]
  (some (fn [[code a]] (and (= a action) (.has held code))) bindings))

(defn attach!
  "Wire up listeners. Returns a detach fn."
  []
  (let [on-down (fn [e]
                  (when (contains? bindings (.-code e))
                    (.preventDefault e)
                    (.add held (.-code e))))
        on-up   (fn [e] (.delete held (.-code e)))
        on-blur (fn [_] (.clear held))]
    (.addEventListener js/window "keydown" on-down)
    (.addEventListener js/window "keyup" on-up)
    (.addEventListener js/window "blur" on-blur)
    (fn detach! []
      (.removeEventListener js/window "keydown" on-down)
      (.removeEventListener js/window "keyup" on-up)
      (.removeEventListener js/window "blur" on-blur))))

(defn sample
  "Snapshot the current keyboard into an immutable command for `tick`.

  Axes are already normalised to [-1, 1] / [0, 1] here rather than in the sim,
  so a networked or AI-driven command is indistinguishable from a human one."
  [tick]
  (->Command tick
             (if (down? :fwd) 1.0 0.0)
             (if (down? :back) 1.0 0.0)
             (+ (if (down? :left) -1.0 0.0) (if (down? :right) 1.0 0.0))
             (down? :handbrake)
             (down? :reset)))
