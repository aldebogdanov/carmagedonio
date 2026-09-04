(ns carmageddon.client.input
  "Keyboard -> tick-stamped command.

  Even in single player, nothing touches the raw keyboard state directly: the
  sim only ever consumes a `Command`. That indirection is what makes input
  buffering, replay and server reconciliation possible later without touching
  the sim, so it is worth the tiny cost now.")

(defrecord Command [tick throttle brake steer handbrake reset])

(def ^:private held (js/Set.))

;; Not part of a `Command`, and deliberately so: the headlights change nothing
;; the sim integrates, so putting them in the command would mean every AI and
;; every network peer had to carry an opinion about them.
(def ^:private lights-forced (volatile! false))

;; What a thumb is doing, when there is one. Held here rather than merged in
;; `main` so that `sample` stays the one place a `Command` is assembled: a
;; command has to mean the same thing whether a key, a thumb, the AI or the
;; network produced it, and two places building one is two chances to disagree.
(def ^:private touch (volatile! nil))

(def ^:private bindings
  {"KeyW" :fwd   "ArrowUp"    :fwd
   "KeyS" :back  "ArrowDown"  :back
   "KeyA" :left  "ArrowLeft"  :left
   "KeyD" :right "ArrowRight" :right
   "Space" :handbrake
   "KeyR"  :reset
   "KeyL"  :lights})

(defn- down? [action]
  (some (fn [[code a]] (and (= a action) (.has held code))) bindings))

(defn attach!
  "Wire up listeners. Returns a detach fn."
  []
  (let [on-down (fn [e]
                  (when (contains? bindings (.-code e))
                    (.preventDefault e)
                    ;; A latch, not a hold. Lights are the one control here
                    ;; that stays where it was put.
                    (when (and (= "KeyL" (.-code e)) (not (.-repeat e)))
                      (vswap! lights-forced not))
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

(defn handbrake-held?
  "For the dashboard tell-tale. Reading raw key state is fine here and only
  here: this never becomes a `Command`, so it cannot desync anything."
  []
  (down? :handbrake))

(defn use-touch!
  "Register the on-screen controls' state atom, or nil to forget them."
  [state]
  (vreset! touch state))

(defn- pad
  "What the thumbs are asking for, or nothing at all."
  []
  (if-let [t @touch] @t {:steer 0.0 :gas false :brake false :handbrake false}))

(defn lights-forced?
  "Has the driver switched the lights on themselves?

  They come on by themselves under cloud, which is right and was also
  completely opaque: there was no way to tell whether the beams in front of you
  were the weather's doing or something broken. A switch and a tell-tale settle
  it."
  []
  @lights-forced)

(defn sample
  "Snapshot the current keyboard into an immutable command for `tick`.

  Axes are already normalised to [-1, 1] / [0, 1] here rather than in the sim,
  so a networked or AI-driven command is indistinguishable from a human one."
  [tick]
  (let [{:keys [steer gas brake handbrake]} (pad)]
    (->Command tick
               (if (or (down? :fwd) gas) 1.0 0.0)
               (if (or (down? :back) brake) 1.0 0.0)
               ;; Whichever is asking for more lock. Adding them would let a
               ;; key and a thumb cancel each other out, which is a way to be
               ;; surprised by a car that will not turn.
               (let [k (+ (if (down? :left) -1.0 0.0) (if (down? :right) 1.0 0.0))]
                 (if (> (js/Math.abs steer) (js/Math.abs k)) steer k))
               (or (down? :handbrake) handbrake)
               (down? :reset))))
