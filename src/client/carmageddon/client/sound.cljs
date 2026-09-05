(ns carmageddon.client.sound
  "Everything you can hear, synthesised.

  No files. The textures are painted onto a canvas at boot and the geometry is
  built in code, and audio is held to the same rule: a sawtooth through a
  filter is an engine, a burst of noise through a bandpass is a collision, and
  neither of them is a download, a decode, or a thing that can 404. It also
  means the sound of a car is a function of its speed rather than of a loop
  that has to be crossfaded.

  Two kinds of sound here and they are built differently. The engine and the
  tyres are *continuous*: one oscillator each, running from the first frame to
  the last, with their frequency and gain steered every frame. Everything else
  is a *one-shot*: a node graph created, started, and left to be collected when
  its envelope reaches zero. Starting an oscillator per frame would be a new
  graph sixty times a second; keeping one alive per impact would be a leak.

  Nothing here is created until the player has clicked something. A browser
  will not start an AudioContext without a gesture, and one created too early
  is not merely silent, it is permanently suspended.")

(def ^:private idle-hz 42.0)      ; engine at rest
(def ^:private rev-hz 118.0)      ; how far one gear climbs
(def ^:private gears 5)

(defn- noise-buffer
  "Two seconds of white noise, generated once and shared by every one-shot that
  needs a crunch. A `BufferSource` is single-use but the buffer behind it is
  not, and generating this per collision is 90k random numbers per bang."
  [^js ctx]
  (let [rate (.-sampleRate ctx)
        n (int (* 2 rate))
        buf (.createBuffer ctx 1 n rate)
        ch (.getChannelData buf 0)]
    (dotimes [i n]
      (aset ch i (- (* 2.0 (js/Math.random)) 1.0)))
    buf))

(defn create!
  "Build the audio graph. Call from a click handler, not from boot."
  []
  (when-let [Ctx (or (.-AudioContext js/window) (.-webkitAudioContext js/window))]
    (let [^js ctx (Ctx.)
          ^js master (.createGain ctx)
          ;; A little headroom under the ceiling: several one-shots can land on
          ;; the same frame and clipping a sum is much uglier than any of the
          ;; sounds in it.
          _ (set! (.-value (.-gain master)) 0.55)
          _ (.connect master (.-destination ctx))

          ;; --- engine ---------------------------------------------------
          ;; Two saws a few cents apart. One is a tone; two that disagree
          ;; slightly is a machine, and the beating between them is most of
          ;; what makes it sound like it has moving parts in it.
          ^js osc-a (.createOscillator ctx)
          ^js osc-b (.createOscillator ctx)
          ^js eng-filter (.createBiquadFilter ctx)
          ^js eng-gain (.createGain ctx)

          ;; --- tyres ----------------------------------------------------
          ^js buf (noise-buffer ctx)
          ^js skid-src (.createBufferSource ctx)
          ^js skid-filter (.createBiquadFilter ctx)
          ^js skid-gain (.createGain ctx)]

      (set! (.-type osc-a) "sawtooth")
      (set! (.-type osc-b) "sawtooth")
      (set! (.-value (.-frequency osc-a)) idle-hz)
      (set! (.-value (.-frequency osc-b)) (* 1.006 idle-hz))
      (set! (.-type eng-filter) "lowpass")
      (set! (.-value (.-frequency eng-filter)) 700)
      (set! (.-value (.-Q eng-filter)) 3.0)
      (set! (.-value (.-gain eng-gain)) 0.0)
      (.connect osc-a eng-filter)
      (.connect osc-b eng-filter)
      (.connect eng-filter eng-gain)
      (.connect eng-gain master)
      (.start osc-a)
      (.start osc-b)

      (set! (.-buffer skid-src) buf)
      (set! (.-loop skid-src) true)
      (set! (.-type skid-filter) "bandpass")
      (set! (.-value (.-frequency skid-filter)) 1700)
      (set! (.-value (.-Q skid-filter)) 6.0)
      (set! (.-value (.-gain skid-gain)) 0.0)
      (.connect skid-src skid-filter)
      (.connect skid-filter skid-gain)
      (.connect skid-gain master)
      (.start skid-src)

      (atom {:ctx ctx :master master :noise buf
             :osc-a osc-a :osc-b osc-b :eng-filter eng-filter :eng-gain eng-gain
             :skid-filter skid-filter :skid-gain skid-gain
             :muted? false}))))

(defn resume!
  "Browsers start a context suspended until a gesture has been seen."
  [s]
  (when s
    (let [^js ctx (:ctx @s)]
      (when (= "suspended" (.-state ctx)) (.resume ctx)))))

(defn muted? [s] (boolean (and s (:muted? @s))))

(defn toggle-mute! [s]
  (when s
    (let [m (not (:muted? @s))
          ^js master (:master @s)]
      (swap! s assoc :muted? m)
      (.setTargetAtTime (.-gain master) (if m 0.0 0.55)
                        (.-currentTime ^js (:ctx @s)) 0.02)
      m)))

;; --- continuous ------------------------------------------------------------

(defn engine!
  "Steer the engine every frame.

  Gears are the whole trick. Frequency straight from road speed rises and rises
  and sounds like a siren; splitting the speed range into five and letting the
  pitch fall back at each boundary is what makes it sound like a car being
  driven rather than a motor being turned up."
  [s {:keys [speed top throttle airborne?]}]
  (when s
    (let [{:keys [^js ctx ^js osc-a ^js osc-b ^js eng-filter ^js eng-gain]} @s
          t (.-currentTime ctx)
          v (min 1.0 (/ (js/Math.abs (or speed 0.0)) (max 1.0 (or top 60.0))))
          ;; Where in the current gear the engine is, 0 at the change-up and 1
          ;; just before the next.
          band (* v gears)
          frac (- band (js/Math.floor band))
          hz (+ idle-hz (* rev-hz (+ 0.35 (* 0.65 frac))))
          ;; A car with its wheels off the ground revs freely: no load, so the
          ;; note jumps and thins out.
          hz (if airborne? (* 1.35 hz) hz)
          load (+ 0.16 (* 0.5 (or throttle 0.0)) (* 0.22 v))]
      (.setTargetAtTime (.-frequency osc-a) hz t 0.05)
      (.setTargetAtTime (.-frequency osc-b) (* 1.006 hz) t 0.05)
      ;; The filter opens with the throttle, which is what makes a car under
      ;; power sound harder rather than merely louder.
      (.setTargetAtTime (.-frequency eng-filter)
                        (+ 420.0 (* 1500.0 (or throttle 0.0)) (* 900.0 v)) t 0.06)
      (.setTargetAtTime (.-gain eng-gain) (* 0.22 load) t 0.05))))

(defn tyres!
  "How hard the tyres are complaining, 0 to 1."
  [s amount]
  (when s
    (let [{:keys [^js ctx ^js skid-filter ^js skid-gain]} @s
          t (.-currentTime ctx)
          a (max 0.0 (min 1.0 (or amount 0.0)))]
      (.setTargetAtTime (.-gain skid-gain) (* 0.20 a) t 0.05)
      ;; A tyre that is only just letting go sings; one that has given up
      ;; roars. Moving the band rather than only the gain is what separates
      ;; those two, and they are different things to be told.
      (.setTargetAtTime (.-frequency skid-filter) (- 2100.0 (* 900.0 a)) t 0.08))))

;; --- one-shots -------------------------------------------------------------

(defn- burst!
  "A hit: noise through a band, with an envelope that is all attack and decay.

  The whole graph is disposable. It is started, it is left, and the browser
  collects it when the source ends -- which is why `stop` is scheduled here
  rather than left to a caller who would have to remember."
  [s {:keys [gain freq q decay kind]
      :or {gain 0.3 freq 900 q 1.2 decay 0.25 kind "bandpass"}}]
  (when s
    (let [{:keys [^js ctx ^js master ^js noise]} @s
          t (.-currentTime ctx)
          ^js src (.createBufferSource ctx)
          ^js f (.createBiquadFilter ctx)
          ^js g (.createGain ctx)]
      (set! (.-buffer src) noise)
      ;; Start somewhere random in the buffer, or every crunch is the same
      ;; crunch and a street full of crates sounds like a machine gun.
      (set! (.-type f) kind)
      (set! (.-value (.-frequency f)) freq)
      (set! (.-value (.-Q f)) q)
      (.setValueAtTime (.-gain g) gain t)
      (.exponentialRampToValueAtTime (.-gain g) 0.0008 (+ t decay))
      (.connect src f)
      (.connect f g)
      (.connect g master)
      (.start src (max 0.0 (- t 0.001)) (* 1.8 (js/Math.random)) decay)
      (.stop src (+ t decay 0.02)))))

(defn thump!
  "Metal on metal. `hard` is 0 to 1."
  [s hard]
  (let [h (max 0.0 (min 1.0 (or hard 0.0)))]
    (when (> h 0.02)
      ;; Low and long for a heavy hit, sharp and short for a graze.
      (burst! s {:gain (* 0.55 h) :freq (- 640.0 (* 300.0 h))
                 :q 0.9 :decay (+ 0.12 (* 0.30 h))}))))

(defn smash!
  "Something brittle. Higher and drier than a car."
  [s]
  (burst! s {:gain 0.22 :freq 2100 :q 1.6 :decay 0.16}))

(defn boom!
  "An explosion. `size` is 0 to 1; a barrel is small and a tanker is not."
  [s size]
  (let [z (max 0.0 (min 1.0 (or size 0.5)))]
    ;; Two layers, because one is a hiss and two is a bang: a lowpassed thud
    ;; that carries, and a brighter crack on top of it.
    (burst! s {:gain (+ 0.35 (* 0.45 z)) :freq (- 220.0 (* 120.0 z))
               :q 0.5 :decay (+ 0.5 (* 1.1 z)) :kind "lowpass"})
    (burst! s {:gain (* 0.30 z) :freq 1400 :q 0.8 :decay (+ 0.18 (* 0.3 z))})))

(defn chime!
  "Something collected. A short two-tone rise -- the one sound in the game that
  is a note rather than a noise, because it is the one thing that is good news."
  [s high?]
  (when s
    (let [{:keys [^js ctx ^js master]} @s
          t (.-currentTime ctx)
          ^js o (.createOscillator ctx)
          ^js g (.createGain ctx)
          f0 (if high? 880.0 560.0)]
      (set! (.-type o) "triangle")
      (.setValueAtTime (.-frequency o) f0 t)
      (.exponentialRampToValueAtTime (.-frequency o) (* 1.5 f0) (+ t 0.09))
      (.setValueAtTime (.-gain g) 0.0 t)
      (.linearRampToValueAtTime (.-gain g) 0.16 (+ t 0.012))
      (.exponentialRampToValueAtTime (.-gain g) 0.0008 (+ t 0.22))
      (.connect o g)
      (.connect g master)
      (.start o t)
      (.stop o (+ t 0.24)))))

(defn attach!
  "Wire the mute key. Returns a detach fn."
  [s]
  (let [on-key (fn [^js e]
                 (when (and (= "KeyV" (.-code e)) (not (.-repeat e)))
                   (.preventDefault e)
                   (toggle-mute! s)))]
    (.addEventListener js/window "keydown" on-key)
    (fn detach! [] (.removeEventListener js/window "keydown" on-key))))
