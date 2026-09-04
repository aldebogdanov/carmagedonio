(ns carmageddon.client.touch
  "Driving controls for a phone held sideways.

  A car needs one analogue axis and two pedals, and a thumb is not a key. The
  steering is a *drag*, not a pair of buttons: touch down anywhere in the left
  half and the offset from where you landed is the lock. That means the wheel
  is wherever your thumb already is rather than somewhere you have to find, and
  it gives proportional steering, which two buttons cannot -- a car you can only
  steer at full lock is undriveable at speed.

  Nothing here reaches the simulation directly. It produces the same axes the
  keyboard does and `input/sample` folds them into one `Command`, because a
  Command has to mean the same thing whether a human, a thumb, the AI or the
  network produced it."
  (:require [carmageddon.client.input :as input]))

;; How far from the touch-down point counts as full lock, as a share of the
;; steering pad's width. A quarter of it: a thumb pivots, it does not travel,
;; and asking for the whole pad means never reaching full lock at all.
(def ^:private lock-travel 0.25)

(defn available?
  "Is this worth showing?

  `pointer: coarse` rather than `maxTouchPoints`, which is the difference
  between a device driven by a finger and a device that merely has a
  touchscreen: a laptop with one reports touch points and does not want half
  its screen covered in pedals. Forced by `?touch=1` so the layout can be
  looked at on a desktop."
  []
  (or (some? (re-find #"[?&]touch=1" (or js/window.location.search "")))
      (.-matches (js/window.matchMedia "(pointer: coarse)"))))

(defn- el [tag attrs]
  (let [^js e (js/document.createElement (name tag))]
    (doseq [[k v] attrs]
      (if (= :text k)
        (set! (.-textContent e) v)
        (.setAttribute e (name k) v)))
    e))

(defn- capture!
  "Keep this element receiving the pointer after the thumb slides off it --
  otherwise a pedal sticks on the moment you roll your thumb.

  Allowed to fail. A browser refuses capture for a pointer that has already
  been released, and a control that throws on the way down is worse than one
  that merely loses its grip."
  [^js node ^js e]
  (try (.setPointerCapture node (.-pointerId e))
       (catch :default _ nil)))

(defn- press!
  "Wire one button to a key of the state atom. Pointer events rather than touch
  events: they cover a mouse as well, which is what makes `?touch=1` usable,
  and a captured pointer keeps firing at this element after the thumb has slid
  off it -- otherwise a pedal sticks on the moment you roll your thumb."
  [^js node state k]
  (let [flag (fn [v] (fn [^js e]
                       (.preventDefault e)
                       (when v (capture! node e))
                       (swap! state assoc k v)))]
    (.addEventListener node "pointerdown" (flag true))
    (.addEventListener node "pointerup" (flag false))
    (.addEventListener node "pointercancel" (flag false))))

(defn- steering!
  "The left half. One pointer at a time -- the one that landed first owns the
  wheel, so a second thumb arriving on the pedals cannot yank the steering."
  [^js node state]
  (let [origin (volatile! nil)
        owner  (volatile! nil)]
    (.addEventListener
     node "pointerdown"
     (fn [^js e]
       (.preventDefault e)
       (when (nil? @owner)
         (vreset! owner (.-pointerId e))
         (vreset! origin (.-clientX e))
         (capture! node e))))
    (.addEventListener
     node "pointermove"
     (fn [^js e]
       (when (= @owner (.-pointerId e))
         (.preventDefault e)
         (let [travel (max 1.0 (* lock-travel (.-clientWidth node)))
               d (/ (- (.-clientX e) @origin) travel)]
           (swap! state assoc :steer (max -1.0 (min 1.0 d)))))))
    (let [release (fn [^js e]
                    (when (= @owner (.-pointerId e))
                      (vreset! owner nil)
                      (vreset! origin nil)
                      ;; Straight back to centre. A wheel that stays where it
                      ;; was left is a car that drives into a wall the moment
                      ;; you take your thumb off to reach the handbrake.
                      (swap! state assoc :steer 0.0)))]
      (.addEventListener node "pointerup" release)
      (.addEventListener node "pointercancel" release))))

(defn attach!
  "Build the controls and wire them. Returns a detach fn, or nil on a device
  that does not want them."
  []
  (when (available?)
    (let [state (atom {:steer 0.0 :gas false :brake false :handbrake false})
          root (el :div {:id "touch"})
          pad  (el :div {:id "touch-steer" :text "STEER"})
          gas  (el :div {:class "touch-btn gas" :text "GO"})
          brk  (el :div {:class "touch-btn brake" :text "BRAKE"})
          hand (el :div {:class "touch-btn hand" :text "HAND"})]
      (doseq [n [pad brk hand gas]] (.appendChild root n))
      (.appendChild js/document.body root)
      (.add (.-classList js/document.body) "touch")
      (steering! pad state)
      (press! gas state :gas)
      (press! brk state :brake)
      (press! hand state :handbrake)
      (input/use-touch! state)
      (fn detach! []
        (input/use-touch! nil)
        (.remove root)
        (.remove (.-classList js/document.body) "touch")))))
