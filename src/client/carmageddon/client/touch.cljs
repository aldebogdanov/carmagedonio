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

(defn- looking!
  "Everything on the screen that is not a control is a window to look through.

  One finger dragged there swings the view, exactly as a mouse drag does; two
  pinch it in and out. It listens on the overlay itself and only to pointers
  that landed on the overlay rather than on a control in it, so a thumb that
  slides off the steering pad keeps steering and does not start looking."
  [^js root {:keys [look zoom]}]
  (let [pts (js/Map.)
        spread (fn []
                 (let [[a b] (vec (.values pts))]
                   (js/Math.hypot (- (aget a 0) (aget b 0))
                                  (- (aget a 1) (aget b 1)))))
        last-spread (volatile! nil)
        release (fn [^js e]
                  (when (.has pts (.-pointerId e))
                    (.delete pts (.-pointerId e))
                    (vreset! last-spread nil)))]
    (.addEventListener
     root "pointerdown"
     (fn [^js e]
       (when (identical? (.-target e) root)
         (.preventDefault e)
         (capture! root e)
         (.set pts (.-pointerId e) #js [(.-clientX e) (.-clientY e)])
         (vreset! last-spread nil))))
    (.addEventListener
     root "pointermove"
     (fn [^js e]
       (when-let [^js p (.get pts (.-pointerId e))]
         (.preventDefault e)
         (let [dx (- (.-clientX e) (aget p 0))
               dy (- (.-clientY e) (aget p 1))]
           (aset p 0 (.-clientX e))
           (aset p 1 (.-clientY e))
           (if (= 2 (.-size pts))
             (let [d (spread)]
               (when-let [d0 @last-spread]
                 (when (and zoom (pos? d)) (zoom (/ d0 d))))
               (vreset! last-spread d))
             ;; A finger covers less ground than a mouse for the same
             ;; intent, so it turns the view a little further per pixel.
             (when look (look (* 1.4 dx) (* 1.4 dy))))))))
    (.addEventListener root "pointerup" release)
    (.addEventListener root "pointercancel" release)))

(defn attach!
  "Build the controls and wire them. Returns a detach fn, or nil on a device
  that does not want them.

  `camera` is {:look (fn [dx dy]) :zoom (fn [factor]) :cycle (fn [])} -- what
  the camera does with a drag, a pinch and its button. Passed in rather than
  required, for the reason everything here goes through `input`: this is a way
  of holding the controls, and should not know what a camera is."
  ([] (attach! {}))
  ([camera]
  (when (available?)
    (let [state (atom {:steer 0.0 :gas false :brake false :handbrake false})
          root (el :div {:id "touch"})
          pad  (el :div {:id "touch-steer" :text "STEER"})
          gas  (el :div {:class "touch-btn gas" :text "GO"})
          brk  (el :div {:class "touch-btn brake" :text "BRAKE"})
          hand (el :div {:class "touch-btn hand" :text "HAND"})
          ;; There was no way to change the view at all on a phone: it is the
          ;; C key, and a phone has no C key.
          cam  (el :div {:class "touch-btn cam" :text "CAM"})]
      (doseq [n [pad brk hand gas cam]] (.appendChild root n))
      (.appendChild js/document.body root)
      (.add (.-classList js/document.body) "touch")
      (steering! pad state)
      (press! gas state :gas)
      (press! brk state :brake)
      (press! hand state :handbrake)
      (.addEventListener cam "pointerdown"
                         (fn [^js e]
                           (.preventDefault e)
                           (when-let [f (:cycle camera)] (f))))
      (looking! root camera)
      (input/use-touch! state)
      (fn detach! []
        (input/use-touch! nil)
        (.remove root)
        (.remove (.-classList js/document.body) "touch"))))))
