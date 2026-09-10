(ns carmageddon.client.events
  "A running note of what the player just did, top right.

  The cluster shows the score but not where it came from: a number that jumps
  from 1240 to 1470 says nothing about which of the four things that could have
  caused it did. This is the other half of the instrument -- what happened, what
  it was worth, and how much clock it bought.

  Every line comes through `game/award!`, which is the single funnel every
  scoring path already went through. Nothing here can drift from the score,
  because there is no second opinion about what an event is worth: the rules
  say, `award!` applies it, and this prints what `award!` was handed.

  Plain DOM rather than the cockpit's canvas. It is six lines of text that
  change a few times a second, and a canvas would mean laying out wrapped text
  by hand for no gain."
  (:require [carmageddon.shared.rules :as rules]))

(def ^:private life 4200)          ; ms a line stays up
(def ^:private max-lines 6)
;; Repeats of the same thing inside this window become one line with a count.
;; A trail of nine coins is one event to a player and nine to the scorer, and
;; without this it pushes everything else off the top of the log.
(def ^:private merge-window 1500)

(def ^:private labels
  {:ped    "pedestrian"
   :prop   "clutter"
   :car    "traffic car"
   :wreck  "RIVAL WRECKED"
   :dent   "rival damaged"
   :coin   "coin"
   :nugget "gold"
   :delivery "DELIVERED"
   :express  "EXPRESS DELIVERED"
   :fragile  "DELIVERED INTACT"
   :tour     "ROUND FINISHED"
   :streak   "THREE IN A ROW"})

(defn create!
  "Make the log element and attach it. Returns the state."
  []
  (let [el (js/document.createElement "div")]
    (set! (.-id el) "events")
    (.appendChild js/document.body el)
    (atom {:el el :lines []})))

(defn- fresh [lines now] (vec (remove #(> (- now (:at %)) life) lines)))

(defn note!
  "Record one scoring event. `kind` is a `rules/scoring` key."
  [es kind points seconds]
  (when es
    (let [now (js/Date.now)]
      (swap! es update :lines
             (fn [lines]
               (let [ls (fresh lines now)
                     prev (peek ls)]
                 (if (and prev
                          (= kind (:kind prev))
                          (< (- now (:at prev)) merge-window))
                   (conj (pop ls) (-> prev
                                      (update :n inc)
                                      (update :points + points)
                                      (update :seconds + seconds)
                                      (assoc :at now)))
                   (vec (take-last max-lines
                                   (conj ls {:kind kind :n 1 :points points
                                             :seconds seconds :at now}))))))))))

(def ^:private job-words
  {:offered   "job: "
   :picked-up "picked up "
   :dropped   "dropped one, "
   :failed    "job lost: "})

(def ^:private why-words
  {:late "ran out of time" :broken "cargo broken"
   :abandoned "walked away from it"})

(defn job!
  "A notice about work that is not itself worth points -- being given a job,
  collecting the cargo, losing it. The payout goes through `note!` like
  everything else; this is the running commentary around it, and it carries its
  own words because there is no scoring entry to look them up from."
  [es what job]
  (when (and es (job-words what))
    (let [text (case what
                 ;; Both ends, in the order they happen. The first version
                 ;; named the *pickup* after the word "to", which reads as the
                 ;; destination and is the one thing the line has to get right.
                 :offered (str (job-words what) (:cargo job) ", "
                               (:label (first (:stops job)))
                               " \u2192 " (:label (last (:stops job))))
                 :picked-up (str (job-words what) (:cargo job))
                 :dropped (str (job-words what)
                               (- (count (:stops job)) (:at job)) " to go")
                 :failed (str (job-words what) (get why-words (:why job) "gone")))]
      (swap! es update :lines
             (fn [lines]
               (vec (take-last max-lines
                               (conj (fresh lines (js/Date.now))
                                     {:kind what :n 1 :text text
                                      :points 0 :seconds 0
                                      :at (js/Date.now)}))))))))

(defn- line-el
  "One entry, built out of nodes rather than a string of markup. Everything in
  here is ours and none of it is user text, but building HTML by hand is a
  habit worth not having."
  [{:keys [kind n points seconds text]} faded?]
  (let [row (js/document.createElement "div")
        what (js/document.createElement "span")
        pts (js/document.createElement "span")]
    (set! (.-className row) (str "ev" (when faded? " fade")))
    (set! (.-className what) "what")
    (set! (.-textContent what)
          (or text
              (str (get labels kind (name kind)) (when (> n 1) (str " ×" n)))))
    (set! (.-className pts) "pts")
    (set! (.-textContent pts)
          (if (pos? points)
            (str "+" points (when (pos? seconds)
                              (str "  +" (.toFixed seconds 1) "s")))
            ""))
    (.appendChild row what)
    (.appendChild row pts)
    row))

(defn sync!
  "Repaint. Called at HUD rate -- this is a log, not an instrument."
  [es now]
  (when es
    (let [{:keys [^js el lines]} @es
          live (fresh lines now)]
      (when (not= (count live) (count lines))
        (swap! es assoc :lines live))
      (set! (.-textContent el) "")
      ;; Newest at the top, and the oldest fading, so the eye knows which way
      ;; the list grows without having to watch it grow.
      (doseq [e (reverse live)]
        (.appendChild el (line-el e (> (- now (:at e)) (* 0.6 life))))))))

(defn clear! [es] (when es (swap! es assoc :lines [])))

;; Sanity: a label for everything the rules can score, or an event arrives with
;; a keyword nobody wrote a word for.
(assert (every? labels (keys rules/scoring))
        "events/labels is missing a scoring kind")
