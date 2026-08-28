(ns carmageddon.client.overlay
  "The live overlay, and getting it on and off this machine.

  `carmageddon.shared.overlay` is the data and the rules for changing it; this
  is the atom holding one, plus the two places a browser can keep it. Everything
  destructible in the world reads this before it spawns and writes to it when it
  is destroyed, so there is exactly one answer to 'is that crate still there'
  rather than one per subsystem."
  (:require [carmageddon.shared.overlay :as ov]))

(def ^:private storage-prefix "carmagedonio/world/")

;; Saving is debounced rather than done on every change: a good run smashes a
;; few hundred things, and serialising the whole overlay each time would turn a
;; pile-up into a stutter.
(def ^:private save-every-ms 4000.0)

(defn create
  ([seed] (create seed :normal))
  ([seed mode] (atom (assoc (ov/empty-overlay seed mode) ::last-save 0.0))))

(defn- storage []
  (try (.-localStorage js/window) (catch :default _ nil)))

(defn- key-for [seed] (str storage-prefix seed))

(defn load!
  "Replace the overlay with the one saved for this world, if there is one and it
  still reads. Returns the overlay's stats, or nil if nothing was restored.

  A save from an older version is a thing to start fresh from, not an error to
  interrupt someone with, so an unreadable one is simply ignored."
  [state seed]
  (when-let [^js s (storage)]
    (when-let [saved (some-> (.getItem s (key-for seed)) ov/read-edn)]
      (when (= seed (:seed saved))
        (swap! state (fn [cur] (assoc (ov/merge-overlays cur saved) ::last-save 0.0)))
        (ov/stats saved)))))

(defn save!
  "Write the overlay out. `force?` bypasses the debounce, for the end of a run
  or the page closing."
  ([state now-ms] (save! state now-ms false))
  ([state now-ms force?]
   (let [cur @state]
     (when (and (or force? (>= (- now-ms (::last-save cur 0.0)) save-every-ms))
                (storage))
       (let [^js s (storage)]
         (try
           (.setItem s (key-for (:seed cur)) (ov/->edn cur))
           (swap! state assoc ::last-save now-ms)
           true
           ;; Storage can be full, or disabled entirely. Losing a save is worth
           ;; a line in the console and nothing more.
           (catch :default e
             (js/console.warn "overlay not saved:" (.-message e))
             false)))))))

(defn clear! [state seed]
  (when-let [^js s (storage)]
    (try (.removeItem s (key-for seed)) (catch :default _ nil)))
  (reset! state (assoc (ov/empty-overlay seed (:mode @state)) ::last-save 0.0)))

(defn destroyed [state key kind] (ov/destroyed @state key kind))
(defn record! [state key kind idx] (swap! state ov/record key kind idx))
(defn visit! [state key] (swap! state ov/visit key))
(defn set-vehicle! [state v] (swap! state ov/set-vehicle v))
(defn set-tally! [state tally] (swap! state ov/set-tally tally))
(defn stats [state] (ov/stats @state))
