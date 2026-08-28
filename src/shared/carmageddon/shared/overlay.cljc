(ns carmageddon.shared.overlay
  "Everything about a world that its seed does not already say.

  The world is a pure function of a seed, so none of it is stored. What *is*
  stored is the difference: which crate was smashed, who was run over, how badly
  the car is dented, where it got to. That difference is small -- a long run
  touches a few dozen chunks and a few hundred objects -- so it can be held
  entirely in memory, sent over a wire, and written to a file, none of which
  would be true of the world itself.

  One structure serves three purposes that used to want three:

    - the live record of what has happened, which the client reads before
      spawning anything into a chunk;
    - the save file, which is this printed as EDN;
    - the authoritative record a server keeps of a session.

  They are the same thing because they have to agree, and the cheapest way to
  make two things agree is for there to be one of them.

  Pure and `.cljc` throughout: the server has to be able to read, merge and
  reason about an overlay without a browser, and the client has to be able to
  apply one that arrived from somewhere else."
  (:require [clojure.edn :as edn]))

(def version 1)

(def kinds
  "What can be destroyed, and therefore what a chunk's delta can record. Adding
  a kind is safe; renaming one invalidates every saved world."
  #{:props :peds :cars})

(defn empty-overlay
  ([seed] (empty-overlay seed :normal))
  ([seed mode]
   {:version version
    :seed seed
    :mode mode
    :chunks {}
    :vehicle nil
    :tally {}
    :visited #{}}))

;; --- destruction ------------------------------------------------------------

(defn destroyed
  "The set of destroyed indices of `kind` in chunk `key`. Empty, never nil, so
  callers can `contains?` it without checking."
  [ov key kind]
  (get-in ov [:chunks key kind] #{}))

(defn destroyed? [ov key kind idx]
  (contains? (destroyed ov key kind) idx))

(defn record
  "Note that index `idx` of `kind` in chunk `key` is gone.

  Recorded whether or not that chunk is loaded. A delta that arrived from
  another player refers to a chunk this client may not have reached yet, and it
  has to still be true when it does."
  [ov key kind idx]
  {:pre [(kinds kind)]}
  (update-in ov [:chunks key kind] (fnil conj #{}) idx))

(defn count-destroyed [ov kind]
  (reduce + (map (fn [[_ c]] (count (get c kind))) (:chunks ov))))

;; --- the rest of the world's state -----------------------------------------

(defn visit [ov key] (update ov :visited conj key))

(defn set-vehicle
  "The player's car as it stands: where it is, how bent, and how it is set up.

  Tuning rides along because it is exactly the kind of thing a player changes
  and expects to find again, and it costs a dozen numbers."
  [ov v]
  (assoc ov :vehicle v))

(defn set-tally [ov tally] (assoc ov :tally tally))

(defn prune
  "Drop chunk entries that record nothing.

  Worth doing before saving: a chunk is added to the map the moment anything
  asks about it, and most chunks a player drives through never lose anything."
  [ov]
  (update ov :chunks
          (fn [cs]
            (into {} (for [[k c] cs
                           :let [c' (into {} (remove (fn [[_ v]] (empty? v)) c))]
                           :when (seq c')]
                       [k c'])))))

(defn merge-overlays
  "Combine two overlays for the same world. Destruction is a union -- it only
  ever accumulates, and nothing can be un-smashed -- while the vehicle and the
  tally are single-valued and the later one wins.

  This is what a server does when a client reconnects with local progress, and
  what a client does when a session's history arrives."
  [a b]
  (when (and (:seed a) (:seed b) (not= (:seed a) (:seed b)))
    (throw (ex-info "overlays are for different worlds"
                    {:a (:seed a) :b (:seed b)})))
  (-> a
      (assoc :chunks (merge-with (fn [ca cb] (merge-with into ca cb))
                                 (:chunks a) (:chunks b)))
      (assoc :visited (into (:visited a #{}) (:visited b #{})))
      (assoc :vehicle (or (:vehicle b) (:vehicle a)))
      (assoc :tally (or (:tally b) (:tally a)))
      (assoc :mode (or (:mode b) (:mode a)))))

;; --- serialisation ----------------------------------------------------------

(defn ->edn [ov] (pr-str (prune ov)))

(defn valid?
  "Is this something we can actually use? Deliberately shallow: a save file is
  not hostile input in the way a network message is, and the cost of being
  wrong is one lost run rather than a corrupted world."
  [ov]
  (and (map? ov)
       (= version (:version ov))
       (int? (:seed ov))
       (map? (:chunks ov))
       (every? (fn [[k c]] (and (vector? k) (= 2 (count k)) (map? c)))
               (:chunks ov))))

(defn read-edn
  "Parse an overlay, or nil if it is not one. Returns nil rather than throwing:
  a save from an older version is a thing to start fresh from, not an error to
  interrupt someone with."
  [s]
  (try
    (let [ov (edn/read-string s)]
      (when (valid? ov)
        ;; Sets survive EDN, but a hand-edited or hand-built file may carry
        ;; vectors; normalise so `contains?` means what callers expect.
        (update ov :chunks
                (fn [cs]
                  (into {} (for [[k c] cs]
                             [(vec k) (into {} (for [[kind v] c] [kind (set v)]))]))))))
    (catch #?(:clj Exception :cljs :default) _ nil)))

(defn stats [ov]
  {:chunks (count (:chunks ov))
   :visited (count (:visited ov))
   :props (count-destroyed ov :props)
   :peds (count-destroyed ov :peds)
   :cars (count-destroyed ov :cars)
   :bytes (count (->edn ov))})
