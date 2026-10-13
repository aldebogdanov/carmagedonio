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
  #{:props :peds :cars :parts :pickups})

(defn empty-overlay
  ([seed] (empty-overlay seed :normal))
  ([seed mode]
   {:version version
    :seed seed
    :mode mode
    :chunks {}
    ;; When each chunk last lost something of each kind -- {key {kind ms}} --
    ;; which is what lets the world heal. See `heal`.
    :stamps {}
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
  "Note that index `idx` of `kind` in chunk `key` is gone, at wall-clock time
  `now` in ms if it is given.

  Recorded whether or not that chunk is loaded. A delta that arrived from
  another player refers to a chunk this client may not have reached yet, and it
  has to still be true when it does."
  ([ov key kind idx]
   {:pre [(kinds kind)]}
   (update-in ov [:chunks key kind] (fnil conj #{}) idx))
  ([ov key kind idx now]
   (-> (record ov key kind idx)
       (assoc-in [:stamps key kind] now))))

;; --- healing ----------------------------------------------------------------
;;
;; Destruction used to be forever, and that was a mistake that took a while to
;; show. The overlay is saved per world, and a world is played run after run
;; from the same spawn -- so every person ever run over near the start stayed
;; dead in every run after it. A few dozen runs in, the streets round the
;; spawn were empty, and the crowd looked thinner every week for a reason no
;; amount of reading the generator would find: the generator was producing the
;; same five hundred people it always had, and the save was killing them on
;; arrival.
;;
;; So a chunk forgets, but only some time after it last lost something, and
;; only when it is loaded again -- never in front of you. Within a run a
;; street you emptied stays empty, which is what made the record worth
;; keeping; come back after a while and somebody else has moved in.

(def memory-ms
  "How long a chunk remembers losing each kind of thing, counted from the last
  loss of that kind in it. People and traffic move on quickest; a smashed
  fence takes longer to be put back."
  {:peds    (* 6 60 1000)
   :cars    (* 6 60 1000)
   :pickups (* 15 60 1000)
   :props   (* 20 60 1000)
   :parts   (* 20 60 1000)})

(defn heal
  "Forget whatever chunk `key` lost long enough before `now` (ms).

  A kind with no stamp at all is forgotten at once: that is a save from before
  stamps existed, and it is exactly the save that has been emptying the
  streets."
  [ov key now]
  (if-let [c (get-in ov [:chunks key])]
    (let [stamps (get-in ov [:stamps key] {})
          stale  (for [kind (keys c)
                       :let [at (get stamps kind)]
                       :when (or (nil? at)
                                 (> (- now at) (get memory-ms kind 0)))]
                   kind)]
      (if (empty? stale)
        ov
        (let [c'      (apply dissoc c stale)
              stamps' (apply dissoc stamps stale)]
          (cond-> ov
            true (assoc-in [:chunks key] c')
            (empty? c') (update :chunks dissoc key)
            true (assoc-in [:stamps key] stamps')
            (empty? stamps') (update :stamps dissoc key)))))
    ov))

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
  (let [ov (update ov :chunks
                   (fn [cs]
                     (into {} (for [[k c] cs
                                    :let [c' (into {} (remove (fn [[_ v]] (empty? v)) c))]
                                    :when (seq c')]
                                [k c']))))]
    ;; A stamp for something no longer recorded is a stamp for nothing.
    (update ov :stamps
            (fn [ss]
              (into {} (for [[k s] ss
                             :let [s' (select-keys s (keys (get-in ov [:chunks k])))]
                             :when (seq s')]
                         [k s']))))))

(defn merge-overlays
  "Combine two overlays for the same world. Destruction is a union -- merging
  never un-smashes anything, which is `heal`'s job and only `heal`'s -- and of
  two stamps the later wins, while the vehicle and the tally are single-valued
  and the later one wins.

  This is what a server does when a client reconnects with local progress, and
  what a client does when a session's history arrives."
  [a b]
  (when (and (:seed a) (:seed b) (not= (:seed a) (:seed b)))
    (throw (ex-info "overlays are for different worlds"
                    {:a (:seed a) :b (:seed b)})))
  (-> a
      (assoc :chunks (merge-with (fn [ca cb] (merge-with into ca cb))
                                 (:chunks a) (:chunks b)))
      (assoc :stamps (merge-with (fn [sa sb] (merge-with max sa sb))
                                 (:stamps a {}) (:stamps b {})))
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
        (-> ov
            (update :chunks
                    (fn [cs]
                      (into {} (for [[k c] cs]
                                 [(vec k) (into {} (for [[kind v] c] [kind (set v)]))]))))
            ;; Absent from every save written before the world could heal, and
            ;; an absent stamp is what tells `heal` to forget at once.
            (update :stamps
                    (fn [ss]
                      (into {} (for [[k s] ss :when (map? s)] [(vec k) s])))))))
    (catch #?(:clj Exception :cljs :default) _ nil)))

(defn stats [ov]
  {:chunks (count (:chunks ov))
   :visited (count (:visited ov))
   :props (count-destroyed ov :props)
   :peds (count-destroyed ov :peds)
   :cars (count-destroyed ov :cars)
   :bytes (count (->edn ov))})
