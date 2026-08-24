(ns carmageddon.shared.interp
  "Entity interpolation for remote players.

  Snapshots arrive 25 times a second, jittered by the network; the display wants
  a position 60+ times a second. The fix is not to predict forward but to render
  *behind*: hold a short buffer and draw the world as it was `interp-delay-ms`
  ago, which is almost always bracketed by two real snapshots. A proxy is then
  always moving between two things the server actually said, rather than
  somewhere the client guessed.

  The cost is that remote cars are rendered a tenth of a second in the past.
  That is the standard trade and it is the right one here: a proxy that stutters
  or rubber-bands is far more noticeable than one that is slightly late.

  Pure and platform-free so the awkward parts -- gaps, reordering, running off
  the end of the buffer -- can be tested without a socket."
  (:require [carmageddon.shared.constants :as k]))

(def ^:private max-samples 24)

(defn buffer [] [])

(defn insert
  "Add a snapshot stamped with local receive time. Out-of-order arrivals are
  placed correctly rather than dropped -- reordering is normal, and a late
  packet still improves the picture if it lands before it is needed."
  [buf t state]
  (let [entry {:t t :state state}
        buf   (if (or (empty? buf) (>= t (:t (peek buf))))
                (conj buf entry)
                (vec (sort-by :t (conj buf entry))))]
    (if (> (count buf) max-samples)
      (subvec buf (- (count buf) max-samples))
      buf)))

(defn- lerp [a b f] (+ a (* (- b a) f)))

(defn- lerp3 [a b f]
  [(lerp (nth a 0) (nth b 0) f)
   (lerp (nth a 1) (nth b 1) f)
   (lerp (nth a 2) (nth b 2) f)])

(defn- nlerp4
  "Normalised lerp between quaternions, taking the shorter arc. Cheaper than a
  true slerp and indistinguishable across the small angles between consecutive
  snapshots -- but the sign check matters: without it a proxy occasionally spins
  the long way round between two nearly identical rotations."
  [a b f]
  (let [dot (reduce + (map * a b))
        b   (if (neg? dot) (mapv - b) b)
        v   (mapv #(lerp %1 %2 f) a b)
        len (Math/sqrt (reduce + (map * v v)))]
    (if (< len 1e-9) a (mapv #(/ % len) v))))

(defn sample-at
  "The state to draw at local time `now`, or nil if nothing is known yet.

  Renders `interp-delay-ms` in the past. Past the end of the buffer it holds the
  last known state rather than extrapolating: a proxy that stops for a moment
  looks like lag, while one that sails onwards has to be snapped back."
  ([buf now] (sample-at buf now k/interp-delay-ms))
  ([buf now delay-ms]
   (when (seq buf)
     (let [target (- now delay-ms)]
       (cond
         (<= target (:t (first buf))) (:state (first buf))
         (>= target (:t (peek buf)))  (:state (peek buf))
         :else
         (let [[a b] (->> (partition 2 1 buf)
                          (filter (fn [[x y]] (and (<= (:t x) target) (<= target (:t y)))))
                          first)]
           (if-not b
             (:state (peek buf))
             (let [span (- (:t b) (:t a))
                   f    (if (pos? span) (/ (- target (:t a)) span) 0.0)
                   sa   (:state a) sb (:state b)]
               {:pos  (lerp3 (:pos sa) (:pos sb) f)
                :quat (nlerp4 (:quat sa) (:quat sb) f)
                :damage (lerp (:damage sa 0.0) (:damage sb 0.0) f)}))))))))

(defn stale?
  "True when nothing has arrived for a while -- the player has probably gone."
  [buf now timeout-ms]
  (or (empty? buf) (> (- now (:t (peek buf))) timeout-ms)))
