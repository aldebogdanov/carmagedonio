(ns carmageddon.client.fire
  "Burning liquid: pools of it, what they do to whatever stands in them, and
  what they look like.

  A pool is not a physics object. It is a circle on the ground with an age, and
  everything about it -- how much it hurts, how big it is, whether it has gone
  out -- is a function of that age. Nothing is simulated, nothing collides, and
  a hundred of them cost a hundred distance checks.

  The life of a pool is grow, hold, die. It matters that it dies: fire that
  burns for ever turns a city into a permanent hazard map and takes the tension
  out of setting it, which is the opposite of the point. A barrel is finished in
  about twelve seconds and a tanker in twenty-five.

  Spread is deliberately bounded the same way. A pool with fuel behind it seeds
  a couple of children early in its life, each smaller and shorter-lived than
  its parent, and children do not seed. Two generations, and then it is over --
  which is a spreading fire to look at and a terminating one to reason about.

  Ownership is carried because a pedestrian who walks into a fire is worth
  points to whoever lit it and to nobody else. Without it the sensible play
  would be to torch an industrial estate and drive away."
  (:require ["three" :as three]
            [carmageddon.client.figures :as fig]))

;; --- shape of a life --------------------------------------------------------

(def ^:private grow-frac 0.18)   ; of its life spent reaching full size
(def ^:private fade-frac 0.35)   ; and shrinking again at the end

(defn- envelope
  "How much of its full size a pool is at age `t` of lifetime `life`, 0 to 1."
  [t life]
  (let [u (/ t life)]
    (cond
      (>= u 1.0) 0.0
      (< u grow-frac) (/ u grow-frac)
      (> u (- 1.0 fade-frac)) (/ (- 1.0 u) fade-frac)
      :else 1.0)))

(deftype Pool [x y z r0 life owner ^:mutable t ^:mutable seeds]
  ;; deftype rather than a map: `heat-at` walks every pool for every vehicle
  ;; and every pedestrian in range, on the fixed step.
  Object
  (toString [_] (str "Pool " (.toFixed x 0) "," (.toFixed z 0))))

;; --- the set of them --------------------------------------------------------

(def ^:private max-pools 96)
(def ^:private flames-per-pool 9)
(def ^:private smoke-per-pool 2)

;; Flames are drawn from pools like everything else in this client. Sized for
;; the cap: past it the oldest pool is dropped rather than the newest refused,
;; because a fire that will not start is a bug and a fire that ends early is a
;; fire.
(def ^:private flame-slots (* max-pools flames-per-pool))
(def ^:private smoke-slots (* max-pools smoke-per-pool))

(defn create [^js scene]
  (let [flame-mat (three/MeshBasicMaterial. #js {:transparent true :opacity 0.7
                                                 :depthWrite false})
        smoke-mat (three/MeshBasicMaterial. #js {:color 0x2a2724 :transparent true
                                                 :opacity 0.30 :depthWrite false})
        ;; The liquid itself. Without it the flames read as traffic cones
        ;; standing on a road; with it they read as a road that is alight,
        ;; which is the whole difference.
        slick-mat (three/MeshBasicMaterial. #js {:color 0xff7a1e :transparent true
                                                 :opacity 0.55 :depthWrite false})]
    (atom {:scene scene
           ;; Basic materials, not lit ones: a flame is not a surface that light
           ;; falls on, it is the light. Depth writes are off so overlapping
           ;; flames do not cut holes in each other.
           ;; A cone, not a box. A flame is the one shape in this game that
           ;; genuinely is not a box, and a six-sided cone costs the same as a
           ;; cube to instance.
           :flames (fig/pool scene (three/ConeGeometry. 0.5 1 6) flame-mat
                             flame-slots {:cast? false})
           :smoke (fig/pool scene (three/BoxGeometry. 1 1 1) smoke-mat
                            smoke-slots {:cast? false})
           :slick (fig/pool scene (three/CylinderGeometry. 0.5 0.5 1 16) slick-mat
                            max-pools {:cast? false})
           :pools #js []
           :m4 (three/Matrix4.)
           :drawn 0
           :lit 0})))

(defn- push! [fs ^Pool p]
  (let [^js ps (:pools @fs)]
    (when (>= (.-length ps) max-pools) (.shift ps))
    (.push ps p)
    (swap! fs update :lit inc)))

(defn ignite!
  "Start a fire `r` metres across at (x, y, z), burning for `life` seconds.

  `y` is the ground it is burning on, and it is not optional. The first version
  took only x and z and drew everything at world zero, which put the slick
  under the road and the flames waist-deep in it anywhere the terrain was not
  at sea level -- which is almost everywhere.

  `seeds` is how many children it may throw -- 0 for a barrel, 2 for a tanker.
  `owner` is who gets the points for whatever walks into it."
  ([fs x y z r life owner] (ignite! fs x y z r life owner 0))
  ([fs x y z r life owner seeds]
   (push! fs (->Pool x y z r life owner 0.0 seeds))
   nil))

(defn tick!
  "Age every pool, spread the ones with fuel behind them, and drop the dead."
  [fs dt]
  (let [^js ps (:pools @fs)
        out #js []]
    (dotimes [i (.-length ps)]
      (let [^Pool p (aget ps i)]
        (set! (.-t p) (+ (.-t p) dt))
        ;; Children come early, while there is still something to run. A pool
        ;; that seeded at the end of its life would look like fire appearing
        ;; from nowhere next to embers.
        (when (and (pos? (.-seeds p))
                   (> (.-t p) (* 0.22 (.-life p))))
          (set! (.-seeds p) (dec (.-seeds p)))
          (let [a (* 6.2831853 (js/Math.random))
                d (* (.-r0 p) (+ 0.7 (* 0.6 (js/Math.random))))]
            (swap! fs update :lit inc)
            (.push out (->Pool (+ (.-x p) (* d (js/Math.sin a)))
                               (.-y p)
                               (+ (.-z p) (* d (js/Math.cos a)))
                               (* 0.72 (.-r0 p))
                               (* 0.70 (.-life p))
                               (.-owner p)
                               0.0
                               ;; Children do not seed. Two generations is a
                               ;; spreading fire; unbounded is a bug that only
                               ;; shows up on somebody else's machine.
                               0))))
        (when (< (.-t p) (.-life p)) (.push out p))))
    (swap! fs assoc :pools out)
    nil))

(defn heat-at
  "How fiercely (x, z) is burning, 0 to 1, and who owns the worst of it.

  Returns nil when nothing is burning there, which is the overwhelmingly common
  answer and the reason this is a bare loop over a typed field rather than
  anything cleverer."
  [fs x z]
  (let [^js ps (:pools @fs)]
    (loop [i 0, best 0.0, owner nil]
      (if (>= i (.-length ps))
        (when (pos? best) {:heat best :owner owner})
        (let [^Pool p (aget ps i)
              e (envelope (.-t p) (.-life p))
              r (* e (.-r0 p))]
          (if (or (zero? e) (zero? r))
            (recur (inc i) best owner)
            (let [dx (- x (.-x p)) dz (- z (.-z p))
                  d (js/Math.sqrt (+ (* dx dx) (* dz dz)))]
              (if (< d r)
                ;; Hottest in the middle, so driving through the edge of a pool
                ;; is survivable and stopping in one is not.
                (let [h (* e (- 1.0 (* 0.6 (/ d r))))]
                  (if (> h best)
                    (recur (inc i) h (.-owner p))
                    (recur (inc i) best owner)))
                (recur (inc i) best owner)))))))))

(defn burning? [fs x z] (some? (heat-at fs x z)))

;; --- drawing ----------------------------------------------------------------

(def ^:private hot 0xffd24a)
(def ^:private cool 0x9c2b12)

(defn sync!
  "Draw every pool as a cluster of flames and a little smoke above it.

  Boxes, like everything else here. A flame is a tall thin box that jitters on
  a per-flame phase and shortens as the pool dies; seven of them read as fire
  at the distance anyone sees one from, and the whole system is two draws."
  [fs now-s]
  (let [{:keys [flames smoke slick ^js m4 drawn]} @fs
        ^js ps (:pools @fs)
        n (.-length ps)
        ;; Only touch the slots in use plus whatever was in use last frame, so
        ;; an empty city costs nothing. Clearing all 672 every frame regardless
        ;; is the kind of cost that is invisible until there are twenty other
        ;; systems doing the same thing.
        span (max n (or drawn 0))]
    (swap! fs assoc :drawn n)
    (dotimes [i (* span flames-per-pool)]
      (let [pool-i (quot i flames-per-pool)
            j (mod i flames-per-pool)]
        (if (>= pool-i (.-length ps))
          (fig/set-matrix! flames i (doto m4 (.makeScale 0 0 0)))
          (let [^Pool p (aget ps pool-i)
                e (envelope (.-t p) (.-life p))
                r (* e (.-r0 p))
                ;; Fixed per-flame offsets, from two irrational strides. The
                ;; first attempt hashed with `7919 mod 360`, which is 359 --
                ;; so consecutive flames landed one degree apart and all nine
                ;; of them stacked into what looked like a single slab.
                a (* 6.2831853 (mod (+ (* 0.6180339 j) (* 0.3111 pool-i)) 1.0))
                ;; sqrt, so the flames spread over the pool's *area* rather
                ;; than bunching towards the middle.
                rad (* r (js/Math.sqrt (mod (+ (* 0.7548776 j) (* 0.113 pool-i)) 1.0)))
                fx (+ (.-x p) (* rad (js/Math.sin a)))
                fz (+ (.-z p) (* rad (js/Math.cos a)))
                flick (+ 0.55 (* 0.45 (js/Math.sin (+ (* 9.0 now-s) (* 2.1 j) pool-i))))
                ;; Height barely scales with the pool. A tanker fire is a wider
                ;; fire, not a fire with twenty-metre flames -- which is what
                ;; scaling height by radius gave, and it looked like two office
                ;; blocks made of custard.
                fh (* e flick (+ 1.8 (* 0.42 (.-r0 p))))
                fw (* e (+ 0.55 (* 0.145 (.-r0 p))))]
            (.makeScale m4 (max 0.01 fw) (max 0.01 fh) (max 0.01 fw))
            (aset (.-elements m4) 12 fx)
            (aset (.-elements m4) 13 (+ (.-y p) (* 0.5 fh)))
            (aset (.-elements m4) 14 fz)
            (fig/set-matrix! flames i m4)
            (fig/set-colour! flames i (if (> flick 0.75) hot cool))))))
    ;; The burning slick: one flat disc per pool, just clear of the road so it
    ;; does not fight the carriageway for the same depth.
    (dotimes [i span]
      (if (>= i n)
        (fig/set-matrix! slick i (doto m4 (.makeScale 0 0 0)))
        (let [^Pool p (aget ps i)
              e (envelope (.-t p) (.-life p))
              r (* e (.-r0 p))]
          (.makeScale m4 (max 0.01 (* 2.0 r)) 0.06 (max 0.01 (* 2.0 r)))
          (aset (.-elements m4) 12 (.-x p))
          (aset (.-elements m4) 13 (+ (.-y p) 0.09))
          (aset (.-elements m4) 14 (.-z p))
          (fig/set-matrix! slick i m4))))
    (dotimes [i (* span smoke-per-pool)]
      (let [pool-i (quot i smoke-per-pool)
            j (mod i smoke-per-pool)]
        (if (>= pool-i (.-length ps))
          (fig/set-matrix! smoke i (doto m4 (.makeScale 0 0 0)))
          (let [^Pool p (aget ps pool-i)
                e (envelope (.-t p) (.-life p))
                r (* e (.-r0 p))
                rise (mod (+ (* 0.35 now-s) (* 0.5 j)) 1.0)
                s (* 0.5 r (+ 0.8 rise))]
            (.makeScale m4 (max 0.01 s) (max 0.01 s) (max 0.01 s))
            (aset (.-elements m4) 12 (.-x p))
            (aset (.-elements m4) 13 (+ (.-y p) (* 2.2 r) (* 6.0 rise)))
            (aset (.-elements m4) 14 (.-z p))
            (fig/set-matrix! smoke i m4)))))
    (fig/flush! flames)
    (fig/flush! smoke)
    (fig/flush! slick)))

(defn stats [fs]
  {:pools (.-length (:pools @fs)) :lit (:lit @fs)})
