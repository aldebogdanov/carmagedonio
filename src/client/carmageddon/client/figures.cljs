(ns carmageddon.client.figures
  "Instanced pools, and the machinery for building a thing out of several of
  them.

  Pedestrians used to be one capsule each and one `three/Mesh` each. Sharing the
  geometry and the material does not share the draw call, so a street with two
  hundred people on it was two hundred draw calls before the shadow pass, and
  every one of them drew a pill standing bolt upright and sliding along the
  pavement.

  A pool is one `InstancedMesh` with a free list. Everything that is a box in
  this game -- a torso, a thigh, a car door -- comes out of the same pool and
  costs one draw call between them all. What separates a person from a cow is
  then only which slots it claims and what it writes into them, which is data
  rather than code.

  Unused slots are scaled to nothing rather than removed: an InstancedMesh has
  a fixed count, renumbering it would invalidate every slot anyone is holding,
  and a degenerate triangle costs the rasteriser nothing."
  (:require ["three" :as three]))

(def ^:private x-axis (three/Vector3. 1 0 0))

(defn- hide! [^js mesh ^js scratch i]
  (.set (.-position scratch) 0 -10000 0)
  (.set (.-scale scratch) 0 0 0)
  (.updateMatrix scratch)
  (.setMatrixAt mesh i (.-matrix scratch)))

(defn pool
  "One instanced shape with `capacity` slots, all free."
  [^js scene ^js geometry ^js material capacity {:keys [cast? receive?]
                                                 :or {cast? true receive? false}}]
  (let [^js mesh (three/InstancedMesh. geometry material capacity)
        ^js scratch (three/Object3D.)
        free (js/Int32Array. capacity)]
    (set! (.-frustumCulled mesh) false)
    (set! (.-castShadow mesh) cast?)
    (set! (.-receiveShadow mesh) receive?)
    (dotimes [i capacity]
      ;; Stack, so the first claim gets slot 0 and the instances a crowd
      ;; occupies stay near the front of the buffer.
      (aset free i (- capacity 1 i))
      (hide! mesh scratch i))
    (.setColorAt mesh 0 (three/Color. 0xffffff))
    (.add scene mesh)
    {:mesh mesh :scratch scratch :colour (three/Color.)
     :free free :n (volatile! capacity) :capacity capacity}))

(defn claim!
  "A free slot, or -1 when the pool is full. A full pool draws nothing extra
  rather than throwing: running out of pedestrians is a worse bug than a
  pedestrian that is not drawn."
  [{:keys [^js free n]}]
  (if (zero? @n)
    -1
    (do (vswap! n dec) (aget free @n))))

(defn release! [{:keys [^js mesh ^js scratch ^js free n capacity]} slot]
  (when (and (>= slot 0) (< @n capacity))
    (hide! mesh scratch slot)
    (aset free @n slot)
    (vswap! n inc)))

(defn set-matrix! [{:keys [^js mesh]} slot ^js m4]
  (when (>= slot 0) (.setMatrixAt mesh slot m4)))

(defn set-colour! [{:keys [^js mesh ^js colour]} slot hex]
  (when (>= slot 0)
    (.setHex colour hex)
    (.setColorAt mesh slot colour)))

(defn flush!
  "Tell the GPU the buffers moved. Once per pool per frame, not once per slot."
  [{:keys [^js mesh]}]
  (set! (.-needsUpdate (.-instanceMatrix mesh)) true)
  (when-let [ic (.-instanceColor mesh)] (set! (.-needsUpdate ic) true)))

(defn used [{:keys [n capacity]}] (- capacity @n))

(defn dispose! [{:keys [^js mesh]} ^js scene]
  (.remove scene mesh)
  (.dispose mesh))

;; --- composing a figure from parts ------------------------------------------
;;
;; A rig is flattened into typed arrays once, at build time. The first version
;; read each part out of a Clojure map every frame and built its transform with
;; two `Object3D`s; that was six map lookups and four matrix compositions per
;; part, and with two hundred figures of seven parts each it cost 2.8 ms a
;; frame -- more than the entire render.

(def ^:const part-stride 9)   ; px py pz  sx sy sz  drop swing phase

(defn- write-local!
  "The local transform of one part: scaled, leaned by `angle` about the X axis
  through its pivot, and hung `drop` below it.

  Written straight into the sixteen elements rather than composed from a
  position, a quaternion and a scale. Column-major, so elements 0-2 are the
  first column."
  [^js m angle drop sx sy sz px py pz]
  (let [c (js/Math.cos angle) s (js/Math.sin angle)
        ^js e (.-elements m)]
    (aset e 0 sx) (aset e 1 0.0) (aset e 2 0.0) (aset e 3 0.0)
    (aset e 4 0.0) (aset e 5 (* c sy)) (aset e 6 (* s sy)) (aset e 7 0.0)
    (aset e 8 0.0) (aset e 9 (* (- s) sz)) (aset e 10 (* c sz)) (aset e 11 0.0)
    (aset e 12 px) (aset e 13 (+ py (* c drop))) (aset e 14 (+ pz (* s drop)))
    (aset e 15 1.0)
    m))

(defn rig
  "Flatten a list of parts into what `place-rig!` wants.

  `:locals` holds a prebuilt matrix for every part that never moves, which is
  most of them -- a torso and a bonnet are rigid, only limbs and wheels are not."
  [parts]
  (let [n (count parts)
        nums (js/Float32Array. (* n part-stride))
        modes (js/Int8Array. n)
        shapes (make-array n)
        locals (make-array n)]
    (dotimes [i n]
      (let [{:keys [shape at size drop swing phase spin? tilt]} (nth parts i)
            [px py pz] at
            [sx sy sz] size
            o (* i part-stride)]
        ;; 0 fixed, 1 leaning back and forth, 2 turning continuously. A limb
        ;; swings; a wheel does not.
        (aset modes i (cond spin? 2 (pos? (or swing 0.0)) 1 :else 0))
        (aset nums (+ o 0) px) (aset nums (+ o 1) py) (aset nums (+ o 2) pz)
        (aset nums (+ o 3) sx) (aset nums (+ o 4) sy) (aset nums (+ o 5) sz)
        (aset nums (+ o 6) (or drop 0.0))
        (aset nums (+ o 7) (or swing 0.0))
        (aset nums (+ o 8) (or phase 0.0))
        (aset shapes i shape)
        ;; A rigid part may still be *tilted*: baked into its prebuilt matrix,
        ;; so it costs nothing per frame. This is what stops a car being a pile
        ;; of cubes -- a bonnet that slopes and a windscreen that rakes are the
        ;; two angles the eye reads a car by, and both are a box turned a few
        ;; degrees about X.
        (aset locals i
              (when (zero? (aget modes i))
                (write-local! (three/Matrix4.) (or tilt 0.0) (or drop 0.0)
                              sx sy sz px py pz)))))
    {:n n :nums nums :modes modes :shapes shapes :locals locals :parts (vec parts)}))

(defn body-matrix!
  "The figure's own transform, from a rigid body."
  [^js out ^js pos ^js quat ^js one ^js body]
  (let [t (.translation body) r (.rotation body)]
    (.set pos (.-x t) (.-y t) (.-z t))
    (.set quat (.-x r) (.-y r) (.-z r) (.-w r))
    (.compose out pos quat one)))

(defn place-rig!
  "Write every part of one figure into its slots.

  `meshes` is a JS array parallel to the rig's shapes, holding the pool mesh
  each part belongs to -- resolved once when the figure spawns rather than
  looked up per part per frame."
  [{:keys [n ^js nums ^js modes ^js locals]} ^js meshes ^js slots ^js body-m
   ^js local-m ^js out-m phase]
  (dotimes [i n]
    (let [^js local (aget locals i)
          ^js m (if local
                  local
                  (let [o (* i part-stride)
                        swing (aget nums (+ o 7))
                        off (aget nums (+ o 8))]
                    (write-local! local-m
                                  (if (= 2 (aget modes i))
                                    (+ phase off)
                                    (* swing (js/Math.sin (+ phase off))))
                                  (aget nums (+ o 6))
                                  (aget nums (+ o 3)) (aget nums (+ o 4)) (aget nums (+ o 5))
                                  (aget nums (+ o 0)) (aget nums (+ o 1)) (aget nums (+ o 2)))))
          slot (aget slots i)]
      (when (>= slot 0)
        (.multiplyMatrices out-m body-m m)
        (let [^js mesh (aget meshes i)]
          (.setMatrixAt mesh slot out-m))))))
