(ns carmageddon.shared.prng
  "Deterministic PRNG that produces bit-identical output on the JVM and in JS.

  This is load-bearing: the whole world is derived from a seed, and the client
  and the server must agree on every chunk without exchanging a single byte.
  That only holds if every arithmetic op below is exactly 32-bit on both
  platforms.

  Rules for anything in this namespace:
    * values are always signed int32, kept in a long (JVM) or a JS number
    * never use `*` on two random words -- a 32x32 multiply overflows the 53-bit
      double mantissa in JS. Use `imul32`.
    * never use `unsigned-bit-shift-right` with a shift of 0 in CLJS: it yields
      a uint32 that no longer fits int32.

  Algorithm is xoshiro128** (Blackman/Vigna, public domain), seeded via
  splitmix32.")

;; ---------------------------------------------------------------------------
;; 32-bit primitives

(defn i32
  "Truncate to signed 32-bit."
  [x]
  #?(:clj  (unchecked-int x)
     :cljs (bit-or x 0)))

(defn imul32
  "32-bit multiply, low word, signed. The single most important fn here."
  [a b]
  #?(:clj  (unchecked-int (unchecked-multiply (long a) (long b)))
     :cljs (js/Math.imul a b)))

(defn shl32 [x k]
  #?(:clj  (unchecked-int (bit-shift-left (long x) k))
     :cljs (bit-shift-left x k)))

(defn shr32
  "Logical (zero-fill) right shift. `k` must be >= 1."
  [x k]
  #?(:clj  (unchecked-int (bit-shift-right (bit-and (long x) 0xFFFFFFFF) k))
     :cljs (unsigned-bit-shift-right x k)))

(defn rotl32 [x k]
  (i32 (bit-or (shl32 x k) (shr32 x (- 32 k)))))

;; Golden-ratio / murmur mixing constants, written as signed int32 so they need
;; no coercion at read time.
(def ^:private c-gr   -1640531527) ; 0x9E3779B9
(def ^:private c-gr1  -1640531535) ; 0x9E3779B1
(def ^:private c-mur   -2048144777) ; 0x85EBCA77
(def ^:private c-sm1   568395693)  ; 0x21F0AAAD
(def ^:private c-sm2   1935289751) ; 0x735A2D97

(defn mix32
  "splitmix32 finalizer. Avalanches an int32 into an int32."
  [x]
  (let [z (i32 x)
        z (bit-xor z (shr32 z 16))
        z (imul32 z c-sm1)
        z (bit-xor z (shr32 z 15))
        z (imul32 z c-sm2)
        z (bit-xor z (shr32 z 15))]
    z))

;; ---------------------------------------------------------------------------
;; State

(defn- new-state []
  #?(:clj  (int-array 4)
     :cljs (js/Int32Array. 4)))

(defn- g  [s i]   #?(:clj (aget ^ints s i)        :cljs (aget s i)))
(defn- p! [s i v] #?(:clj (aset ^ints s i (int v)) :cljs (aset s i v)))

(defn make
  "Build a generator state from an integer seed. Mutable -- do not share across
  threads or across chunk generation tasks; make a fresh one per task."
  [seed]
  (let [s (new-state)]
    (loop [i 0, x (i32 seed)]
      (when (< i 4)
        (let [x' (i32 (+ x c-gr))]
          (p! s i (mix32 x'))
          (recur (inc i) x'))))
    ;; xoshiro is stuck on an all-zero state; astronomically unlikely here, but
    ;; the guard is free.
    (when (and (zero? (g s 0)) (zero? (g s 1))
               (zero? (g s 2)) (zero? (g s 3)))
      (p! s 0 1))
    s))

(defn next-i32!
  "Advance the state, return the next word as signed int32."
  [s]
  (let [s0 (g s 0), s1 (g s 1), s2 (g s 2), s3 (g s 3)
        result (imul32 (rotl32 (imul32 s1 5) 7) 9)
        t      (shl32 s1 9)
        n2     (bit-xor s2 s0)
        n3     (bit-xor s3 s1)
        n1     (bit-xor s1 n2)
        n0     (bit-xor s0 n3)
        n2     (bit-xor n2 t)
        n3     (rotl32 n3 11)]
    (p! s 0 n0) (p! s 1 n1) (p! s 2 n2) (p! s 3 n3)
    result))

(defn next-u32!
  "Next word as an unsigned value in [0, 2^32)."
  [s]
  #?(:clj  (bit-and (long (next-i32! s)) 0xFFFFFFFF)
     :cljs (unsigned-bit-shift-right (next-i32! s) 0)))

(defn next-double!
  "Uniform double in [0.0, 1.0)."
  [s]
  (/ (double (next-u32! s)) 4294967296.0))

(defn next-int!
  "Uniform-ish int in [0, n). Uses modulo, so there is a bias of at most
  n / 2^32 -- irrelevant for world layout, and identical on both platforms,
  which is what actually matters."
  [s n]
  (mod (next-u32! s) n))

(defn next-range!
  "Uniform double in [lo, hi)."
  [s lo hi]
  (+ lo (* (next-double! s) (- hi lo))))

(defn next-bool!
  ([s] (next-bool! s 0.5))
  ([s p] (< (next-double! s) p)))

(defn pick!
  "Deterministically choose one element of an indexed collection."
  [s coll]
  (nth coll (next-int! s (count coll))))

(defn shuffled
  "Deterministic Fisher-Yates. Returns a vector; does not mutate `coll`."
  [s coll]
  (let [v (object-array (vec coll))]
    (loop [i (dec (alength v))]
      (when (pos? i)
        (let [j (next-int! s (inc i))
              a (aget v i)]
          (aset v i (aget v j))
          (aset v j a)
          (recur (dec i)))))
    (vec v)))

;; ---------------------------------------------------------------------------
;; Spatial seeds
;;
;; Every generated thing derives its seed from world-seed + its own coordinates,
;; never from generation order. That is what makes chunks independent and lets
;; two machines agree with zero coordination.

(defn hash-coords
  "Mix a world seed with 2D integer coordinates into an int32."
  [seed x z]
  (-> (i32 seed)
      (bit-xor (imul32 (i32 x) c-gr1))
      mix32
      (bit-xor (imul32 (i32 z) c-mur))
      mix32))

(defn chunk-seed
  "Seed for chunk (cx, cz). `salt` lets one chunk run several independent
  streams (terrain / roads / props) without them correlating."
  ([seed cx cz] (hash-coords seed cx cz))
  ([seed cx cz salt] (mix32 (bit-xor (hash-coords seed cx cz) (imul32 (i32 salt) c-sm1)))))

(defn edge-seed
  "Seed for the boundary *between* two adjacent chunks.

  Canonicalised so both neighbours compute the same value: this is the hook the
  road generator uses to place matching portal points on a shared edge without
  either chunk knowing the other exists."
  [seed ax az bx bz]
  (let [[x1 z1 x2 z2] (if (or (< ax bx) (and (= ax bx) (< az bz)))
                        [ax az bx bz]
                        [bx bz ax az])]
    (-> (hash-coords seed x1 z1)
        (bit-xor (imul32 (hash-coords seed x2 z2) c-gr1))
        mix32)))

(defn chunk-rng
  "Convenience: fresh generator for one chunk / one purpose."
  ([seed cx cz]      (make (chunk-seed seed cx cz)))
  ([seed cx cz salt] (make (chunk-seed seed cx cz salt))))
