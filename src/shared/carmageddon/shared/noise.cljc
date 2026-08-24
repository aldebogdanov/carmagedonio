(ns carmageddon.shared.noise
  "Deterministic value noise, shared by client and server.

  Built on `prng/hash-coords` rather than a stateful generator: the value at a
  lattice point is a pure function of (seed, x, y), so chunks can be generated
  in any order, on any machine, in any thread, and still agree. That property is
  the whole reason worldgen lives in .cljc.

  Value noise rather than simplex: it is cheaper, it is trivially identical
  across platforms, and terrain detail here comes from octave stacking and the
  road network, not from the noise basis being fancy."
  (:require [carmageddon.shared.prng :as prng]))

(defn- lattice
  "Deterministic value in [0.0, 1.0) at integer lattice point (x, y).
  Uses the low 24 bits so the result is exact in a double on both platforms."
  [seed x y]
  (/ (double (bit-and (prng/hash-coords seed x y) 0xFFFFFF)) 16777216.0))

(defn- smootherstep
  "Ken Perlin's 6t^5 - 15t^4 + 10t^3. Zero first and second derivative at the
  ends, so octaves stack without visible lattice creases."
  [t]
  (* t t t (+ 10.0 (* t (- (* t 6.0) 15.0)))))

(defn- fast-floor [x]
  (let [i (long x)]
    (if (< x (double i)) (dec i) i)))

(defn value2d
  "Smoothly interpolated value noise in [0.0, 1.0). One unit = one lattice cell."
  [seed x y]
  (let [x0 (fast-floor x)
        y0 (fast-floor y)
        fx (smootherstep (- x (double x0)))
        fy (smootherstep (- y (double y0)))
        v00 (lattice seed x0 y0)
        v10 (lattice seed (inc x0) y0)
        v01 (lattice seed x0 (inc y0))
        v11 (lattice seed (inc x0) (inc y0))
        a (+ v00 (* (- v10 v00) fx))
        b (+ v01 (* (- v11 v01) fx))]
    (+ a (* (- b a) fy))))

(defn fbm2d
  "Fractional Brownian motion: `octaves` layers of value2d, each double the
  frequency and `gain` times the amplitude. Returns [0.0, 1.0).

  Each octave gets its own derived seed so the layers do not correlate -- reusing
  one seed at different frequencies produces visible self-similar artefacts."
  ([seed x y octaves] (fbm2d seed x y octaves 0.5 2.0))
  ([seed x y octaves gain lacunarity]
   (loop [i 0, freq 1.0, amp 1.0, sum 0.0, norm 0.0]
     (if (< i octaves)
       (recur (inc i)
              (* freq lacunarity)
              (* amp gain)
              (+ sum (* amp (value2d (prng/mix32 (+ seed i)) (* x freq) (* y freq))))
              (+ norm amp))
       (if (zero? norm) 0.0 (/ sum norm))))))

(defn ridged
  "1 - |2n - 1|: folds the noise about its midpoint to make creases. Useful for
  ridgelines; kept here so terrain code has it in M2."
  [seed x y octaves]
  (- 1.0 (abs (- (* 2.0 (fbm2d seed x y octaves)) 1.0))))
