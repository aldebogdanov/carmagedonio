(ns carmageddon.shared.constants
  "Values that client and server must agree on. Anything here is part of the
  wire/worldgen contract -- changing a number invalidates saved worlds and
  desyncs old clients, so treat it as versioned data, not tuning knobs.
  Vehicle tuning lives in the client sim, not here.")

;; --- Simulation -------------------------------------------------------------

(def tick-hz 60)
(def dt (/ 1.0 tick-hz))

;; Never advance more than this many sim steps in one frame. Past this we drop
;; the accumulated debt rather than spiral: a slow machine runs in slow motion
;; for a frame instead of locking up.
(def max-steps-per-frame 5)

;; Ignore frame deltas larger than this (tab was backgrounded, breakpoint hit).
(def max-frame-seconds 0.25)

(def gravity [0.0 -9.81 0.0])

;; --- World ------------------------------------------------------------------

(def chunk-size 256.0)      ; metres per chunk edge
(def chunk-verts 33)        ; heightfield resolution per chunk edge (2^n + 1)
(def stream-radius 3)       ; chunks loaded around the player -> ~1.5 km
(def collider-radius 2)     ; chunks with physics colliders (cheaper than visual)

;; Independent PRNG streams within one chunk. Adding a new generator means
;; adding a salt here, never reordering the existing ones.
(def salt
  {:terrain 0
   :roads   1
   :blocks  2
   :props   3
   :peds    4})

;; --- Network ----------------------------------------------------------------

(def snapshot-hz 25)           ; outbound vehicle state rate
(def interp-delay-ms 100.0)    ; remote proxies render this far in the past
(def protocol-version 1)
