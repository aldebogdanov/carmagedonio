# carmagedonio

Carmageddon-style vehicular carnage in an endless procedurally generated world.
Single player now, built so multiplayer is an addition rather than a rewrite.

* Frontend: ClojureScript + three.js + Rapier (Rust/WASM physics)
* Backend: Clojure
* Shared: `.cljc` — the world generator, the PRNG and the wire codec compile to
  both, and are asserted to produce **bit-identical** results on JVM and JS.

## Run it

```bash
npm install
```

```bash
npx shadow-cljs watch client
```

Then open <http://localhost:8080>. WASD/arrows drive, space is the handbrake,
R resets.

The Clojure server is separate and not needed to play yet:

```bash
clojure -M:server
```

It serves the client and an EDN API on :3000, persisting to `data/carmagedonio.edn`
(override with `CARM_DB`). The game stays fully playable without it.

## Tests

```bash
clojure -M:test
```

```bash
npx shadow-cljs compile test && node target/test/shared.js
```

Vehicle characterisation — scripted manoeuvres against the real physics, no
browser needed, so handling is CI-testable:

```bash
npx shadow-cljs compile testbed && node target/testbed.js
```

Both run the *same* `.cljc` suite. `prng_test.cljc` asserts hard-coded golden
values on both runtimes — if it ever passes on one and fails on the other, the
shared-worldgen premise is broken and chunk generation would desync.

## Layout

```
src/shared/   .cljc — runs on both. PRNG, noise, worldgen, rules, wire, interp.
src/client/   .cljs — sim, vehicle, ai, chunks, props, buildings, peds, game,
              render, textures, input, clock, net, worker, testbed.
src/server/   .clj  — HTTP API, websocket sessions, validation, persistence.
test/shared/  .cljc — run under both runtimes.
```

## Architecture decisions worth knowing

**The server will never be the physics authority.** No JVM physics engine agrees
with Rapier bit-for-bit, so a Clojure server cannot reproduce client motion.
Clients own their own vehicle and broadcast state; the server owns *rules*
(score, pickups, spawns, timers) and validates the implausible (teleports,
impossible speeds). If cheating ever matters more than simplicity, the escape
hatch is a Node/Bun sidecar running the same Rapier build, orchestrated by
Clojure — `carmageddon.client.net/Transport` exists so that swap does not reach
into gameplay code.

**Physics determinism is therefore not a requirement, and we do not pay for it.**
Only *world generation* must be deterministic, and that is pure `.cljc`.

**Chunks derive everything from `world-seed + coordinates`**, never from
generation order — so two machines agree on the world with zero coordination.
Chunk boundaries are stitched via `prng/edge-seed`, which canonicalises the two
neighbouring chunk coordinates so both sides independently compute the same
road portal points. Terrain agrees at seams to under a nanometre, asserted in
`worldgen_test.cljc`.

**Chunk generation is budgeted, not threaded — for now.** Measured at ~2.4 ms
per chunk, so two per frame stays well inside a frame. That revises the original
plan, which assumed a Web Worker was mandatory; the measurement said otherwise.
Generation goes through an injected `generate` fn in `chunks/create`, so a
Worker-backed implementation drops in unchanged when M3's buildings and props
make chunks expensive enough to need it.

**Colliders cover a smaller radius than meshes.** You can see much further than
you can crash into, and heightfield colliders are the expensive half. Chunks
gain and lose colliders as the player moves — see `reconcile-colliders!`. Props
live and die with that same physics payload, so they exist exactly where the
player can reach them.

**Opponents drive through `input/->Command`, exactly as the player does.**
That is the payoff for making input a value in M0: the AI, the player and the
test harness all reach the simulation by the same path, and an opponent that
handled differently would be a different game.

**The rules are shared, so the server can recompute rather than believe.**
`carmageddon.shared.rules` holds scoring, the target and the clock, and both
sides use it: the client to play, the server to check the client's arithmetic. A
score only the client knows how to derive is a score the server can only accept
on trust.

**Game rules live outside `sim`.** None of scoring, the clock or win/lose is
physics, and keeping them apart means the measurement harness can drive a
vehicle with no game attached. It is also the part a client should not be
trusted with in M6, so it sits behind one namespace with a narrow surface.

**Destruction is a sparse overlay, never a mutation.** A chunk stays a pure
function of its seed; what has happened to it is a per-chunk set of destroyed
prop indices. Drive away, let the chunk unload, come back, and the delta is
re-applied to freshly generated data. It is also precisely the payload
multiplayer will need to sync — a few integers per chunk.

**Contact force is not evidence of a crash.** Gating destruction on force alone
destroyed a sixth of the world while the player was parked: props settling on
uneven ground and resolving overlaps routinely produce larger forces than a car
clipping one. Destruction requires the *car* to be involved and moving.

**Fixed 60 Hz simulation, decoupled from rendering.** Variable-dt physics is
non-reproducible, makes collision response frame-rate dependent, and cannot be
reconciled against a server. `clock/start!` consumes whole ticks and hands the
renderer an `alpha` to interpolate the remainder.

**All input becomes a tick-stamped `Command`**, even in single player. AI cars
and networked players will drive through the identical code path.

**No persistent data structures in the hot loop.** Entity transforms live in
`Float32Array`s. Clojure data is for config, world metadata and UI.

**Textures are painted at runtime, never shipped as files.** Browser-only and
asset-free: no fetches, nothing to cache-bust, no binaries in the repo, and the
same seeded PRNG that drives worldgen drives the textures.

**The ground is two scales.** A tiled grain texture supplies near-field detail
(without it there is no optical flow and speed is invisible), while macro
variation comes from per-vertex colour, which never repeats. Anything large
enough to recognise in the tiled texture reads as a grid across the plane.

**We wrote our own tyre model** rather than using Rapier's vehicle controller.
See below — this was measured, not assumed.

## Milestones

- [x] **M0** — skeleton, fixed-timestep loop, Rapier scene, raycast vehicle,
      cross-platform PRNG.
- [x] **M1a** — tyre model, measurement harness, textured ground for speed cues.
- [ ] **M1b** — the part only you can do: drive it and tune `sim/tuning` until
      it is *fun*. The numbers below are a floor, not an answer.
- [x] **M2** — infinite world. Chunk streaming, heightfield terrain, connected
      road network stitched across chunk boundaries.
- [x] **M3** — city biome, buildings, breakable props, collision damage, sparse
      chunk deltas.
- [x] **M4** — pedestrians, scoring, timer, win/lose, AI opponents.
- [x] **M5** — backend earns its keep: seed registry, profiles, run history,
      server-side validation.
- [x] **M6** — multiplayer: binary WebSocket protocol, remote proxies,
      entity interpolation, server-side validation and scoring.

## Why the tyre model is ours

Rapier ships a `DynamicRayCastVehicleController`. We measured it before
replacing it, and it failed on two counts:

* **Braking was not limited by grip.** Sweeping its `frictionSlip` from 0.8 to
  3.2 changed cornering grip from 0.62 g to 2.07 g and left the 100–0 braking
  distance at 26 m throughout. Stopping from 100 km/h in 26 m on ice is not a
  tuning problem.
* **No controllable slide.** Its friction is a hard clamp, so grip is either
  fully present or fully gone. Above `frictionSlip` 1.4 a slide collapsed to
  zero sideslip within 1.5 s; below 1.1 the car simply spun to 112°. There is no
  setting in between, because there is no falloff region.

It did have a working friction circle and real weight transfer — that part of
the earlier assumption was wrong, and worth recording.

`carmageddon.client.vehicle` keeps the raycast approach and Rapier's rigid
bodies, and replaces the force calculation with a slip-based model: slip ratio and slip
angle feed Magic-Formula-shaped curves that peak and then decay, a friction
ellipse makes longitudinal and lateral share one budget of `mu * Fz`, and the
tyre's reaction torque drives wheel spin so wheels can lock and light up.

Emergent behaviour worth noting: raising `:engine-torque` past 1200 N·m makes
the car *slower* to 100 km/h (6.3 s → 8.1 s), because the extra torque spins the
wheels past the slip peak. Nothing implements that; it falls out of the model.

## Current numbers

Run the testbed to regenerate. Defaults as shipped:

| | |
|---|---|
| 0–100 km/h | 6.28 s, 93 m |
| 100–0 km/h | 32 m, 2.45 s |
| Weight transfer under braking | 9.8 cm of suspension travel |
| Skidpad, 80 km/h | 0.85 g, 31 m radius |
| Grip surrendered when braking at the cornering limit | 91% |
| Handbrake slide from 60 km/h | peaks at 27°, builds steadily |
| 30 s of throttle + steering + handbrake | finite, upright, four wheels down |
| Chunk generation | 2.4 ms |
| Heightfield collider vs analytic surface | 0.03 m mean, 0.12 m max |
| Airborne while following roads at speed | 0.6% of ticks |
| Main-thread cost per chunk (worker) | 0.45 ms (was 19.4 ms) |
| Props live around the player | 182 (13 chunks x 14) |
| Buildings standing in a city | ~140 |
| Pedestrians around the player | ~234 |
| Sim cost with 149 buildings + 182 props | 0.84 ms/tick |

## Current numbers

Run the testbed to regenerate. Defaults as shipped:

| | |
|---|---|
| 0–100 km/h | 6.28 s, 93 m |
| 100–0 km/h | 32 m, 2.45 s |
| Weight transfer under braking | 9.8 cm of suspension travel |
| Skidpad, 80 km/h | 0.85 g, 31 m radius |
| Grip surrendered when braking at the cornering limit | 91% |
| Handbrake slide from 60 km/h | peaks at 27°, builds steadily |
| 30 s of throttle + steering + handbrake | finite, upright, four wheels down |
| Chunk generation | 2.4 ms |
| Heightfield collider vs analytic surface | 0.03 m mean, 0.12 m max |
| Airborne while following roads at speed | 0.6% of ticks |
| Main-thread cost per chunk (worker) | 0.45 ms (was 19.4 ms) |
| Props live around the player | 182 (13 chunks x 14) |
| Buildings standing in a city | ~140 |
| Pedestrians around the player | ~234 |
| Sim cost with 149 buildings + 182 props | 0.84 ms/tick |

## World generation

`carmageddon.shared.worldgen` is pure and runs identically on both platforms.

* **Layout is hub-and-spoke.** One hub near each chunk's middle, one portal on
  each of the four edges, a bowed road from each portal to the hub. That
  guarantees a connected network in every direction forever.
* **Terrain conforms to roads**, not the other way round. Roads take their
  height from the unroaded terrain at their endpoints and interpolate along
  their length — cut and fill to a gentle grade — and the surface blends back
  out to open ground across a shoulder.
* **Road influence is computed over the 3×3 chunk neighbourhood.** A road just
  over the border still cuts terrain on this side; considering only local roads
  would put a visible step at every boundary.
* **Biome is a hard per-chunk decision**, from noise over chunk coordinates.
  Country chunks are hub-and-spoke; city chunks are a street grid plus a stub
  from each edge portal to the nearest street. The stubs are what keep a city
  stitchable -- the grid alone would not reach the portals, and portals are the
  only thing neighbouring chunks agree on.
* **Ground colour uses a smooth field, not the biome boolean.** `urbanness`
  samples the same noise continuously in world space, so concrete fades into
  grass instead of drawing a seam along every chunk boundary where city meets
  country. At a chunk origin it agrees with `biome` exactly.
* **Buildings are rejected near streets** rather than fitted analytically into
  blocks. Cheaper, and it keeps working when the road layout changes.
* Highway corridors and other topologies slot in by extending `spokes`.

Three bugs found here are worth knowing about, because each was invisible in one
of the two systems:

* **The heightfield index order was wrong.** Rapier wants `heights[x*n + z]`.
  The alternatives were wrong by up to 7 m, which still looks like perfectly
  plausible terrain but leaves the car floating or sunk. Established by
  raycasting a real collider and comparing against worldgen, not by reading
  parry's source.
* **Terrain rendered invisible while physics worked fine.** Triangle winding was
  reversed, so every computed normal pointed down and front-face culling hid the
  entire world.
* **Colliders silently disappeared while driving.** A chunk first loaded at the
  visual radius never gained a collider as the player approached, so the inner
  ring emptied out and the car fell through the world.

## World generation

`carmageddon.shared.worldgen` is pure and runs identically on both platforms.

* **Layout is hub-and-spoke.** One hub near each chunk's middle, one portal on
  each of the four edges, a bowed road from each portal to the hub. That
  guarantees a connected network in every direction forever.
* **Terrain conforms to roads**, not the other way round. Roads take their
  height from the unroaded terrain at their endpoints and interpolate along
  their length — cut and fill to a gentle grade — and the surface blends back
  out to open ground across a shoulder.
* **Road influence is computed over the 3x3 chunk neighbourhood.** A road just
  over the border still cuts terrain on this side; considering only local roads
  would put a visible step at every boundary.
* **Props are placed along this chunk's own roads**, then pushed sideways until
  clear of a carriageway. Scattering over the chunk and rejecting misses wasted
  three quarters of every batch, and offsetting once was not enough — near the
  hub all four spokes converge, so stepping clear of one road lands on another.
  Every candidate draws its randomness *before* being accepted, so the stream
  advances identically regardless of terrain.
* **Biome is a hard per-chunk decision**, from noise over chunk coordinates.
  Country chunks are hub-and-spoke; city chunks are a street grid plus a stub
  from each edge portal to the nearest street. The stubs are what keep a city
  stitchable -- the grid alone would not reach the portals, and portals are the
  only thing neighbouring chunks agree on.
* **Ground colour uses a smooth field, not the biome boolean.** `urbanness`
  samples the same noise continuously in world space, so concrete fades into
  grass instead of drawing a seam along every chunk boundary where city meets
  country. At a chunk origin it agrees with `biome` exactly.
* **Buildings are rejected near streets** rather than fitted analytically into
  blocks. Cheaper, and it keeps working when the road layout changes.
* Highway corridors and other topologies slot in by extending `spokes`.

## Bugs worth remembering

Each of these was invisible in one of the two systems that had to agree:

* **Heightfield index order.** Rapier wants `heights[x*n + z]`. The alternatives
  were wrong by up to 7 m, which still looks like plausible terrain but leaves
  the car floating. Established by raycasting a real collider and comparing
  against worldgen, not by reading parry's source.
* **Terrain rendered invisible while physics worked fine.** Triangle winding was
  reversed, so every computed normal pointed down and front-face culling hid the
  entire world.
* **Colliders silently disappeared while driving.** A chunk first loaded at the
  visual radius never gained a collider as the player approached, so the inner
  ring emptied out and the car fell through the world.
* **Prop meshes all sat at the origin.** Their bodies were correct, so physics
  and destruction worked perfectly; only the picture was wrong. Dynamic props
  also need syncing every frame, unlike static scenery.
* **A per-chunk material leaked GPU resources**, the same way per-chunk
  geometry would without `dispose`.
* **`spawn-point` returned the hub**, which is only on a road in country
  chunks. Once the origin chunk generated as a city, the car spawned inside a
  block. It now takes a point from an actual road polyline.
* **Urban ground was tinted the same value as asphalt**, so streets vanished
  into the pavement and a city read as one flat slab.
* **The AI steering sign was inverted**, so cars orbited their target at full
  lock forever instead of reaching it. It cost two milestones' worth of useless
  "hunt" tests before being caught -- and it was settled by measuring which sign
  closed the distance, not by reasoning about handedness.
* **Target selection scanned every pedestrian, per driver, per tick** -- about
  1.8M scans over a half-minute run, which dominated everything else. Targets
  are now re-picked every 30 ticks, staggered between drivers.

## The game

`carmageddon.client.game` owns the run: a 90 second clock, points and seconds
for what you hit, and win/lose. Pedestrians are worth real time (3s each);
scenery is worth almost none (0.4s), so smashing crates cannot substitute for
playing. Wrecking a rival is worth 12s.

Pedestrians are capsules with their rotations locked, which is what keeps them
upright with no balance model at all -- they cannot fall over because they
cannot rotate. On death the lock comes off and they become ordinary debris.
That switch is the entire ragdoll: no joints, no skeleton.

**The kill target is unplaytested.** An AI hunting pedestrians manages roughly
one every eight seconds, which put the original 40 out of reach; it is set to 25
pending a real session.

## The backend

Through M4 the server did essentially nothing. It now owns the things a client
should not be trusted with.

| | |
|---|---|
| `GET /api/health` | liveness |
| `GET /api/rules` | the scoring table, so clients need no second copy |
| `GET/POST /api/worlds` | seed registry — a world *is* its seed |
| `GET /api/worlds/:id/leaderboard` | top runs |
| `POST /api/profiles`, `GET /api/profiles/:id/runs` | identity and history |
| `POST /api/runs` | submit a finished run — **validated** |

**What validation can and cannot be.** The server cannot reproduce a client's
physics; that was settled in M0 and has not changed. So it does not pretend to.
It checks what it can: that the score matches the tally (recomputed, never
read), that a claimed win actually reached the target, and that the run did not
last longer than the clock could possibly have allowed. That catches a client
editing its score without also faking a coherent run around it, which is the
realistic case. It does not catch a patient, internally-consistent liar, and
claiming otherwise would be worse than not checking.

Storage is a single EDN document rewritten atomically behind a protocol. That is
adequate for one process and keeps the project free of database infrastructure
it does not need; a JDBC implementation drops in without anything above
`carmageddon.server.store` noticing.

**The backend is optional.** Every client call degrades to nil rather than
throwing, because the dev loop serves the client from shadow-cljs with no API
behind it. A backend you must run to play would make the fastest iteration loop
the one that skips it.

## Multiplayer

Join a world at `ws://host/ws/:world-id`. Frames are binary
(`carmageddon.shared.wire`): a car snapshot is **30 bytes**, sent 25 times a
second. The same data as EDN is closer to 200, which at that rate is the
difference between framing and payload.

**What crosses the network is tiny, and that is the whole design.** No terrain,
roads, buildings, props or pedestrians are ever sent -- every client derives
them from the seed. The only world traffic is a 12-byte delta saying a
particular prop or pedestrian is now gone.

**Remote cars are kinematic bodies rendered in the past.** Snapshots arrive
jittered at 25 Hz; the display wants 60+. Rather than predicting forward, each
proxy is drawn `interp-delay-ms` behind, almost always bracketed by two real
snapshots. Kinematic rather than dynamic because its position comes from the
network -- but it still shoves the player's car, which in this game is the
point. The cost is that two clients disagree slightly about a heavy shunt; that
was accepted in M0 when the server stopped being the physics authority.

**What the server actually enforces.** It cannot reproduce a client's physics,
so it does not pretend to. It drops snapshots that teleport or claim impossible
speeds, relays under its own player ids rather than whatever the client put in
the frame, and counts kills itself from the deltas it accepted. Measured against
a live server: of four snapshots where two were a 400 m/s cheat and a 1 km
teleport, peers received exactly the two legitimate ones, and the scoreboard
read `{:peds 3 :props 1 :score 715}` computed by the server through the shared
rules. It still will not catch a patient, internally-consistent liar.

## Tuning

Everything lives in one atom, `carmageddon.client.sim/tuning`, read fresh every
tick — `swap!` it from the REPL and the change takes effect immediately. The
testbed's `sweep!` varies one key across a range and prints what it does to
grip, acceleration, braking and slide behaviour.

The HUD shows live sideslip angle, which is the number to watch: near zero is
gripping, steady and large is a drift, growing without bound is a spin.

Knobs most worth your time, in order: `:grip` (overall adhesion — sweeps show
slides sustain best at the top of the range), `:handbrake-torque` (currently
scrubs a lot of speed; lower it for power-on drifts), `:lat-B` / `:lat-E`
(how sharply the tyre lets go past its peak), and `:grip-rear-bias` (tightens
turn-in, but measurably shortens slides and costs acceleration, since the rear
wheels are the driven ones — left neutral by default).

Wheel visuals are parented to the chassis mesh, so suspension travel is visible
while tuning — watch it compress under braking and load up in corners.
