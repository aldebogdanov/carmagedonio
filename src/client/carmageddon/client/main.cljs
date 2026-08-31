(ns carmageddon.client.main
  "Wiring only. Every subsystem is independently testable; this namespace exists
  to connect them and owns no game logic."
  (:require [carmageddon.client.api :as api]
            [carmageddon.client.cars :as cars]
            [carmageddon.client.birds :as birds]
            [carmageddon.client.buildings :as buildings]
            [carmageddon.client.camera :as camera]
            [carmageddon.client.chunks :as chunks]
            [carmageddon.client.cockpit :as cockpit]
            [carmageddon.client.fire :as fire]
            [carmageddon.client.clock :as clock]
            [carmageddon.client.furniture :as furniture]
            [carmageddon.client.game :as game]
            [carmageddon.client.input :as input]
            [carmageddon.client.menu :as menu]
            [carmageddon.client.minimap :as minimap]
            [carmageddon.client.net :as net]
            [carmageddon.client.overlay :as overlay]
            [carmageddon.client.parts :as parts]
            [carmageddon.client.peds :as peds]
            [carmageddon.client.powerups :as powerups]
            [carmageddon.client.props :as props]
            [carmageddon.client.traffic :as traffic]
            [carmageddon.client.remote :as remote]
            [carmageddon.client.render :as render]
            [carmageddon.client.rivals :as rivals]
            [carmageddon.client.sim :as sim]
            [carmageddon.client.vehicle :as vehicle]
            [carmageddon.shared.constants :as k]
            [carmageddon.shared.wire :as wire]))

(defonce app (atom nil))

;; Hardcoded to match the server's /api/world/seed for now. M5 fetches it.
(def fallback-seed
  "Used when there is no backend -- the dev server on :8080 serves the client
  without an API behind it, and the game has to stay playable there."
  20260823)

;; Below this the car is nudging, not crashing.
(def ^:private min-smash-speed 4.0)     ; m/s
(def ^:private max-damage-per-hit 0.02)
(def ^:private opponent-count 3)

;; What a gas cylinder and a tanker are worth, as fire.
(def ^:private barrel-fire {:r 4.2 :life 12.0 :seeds 0 :blast 9.0 :push 2400.0})
(def ^:private tanker-fire {:r 8.5 :life 25.0 :seeds 2 :blast 16.0 :push 7000.0})
;; Damage per second at the centre of a pool. Ten seconds parked in one writes
;; a car off; driving through the edge of one costs a few per cent.
(def ^:private burn-rate 0.11)

;; Contact force below this is scraping, not crashing. Set high deliberately:
;; the chassis grinds along terrain constantly when bottoming out or rolling,
;; and counting those wrote the car off inside half a minute of rough driving.
(def ^:private damage-force-floor 26000.0)
(def ^:private damage-force-scale 900000.0)

(defn- hud! [text]
  (when-let [el (js/document.getElementById "hud")]
    (set! (.-textContent el) text)))

(defn- car-snapshot
  "The player's car as the wire wants it. Read straight from the body rather
  than from `telemetry`, which allocates."
  [sim]
  (let [^js b (sim/chassis-body sim)
        t (.translation b)
        r (.rotation b)
        v (.linvel b)]
    {:id 0
     :pos [(.-x t) (.-y t) (.-z t)]
     :quat [(.-x r) (.-y r) (.-z r) (.-w r)]
     :vel [(.-x v) (.-y v) (.-z v)]
     :damage (sim/damage sim)
     ;; Which vehicle, so a remote lorry arrives in someone's mirror as a
     ;; lorry. It rides in the byte the car frame always had spare.
     :kind (cars/kind-index (sim/kind-of sim 0) 0)}))

(defn- apply-inbound!
  "Drain the transport. Everything the network can say about the world arrives
  here: who moved, who left, and what got destroyed."
  [transport remotes props-state peds-state traffic-state bridges powerups now]
  (doseq [msg (net/-poll! transport)]
    (case (:type msg)
      :welcome (remote/set-self! remotes (:player-id msg))
      :state   (doseq [car (:cars msg)] (remote/observe! remotes now car))
      :bye     (remote/forget! remotes (:player-id msg))
      :delta   (let [{:keys [cx cz kind index]} msg
                     key [cx cz]]
                 ;; Someone else destroyed something. Apply it locally so the
                 ;; shared world stays shared -- and record the delta so it
                 ;; survives the chunk unloading and coming back.
                 (case kind
                   :prop    (props/destroy-index! props-state key index)
                   :ped     (peds/kill-index! peds-state key index [0.0 0.0 0.0])
                   :car     (traffic/wreck-index! traffic-state key index)
                   :barrier (parts/smash-index! bridges key index [0.0 0.0 0.0])
                   :pickup  (powerups/take-index-remote! powerups key index)
                   nil))
      nil)))

(defn- start-frame-loop! [{:keys [sim rs transport canvas chunk-mgr props-state
                                  buildings-state furniture-state traffic-state
                                  birds-state peds-state overlay minimap game
                                  bridges rvs cock fire-state powerups-state remotes]}]
  ;; Outbound network rate is deliberately independent of both sim and render
  ;; rate. In single player the loopback swallows these; in M6 the same call
  ;; site emits the binary snapshot.
  (let [snap-every (/ k/tick-hz k/snapshot-hz)
        stats      (atom {:fps 0 :tick 0})]
    (clock/start!
     {:on-tick
      (fn [tick _dt]
        (let [cmd (input/sample tick)]
          (sim/step! sim cmd (rivals/commands rvs sim peds-state tick))
          ;; Traffic drives on the fixed step, like everything else that moves.
          (traffic/drive! traffic-state k/dt (js/Date.now))
          (peds/walk! peds-state tick (sim/player-x sim) (sim/player-z sim))
          (game/tick! game)
          ;; Fire ages on the fixed step like everything else that changes.
          (fire/tick! fire-state k/dt)
          ;; Anything driven over, and whatever holding it does this tick.
          (let [v (sim/player-vehicle sim)]
            (when-let [{:keys [kind delta]} (powerups/collect! powerups-state
                                                              (sim/player-x sim)
                                                              (sim/player-z sim))]
              (cockpit/flash! cock (powerups/apply! powerups-state v kind))
              (net/-send! transport (wire/encode-delta (assoc delta :kind :pickup))))
            (powerups/tick!
             powerups-state v tick k/dt
             (fn [[what x y z r life]]
               (case what
                 ;; The trail is the player's, so anyone who runs into it is
                 ;; worth points to them.
                 :fire (fire/ignite! fire-state x y z r life :player)
                 :shock (doseq [d (concat
                                   (map #(assoc % :kind :car)
                                        (traffic/wreck-near! traffic-state x z r
                                                             [0.0 60.0 0.0]))
                                   (map #(assoc % :kind :ped)
                                        (peds/kill-near! peds-state x z (* 0.6 r))))]
                          (if (= :car (:kind d)) (game/car-wrecked! game) (game/ped-killed! game))
                          (net/-send! transport (wire/encode-delta d)))
                 nil))))
          (let [px (sim/player-x sim) pz (sim/player-z sim)]
            (when-let [{:keys [heat]} (fire/heat-at fire-state px pz)]
              (vehicle/add-damage! (sim/player-vehicle sim) (* burn-rate heat k/dt)))
            ;; Rivals burn too, which is what makes driving one into a pool a
            ;; tactic rather than a waste of a barrel.
            (doseq [v (rest (sim/vehicles sim))]
              (let [[vx _ vz] (vehicle/chassis-position v)]
                (when-let [{:keys [heat]} (fire/heat-at fire-state vx vz)]
                  (vehicle/add-damage! v (* burn-rate heat k/dt))))))
          (doseq [[d owner] (peds/burn! peds-state fire-state tick)]
            (when (= :player owner) (game/ped-killed! game))
            (net/-send! transport (wire/encode-delta (assoc d :kind :ped))))
          ;; Rivals that have lost touch are brought back. Checked every tick,
          ;; but only acts after a few seconds out of contact.
          (let [[fx _ fz] (sim/forward-vector sim)]
            (rivals/leash! rvs sim (js/Math.atan2 fx fz)))
          (dotimes [_ (rivals/score-wrecks! rvs sim)]
            (game/opponent-wrecked! game))
          ;; Outbound snapshot rate is deliberately independent of both sim and
          ;; render rate.
          (when (zero? (mod tick snap-every))
            (net/-send! transport (wire/encode-state tick [(car-snapshot sim)])))
          (apply-inbound! transport remotes props-state peds-state traffic-state
                          bridges powerups-state (js/Date.now))))

      :on-frame
      (fn [alpha dt]
        (render/resize! rs canvas)
        ;; Streaming is driven from the frame, not the tick: it is presentation
        ;; work with a per-frame budget, and tying it to the fixed timestep
        ;; would make world loading stutter whenever the sim caught up.
        ;;
        ;; Deliberately not `sim/telemetry` -- that allocates a map and six
        ;; vectors, which is fine at HUD rate but not 60+ times a second.
        (chunks/update! chunk-mgr (sim/player-x sim) (sim/player-z sim))
        ;; Where the car is and how bent it is belong in the overlay too, so a
        ;; reload puts the player back rather than at the spawn. Debounced
        ;; inside `save!`; this only reads a transform.
        (overlay/set-vehicle! overlay (car-snapshot sim))
        (overlay/save! overlay (js/Date.now))
        (props/sync! props-state)
        (parts/sync! bridges)
        (fire/sync! fire-state (* 0.001 (js/Date.now)))
        (powerups/sync! powerups-state (* 0.001 (js/Date.now)))
        (traffic/sync! traffic-state)
        (birds/update! birds-state (* 0.001 (js/Date.now))
                       (sim/player-x sim) (sim/player-z sim))
        ;; Lights are a pure function of the clock, so this only has to repaint
        ;; instance colours -- there is no signal state to advance.
        (furniture/sync-signals! furniture-state (js/Date.now))
        (peds/sync! peds-state dt)
        (remote/sync! remotes (js/Date.now))
        ;; The cluster repaints every frame. A needle that moves twice a second
        ;; is not a speedometer, it is a readout with extra steps -- and the
        ;; whole reason for drawing instruments is that their movement is what
        ;; carries the value.
        (let [v (sim/player-vehicle sim)
              kind (sim/kind-of sim 0)]
          (cockpit/draw!
           cock
           (cockpit/state-of
            {:speed      (sim/player-speed sim)
             :top-speed  (:top-speed @(cars/tuning kind) 62.0)
             :panels     (vehicle/panels v)
             :damage     (vehicle/damage v)
             :game       (game/summary game)
             :rivals     (rivals/alive rvs)
             :wheels     (sim/wheels-on-ground sim)
             :slip       (js/Math.abs (sim/sideslip-now sim))
             :handbrake? (input/handbrake-held?)
             :online     (remote/count-players remotes)
             :powerups   (powerups/active powerups-state)
             :car        (cars/display-name kind)})))
        (render/draw! rs sim alpha dt))

      ;; HUD is updated twice a second, not per frame. UI state and sim state
      ;; are kept apart on purpose -- when re-frame arrives for menus it
      ;; subscribes to snapshots like this one, never to the sim itself.
      :on-stats
      (fn [s]
        (reset! stats s)
        ;; The map redraws at HUD rate. Twice a second is indistinguishable from
        ;; sixty times a second for a map, and it rasterises 169 cells.
        (let [[fx _ fz] (sim/forward-vector sim)]
          (minimap/draw! minimap (sim/player-x sim) (sim/player-z sim)
                         (js/Math.atan2 fx fz)
                         (when (minimap/rivals-shown? minimap)
                           (rivals/blips rvs sim))
                         (remote/blips remotes)))
        ;; Everything the cluster does not show. This is diagnostics -- counts,
        ;; frame rate, how much of the world is loaded -- and it is deliberately
        ;; small, dim and out of the way, because it used to sit in the same
        ;; line and at the same weight as the player's remaining seconds.
        (let [cs (chunks/stats chunk-mgr)
              pd (peds/stats peds-state)]
          (hud! (str (.toFixed (:fps s) 0) " fps"
                     "   cam " (camera/labels (camera/mode (:camera-state rs)))
                     "   chunks " (:loaded cs) "/" (:colliders cs)
                     (when (pos? (:pending cs)) (str " (+" (:pending cs) ")"))
                     "   peds " (:people pd) "+" (:animals pd)
                     (if (= :outbreak (:mode pd)) " OUTBREAK" "")
                     "   cars " (:driving (traffic/stats traffic-state))
                     "   props " (:live (props/stats props-state))
                     "   panels " (:smashed (parts/stats bridges))
                     (let [f (fire/stats fire-state)]
                       (if (pos? (:pools f)) (str "   fires " (:pools f)) ""))
                     "   crates " (:live (powerups/stats powerups-state))
                     "   saved " (:bytes (overlay/stats overlay)) "B"))))})))

(defn- boot!
  "Build the world and start playing. `world` is the server's record of it, or
  nil when running without a backend."
  [world profile car]
  (let [seed      (:seed world fallback-seed)
        canvas    (js/document.getElementById "game")
        params    (js/URLSearchParams. (.-search js/location))
        mode      (or (:mode world)
                      (if (.has params "outbreak") :outbreak :normal))
        ;; One record of everything the seed does not already say. Restored
        ;; before anything spawns, so a chunk that was cleared out stays cleared.
        ov        (overlay/create seed mode)
        restored  (overlay/load! ov seed)
        s         (sim/create! {:seed seed :opponents opponent-count :kind car})
        rs        (render/create! canvas s seed)
        ps        (props/create (:world @s) (:scene rs) ov)
        bs        (buildings/create (:world @s) (:scene rs) (:textures rs))
        fu        (furniture/create (:world @s) (:scene rs))
        br        (parts/create (:world @s) (:scene rs) ov)
        fl        (parts/create (:world @s) (:scene rs))
        ;; Landmarks are parts like anything else: same array layout, same
        ;; colliders, same shadows. They get their own state only so the
        ;; overlay-backed barrier bookkeeping on bridges stays theirs.
        lm        (parts/create (:world @s) (:scene rs))
        tf        (traffic/create (:world @s) (:scene rs) seed ov)
        bd        (birds/create (:scene rs))
        fr        (fire/create (:scene rs))
        pu        (powerups/create (:scene rs) ov)
        mm        (minimap/create seed)
        ck        (cockpit/create)
        ;; Outbreak is a property of the world, with a query parameter for
        ;; trying it without one -- the flag changes behaviour, not generation,
        ;; so the same seed makes the same city either way.
        pd        (peds/create (:world @s) (:scene rs) ov mode)
        gm        (game/create)
        rvs       (rivals/create seed opponent-count)
        mgr       (chunks/create
                   {:seed      seed
                    :world     (:world @s)
                    :on-add    (fn [data] (render/add-chunk! rs data))
                    :on-remove (fn [mesh] (render/remove-chunk! rs mesh))
                    :on-physics-add    (fn [key data]
                                     (props/add-chunk! ps key (:props data))
                                     (buildings/add-chunk! bs key (:buildings data) (:building-parts data))
                                     (furniture/add-chunk! fu key (:furniture data))
                                     (parts/add-chunk! br key (:bridges data))
                                     (parts/add-chunk! fl key (:flora data))
                                     (parts/add-chunk! lm key (:landmarks data))
                                     (powerups/add-chunk! pu key (:pickups data))
                                     (traffic/add-chunk! tf key (:traffic data))
                                     (peds/add-chunk! pd key (:peds data)))
                    :on-physics-remove (fn [key]
                                     (props/remove-chunk! ps key)
                                     (buildings/remove-chunk! bs key)
                                     (furniture/remove-chunk! fu key)
                                     (parts/remove-chunk! br key)
                                     (parts/remove-chunk! fl key)
                                     (parts/remove-chunk! lm key)
                                     (powerups/remove-chunk! pu key)
                                     (traffic/remove-chunk! tf key)
                                     (peds/remove-chunk! pd key))})
        ;; One seam, two implementations. Nothing below this line knows
        ;; whether it is single player.
        remotes   (remote/create (:world @s) (:scene rs) (:textures rs))
        transport (if world
                    (net/websocket
                     (str (if (= "https:" (.-protocol js/location)) "wss://" "ws://")
                          (.-host js/location) "/ws/" (:id world))
                     {:on-open  #(js/console.log "multiplayer: connected")
                      :on-close #(js/console.log "multiplayer: disconnected")})
                    (net/loopback))
        ;; Driving input and camera input are attached separately on purpose:
        ;; only the first becomes a `Command`, and a Command has to mean the
        ;; same thing whether a human, the AI or the network produced it.
        detach    (let [d-input (input/attach!)
                        d-cam   (camera/attach! (:camera-state rs) canvas)
                        d-map   (minimap/attach! mm)]
                    (fn [] (d-input) (d-cam) (d-map)))
        [sx _ sz] (:spawn @s)]
           ;; The one place physics impacts become gameplay.
           ;;
           ;; Only collisions involving a car count, and only above a speed
           ;; threshold. Contact force on its own is not evidence of a crash:
           ;; props settling against each other generate more force than a car
           ;; clipping one, and gating on force alone wrecked a sixth of the
           ;; world while the player sat still.
           (swap! s assoc :on-impact
                  (fn [h1 h2 force]
                    (let [v1 (sim/vehicle-of-collider s h1)
                          v2 (sim/vehicle-of-collider s h2)]
                      (when (or v1 v2)
                        ;; Scoring belongs to the player alone. Rivals drive
                        ;; through the same crowds and collect the same dents,
                        ;; but a rival that ran someone over would be taking
                        ;; kills off the player's target.
                        (when-let [hit (cond (= 0 v1) h2 (= 0 v2) h1)]
                          (when (> (js/Math.abs (sim/player-speed s)) min-smash-speed)
                            (when (props/prop? ps hit)
                              (when-let [d (props/destroy! ps hit)]
                                (game/prop-wrecked! gm)
                                ;; Tell the room: this is the only world state
                                ;; that ever crosses the network.
                                (net/-send! transport (wire/encode-delta (assoc d :kind :prop)))
                                ;; A gas cylinder is not scenery. It goes up,
                                ;; takes its neighbours with it, and leaves the
                                ;; street on fire -- and any of those neighbours
                                ;; that were gas do the same. The chain
                                ;; terminates because each is gone from the
                                ;; chunk before the next round looks at it.
                                (when (:volatile? d)
                                  (loop [seeds [d], depth 0]
                                    (when (and (seq seeds) (< depth 4))
                                      (recur
                                       (vec (mapcat
                                             (fn [{[bx by bz] :pos}]
                                               (let [{:keys [r life blast push]} barrel-fire]
                                                 (fire/ignite! fr bx by bz r life :player)
                                                 (sim/blast! s [bx by bz] blast push)
                                                 (filter :volatile?
                                                         (props/destroy-near! ps bx bz blast))))
                                             seeds))
                                       (inc depth)))))))
                            (when (peds/ped? pd hit)
                              (let [[vx vy vz] (:vel (sim/telemetry s))]
                                (when-let [d (peds/kill! pd hit [vx vy vz])]
                                  (game/ped-killed! gm)
                                  (net/-send! transport (wire/encode-delta (assoc d :kind :ped))))))
                            (when (traffic/traffic? tf hit)
                              (let [[vx vy vz] (:vel (sim/telemetry s))]
                                (when-let [d (traffic/wreck! tf hit [vx vy vz])]
                                  (game/car-wrecked! gm)
                                  (net/-send! transport
                                              (wire/encode-delta
                                               (assoc d :kind :car)))
                                  ;; A tanker is the largest thing a player can
                                  ;; set off without a power-up.
                                  (when (:volatile? d)
                                    (let [[bx by bz] (:pos d)
                                          {:keys [r life seeds blast push]} tanker-fire]
                                      (fire/ignite! fr bx by bz r life :player seeds)
                                      (sim/blast! s [bx by bz] blast push)
                                      (doseq [{:keys [pos]} (filter :volatile?
                                                                    (props/destroy-near! ps bx bz blast))]
                                        (fire/ignite! fr (nth pos 0) (nth pos 1) (nth pos 2)
                                                      (:r barrel-fire) (:life barrel-fire)
                                                      :player)))))))
                            ;; Bridge parapets. Scored as clutter, because what
                            ;; they are worth is not the points -- it is the
                            ;; hole, and what is on the other side of it.
                            (when (parts/breakable? br hit)
                              (let [[vx vy vz] (:vel (sim/telemetry s))]
                                (when-let [d (parts/smash! br hit [vx vy vz])]
                                  (game/prop-wrecked! gm)
                                  (net/-send! transport
                                              (wire/encode-delta
                                               (assoc d :kind :barrier))))))))
                        ;; Damage lands on every car in the collision, which is
                        ;; how a rival gets written off at all. This used to be
                        ;; applied to `(:vehicle @s)` -- a key the sim stopped
                        ;; having when it grew from one vehicle to several -- so
                        ;; every qualifying impact threw out of the contact
                        ;; callback and took the rest of that tick with it.
                        ;; Nothing was damaged, and no opponent could be wrecked.
                        (doseq [[vi hit] [[v1 h2] [v2 h1]]
                                :when vi]
                          (let [veh (nth (sim/vehicles s) vi)]
                            (when (> (js/Math.abs (vehicle/forward-speed veh))
                                     min-smash-speed)
                              ;; Pedestrians and clutter barely scratch the
                              ;; paint; terrain, buildings and other cars hit
                              ;; properly. Capped per event so one scrape spread
                              ;; over many contacts cannot write a car off in a
                              ;; single tick.
                              (vehicle/add-damage!
                               veh
                               (min max-damage-per-hit
                                    (* (if (or (props/prop? ps hit) (peds/ped? pd hit))
                                         0.04 1.0)
                                       (/ (max 0.0 (- force damage-force-floor))
                                          damage-force-scale))))))))))) 
           (render/resize! rs canvas)
           ;; Wait for ground under the spawn before the first tick, or the car
           ;; falls through the world. Generation is in a worker, so this is a
           ;; promise rather than a freeze.
           (-> (chunks/ensure-loaded! mgr sx sz)
               (.then
                (fn [cs]
                  (let [stop (start-frame-loop! {:sim s :rs rs :transport transport
                                                 :canvas canvas :chunk-mgr mgr
                                                 :props-state ps :buildings-state bs
                                                 :furniture-state fu
                                                 :traffic-state tf
                                                 :bridges br
                                                 :cock ck
                                                 :fire-state fr
                                                 :powerups-state pu
                                                 :birds-state bd
                                                 :overlay ov
                                                 :minimap mm
                                                 :peds-state pd :game gm
                                                 :rvs rvs
                                                 :remotes remotes})]
                    (reset! app {:sim s :rs rs :transport transport :chunks mgr
                                 :props ps :buildings bs :furniture fu :bridges br :flora fl
                                 :landmarks lm :traffic tf :birds bd :peds pd :fire fr :powerups pu
                                 :overlay ov :minimap mm :cockpit ck :game gm
                                 :rivals rvs :remotes remotes
                                 :stop stop :detach detach}))
                  (js/console.log "carmagedonio up:" (:loaded cs) "chunks,"
                                  (:live (props/stats ps)) "props,"
                                  (:standing (buildings/stats bs)) "buildings,"
                                  (:pieces (furniture/stats fu)) "street furniture"
                                  (if world (str "world " (:id world)) "(offline)")))))))

(defn- watch-for-end!
  "Submit the run once, when it finishes.

  Submission is fire-and-forget and failure is silent by design: losing the
  record of a run is a far smaller problem than interrupting the player to tell
  them about it, and without a backend there is nothing to submit to."
  [game world profile ov]
  (add-watch game ::submit
             (fn [_ _ old new]
               (when (and (= :running (:state old))
                          (not= :running (:state new)))
                 (remove-watch game ::submit)
                 ;; The run is over: write the overlay out now rather than
                 ;; waiting for the debounce, and keep the tally with it.
                 (overlay/set-tally! ov (game/result game))
                 (overlay/save! ov (js/Date.now) true)
                 ;; `submit-run!` returns nil rather than a promise when there
                 ;; is no world or profile to attach the run to, which is the
                 ;; normal offline case -- and chaining `.then` onto that threw
                 ;; out of the watch every time a run ended without a backend.
                 (if-let [pending (api/submit-run! (:id world) (:id profile)
                                                   (game/result game))]
                   (.then pending
                          (fn [r]
                            (js/console.log "run"
                                            (if r (str "recorded as " (:id r))
                                                "not recorded (rejected)"))))
                   (js/console.log "run not recorded (offline)"))))))

(defn init! []
  (-> (sim/init!)
      (.then (fn [_] (api/ensure-profile! "player")))
      (.then (fn [profile]
               ;; The lobby answers the two questions the URL used to: which
               ;; room, and which car. `?world=` still works and is what a
               ;; shared link is -- it arrives preselected rather than skipping
               ;; the screen, so an invited player still picks a car.
               (let [params (js/URLSearchParams. (.-search js/location))
                     car0   (keyword (.get params "car"))]
                 (-> (menu/show! {:preselect (.get params "world")
                                  :car (when (contains? cars/catalogue car0) car0)})
                     (.then (fn [{:keys [world car]}]
                              (-> (boot! world profile car)
                                  (.then (fn [_]
                                           (watch-for-end! (:game @app) world profile
                                                           (:overlay @app)))))))))))
      (.catch (fn [e] (js/console.error "boot failed" e)))))


(defn after-load!
  "shadow-cljs hot reload. Function bodies are picked up automatically because
  calls go through vars, so the world is deliberately NOT rebuilt here -- you
  can retune driving without losing the pile of debris you just knocked over.
  Call `(reboot!)` from the REPL when you do want a fresh world."
  []
  (js/console.log "reloaded"))

(defn reboot! []
  (when-let [{:keys [stop detach]} @app]
    (stop)
    (detach))
  (init!))
