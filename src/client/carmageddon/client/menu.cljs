(ns carmageddon.client.menu
  "The screen before the game: which room, and which car.

  Both were query parameters -- `?world=` and `?car=` -- which works and is
  still what a shared link is, but it means the only way to find out what rooms
  exist is to read the API by hand. A lobby is the smallest thing that makes
  multiplayer usable by somebody who was not told the id.

  Plain DOM, no framework. There is not a component anywhere else in this
  client; the HUD is a canvas and the world is three.js, and introducing a view
  library for one screen would be the largest dependency in the project.

  `show!` returns a promise of `{:world w :car kind :seed n}`. `:world` is nil
  for a solo run, and then `:seed` says which world to drive in. Everything
  above it -- `main/init!` -- waits on that promise and then boots exactly as
  it did when the answers came from the URL."
  (:require [carmageddon.client.api :as api]
            [carmageddon.client.cars :as cars]))

(defn- el
  "One element, its attributes, and its children."
  [tag attrs & children]
  (let [^js e (js/document.createElement (name tag))]
    (doseq [[k v] attrs]
      (case k
        :class (set! (.-className e) v)
        :text  (set! (.-textContent e) v)
        :on-click (.addEventListener e "click" v)
        :on-input (.addEventListener e "input" v)
        (.setAttribute e (name k) (str v))))
    (doseq [c children :when c] (.appendChild e c))
    e))

(defn- clear! [^js e] (set! (.-innerHTML e) ""))

;; --- what a car is, in one line ---------------------------------------------

(defn- mass-of
  "Kilograms, from the box and its density -- the same arithmetic the physics
  does, rather than a number written down twice."
  [kind]
  (let [[hx hy hz] (cars/half kind)]
    (* 8.0 hx hy hz (cars/density kind))))

(defn- drive-of [kind]
  (let [d (:driven (cars/spec kind))]
    (cond (= 4 (count d)) "four-wheel drive"
          (contains? d 0) "front-wheel drive"
          :else "rear-wheel drive")))

(defn- car-line [kind]
  (let [top (* 3.6 (:top-speed (:tuning (cars/spec kind))
                               (:top-speed @cars/base-tuning)))]
    (str (js/Math.round (mass-of kind)) " kg"
         " · " (js/Math.round top) " km/h"
         " · " (drive-of kind))))

;; --- rooms ------------------------------------------------------------------

(defn- room-row
  [{:keys [id name seed mode]} online selected? on-pick]
  (el :button {:class (str "room" (when selected? " picked"))
               :on-click (fn [_] (on-pick id))}
      (el :div {:class "room-main"}
          (el :span {:class "room-name" :text (or name id)})
          (el :span {:class "room-meta"
                     :text (str "seed " seed
                                (when (= :outbreak mode) " · outbreak"))}))
      (el :span {:class (str "room-online" (when (pos? online) " live"))
                 :text (if (pos? online)
                         (str online " here")
                         "empty")})))

(defn- share-link [id]
  (str (.-origin js/location) "/?world=" id))

;; A world nobody asked for. Six digits rather than thirty-two bits: a seed
;; worth replaying is a seed somebody will read off the screen and type in.
(defn- fresh-seed [] (inc (js/Math.floor (* 999999 (js/Math.random)))))

(defn- typed-seed
  "The solo seed field as a number, or nil when it is blank or not one."
  []
  (let [v (some-> (js/document.getElementById "solo-seed") .-value .trim)
        n (js/parseInt (or v ""))]
    (when (and (seq v) (not (js/isNaN n))) n)))

;; --- the screen -------------------------------------------------------------

(defn show!
  "Draw the lobby and resolve once the player has chosen.

  `preselect` is the `?world=` from the link they arrived on, if any -- a shared
  link should land you looking at the room it names, already highlighted."
  [{:keys [preselect car seed]}]
  (js/Promise.
   (fn [resolve _reject]
     (let [^js root (js/document.getElementById "menu")
           state (atom {:worlds nil :room preselect
                        :car (or car cars/default-kind)
                        :online {} :busy? false})]
       (when root
         (set! (.-hidden root) false))
       ;; Built once and re-attached on every render rather than rebuilt.
       ;; `render!` wipes the sheet, and it runs whenever anything changes --
       ;; including a player count arriving from the server a second after the
       ;; screen appeared. Rebuilding these would delete whatever was being
       ;; typed at the moment somebody else's request happened to land.
       (let [^js form (el :div {:class "newroom"})
             ^js busy-btn (el :button {:class "ghost" :text "create room"})
             ;; Persistent for the same reason as the room form: a player
             ;; count arriving mid-keystroke re-renders the sheet.
             ^js solo-seed (el :input {:id "solo-seed" :inputmode "numeric"
                                       :placeholder "seed (blank for a new world)"
                                       :maxlength 10})]
       (when seed (set! (.-value solo-seed) (str seed)))
       (letfn
        [(pick-room! [id] (swap! state assoc :room id) (render!))
         (pick-car! [k] (swap! state assoc :car k) (render!))

         (refresh! []
           (-> (api/list-worlds)
               (.then (fn [ws]
                        ;; nil is "no server", which is a different screen from
                        ;; a server with no rooms on it yet.
                        (swap! state assoc :worlds (when (some? ws) (vec ws)))
                        (render!)
                        ;; Player counts come one request per room. There are a
                        ;; handful of rooms and this happens once, on a screen
                        ;; nobody is being timed on.
                        (doseq [w ws]
                          (-> (api/world-players (:id w))
                              (.then (fn [n]
                                       (swap! state assoc-in [:online (:id w)] n)
                                       (render!)))))))))

         (create! []
           (when-not (:busy? @state)
             ;; Read the form *before* re-rendering. Rendering rebuilds the
             ;; sheet, and reading afterwards read a freshly created, empty set
             ;; of inputs -- every room came out called "carmagedonio" with a
             ;; random seed however carefully it had been filled in.
             (let [nm (some-> (js/document.getElementById "new-room-name") .-value)
                   sd (some-> (js/document.getElementById "new-room-seed") .-value)
                   ob (some-> (js/document.getElementById "new-room-outbreak") .-checked)
                   n  (js/parseInt (or sd ""))]
               (swap! state assoc :busy? true)
               (render!)
               (-> (api/create-world!
                    {:name (if (seq nm) nm "carmagedonio")
                     ;; A blank seed means "you choose", which is the normal
                     ;; case. A typed one is how you rebuild a world somebody
                     ;; described to you.
                     :seed (when-not (js/isNaN n) n)
                     :mode (if ob :outbreak :normal)})
                   (.then (fn [w]
                            (swap! state assoc :busy? false :room (:id w))
                            (refresh!)))))))

         (start! []
           (let [{:keys [worlds room car]} @state
                 w (first (filter #(= room (:id %)) worlds))]
             (when root (set! (.-hidden root) true))
             (resolve {:world w :car car})))

         ;; A solo run: no room, and a world of your own choosing. The same
         ;; seed is the same world -- streets, crowds, rivals, weather -- every
         ;; time, from a clean start; a blank one is somewhere new.
         (solo! []
           (let [typed (typed-seed)]
             (when root (set! (.-hidden root) true))
             (resolve {:world nil :car (:car @state)
                       :seed (or typed (fresh-seed))
                       :fixed? (some? typed)})))

         (render! []
           (when root
             (let [{:keys [worlds room car online busy?]} @state
                   offline? (nil? worlds)]
               (set! (.-textContent busy-btn) (if busy? "creating…" "create room"))
               (set! (.-disabled busy-btn) (boolean busy?))
               (clear! root)
               (.appendChild
                root
                (el :div {:class "sheet"}
                    (el :h1 {:text "carmagedonio"})

                    ;; --- rooms ---
                    (if offline?
                      (el :p {:class "note"
                              :text (str "No server on this address, so this is "
                                         "a solo run. Start one with "
                                         "`clojure -M:server` to play together.")})
                      (el :div {}
                          (el :h2 {:text "room"})
                          (apply el :div {:class "rooms"}
                                 (if (seq worlds)
                                   (map (fn [w]
                                          (room-row w (get online (:id w) 0)
                                                    (= room (:id w)) pick-room!))
                                        worlds)
                                   [(el :p {:class "note"
                                            :text "No rooms yet. Make one."})]))
                          form
                          (when room
                            (el :p {:class "share"
                                    :text (str "share: " (share-link room))}))))

                    ;; --- solo ---
                    (el :h2 {:text "solo"})
                    (el :div {:class "newroom"}
                        solo-seed
                        (when-not offline?
                          (el :button {:class "ghost" :text "drive solo"
                                       :on-click (fn [_] (solo!))})))
                    (el :p {:class "note"
                            :text (str "Your own world, nobody else in it. The same "
                                       "seed is the same world every time, from a "
                                       "clean start.")})

                    ;; --- car ---
                    (el :h2 {:text "car"})
                    (apply el :div {:class "cars"}
                           (map (fn [k]
                                  (el :button {:class (str "car" (when (= car k) " picked"))
                                               :text (cars/display-name k)
                                               :on-click (fn [_] (pick-car! k))}))
                                cars/kinds))
                    (el :p {:class "carline" :text (car-line car)})

                    ;; --- go ---
                    (el :button {:class "drive"
                                 :text (if (and (not offline?) room) "drive" "drive solo")
                                 :on-click (fn [_] (if (and (not offline?) room)
                                                     (start!)
                                                     (solo!)))})))))) ]

         ;; Fill the persistent form in, once.
         (.appendChild form (el :input {:id "new-room-name" :placeholder "new room name"
                                        :maxlength 40}))
         (.appendChild form (el :input {:id "new-room-seed" :placeholder "seed (optional)"
                                        :inputmode "numeric"}))
         (.appendChild form (el :label {:class "check"}
                                (el :input {:id "new-room-outbreak" :type "checkbox"})
                                (el :span {:text "outbreak"})))
         (.addEventListener busy-btn "click" (fn [_] (create!)))
         (.appendChild form busy-btn)

         (render!)
         (refresh!)))))))
