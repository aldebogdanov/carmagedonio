(ns carmageddon.client.net
  "Transport seam.

  Single player runs on `loopback`; multiplayer runs on `websocket`. Nothing
  above this namespace knows which -- that is what the protocol was for, and it
  is why adding multiplayer did not require touching the game loop."
  (:require [carmageddon.shared.wire :as wire]))

(defprotocol Transport
  (-send! [this msg] "Queue an outbound message. Never blocks.")
  (-poll! [this]     "Drain and return inbound messages as a vector.")
  (-state [this]     "One of :connecting :open :closed."))

;; --- Loopback ---------------------------------------------------------------

(deftype Loopback [inbox handler]
  Transport
  (-send! [_ msg]
    ;; A local "server": whatever `handler` returns is delivered back next poll.
    ;; For M0 it just swallows everything.
    (when-let [reply (handler msg)]
      (vswap! inbox conj reply)))
  (-poll! [_]
    (let [msgs @inbox]
      (vreset! inbox [])
      msgs))
  (-state [_] :open))

(defn loopback
  ([] (loopback (constantly nil)))
  ([handler] (->Loopback (volatile! []) handler)))

;; --- WebSocket -------------------------------------------------------------

(deftype WebSocketTransport [^js ws inbox state]
  Transport
  (-send! [_ frame]
    ;; Dropped rather than queued while connecting or closing: this carries
    ;; snapshots, and a stale snapshot delivered late is worse than none.
    (when (= 1 (.-readyState ws))
      (.send ws frame)))
  (-poll! [_]
    (let [msgs @inbox]
      (vreset! inbox [])
      msgs))
  (-state [_] @state))

(defn websocket
  "Connect to a world. Frames are the binary protocol in
  `carmageddon.shared.wire`; decoding happens here so nothing above this
  namespace touches bytes."
  [url {:keys [on-open on-close] :or {on-open identity on-close identity}}]
  (let [ws    (js/WebSocket. url)
        inbox (volatile! [])
        state (volatile! :connecting)]
    (set! (.-binaryType ws) "arraybuffer")
    (set! (.-onopen ws) (fn [_] (vreset! state :open) (on-open)))
    (set! (.-onclose ws) (fn [_] (vreset! state :closed) (on-close)))
    (set! (.-onerror ws) (fn [_] (vreset! state :closed)))
    (set! (.-onmessage ws)
          (fn [^js e]
            (when-let [msg (wire/decode (js/Uint8Array. (.-data e)))]
              (vswap! inbox conj msg))))
    (->WebSocketTransport ws inbox state)))
