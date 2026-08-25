(ns carmageddon.client.worker
  "Chunk generation, off the main thread.

  Measured cost of one chunk swings roughly 5x with hardware -- ~4 ms on a fast
  machine, ~19 ms on a slow one. A per-frame budget cannot absorb that, because
  a chunk is generated as one indivisible unit: the budget can decline to start
  another chunk, but it cannot stop one halfway. On slower hardware every
  boundary crossing therefore dropped a frame.

  Generation is pure `.cljc`, so it moves here unchanged.

  The height and colour arrays are posted as transferables: ownership moves to
  the main thread with no copy, which matters because they are the bulk of what
  a chunk is."
  (:require [carmageddon.shared.worldgen :as worldgen]))

(defn- handle [e]
  ;; String-keyed access throughout: these field names are a wire protocol
  ;; between two separately-compiled contexts, and `:advanced` would happily
  ;; rename a `.-cz` property access on an untyped object.
  (let [d    (.-data e)
        seed (unchecked-get d "seed")
        cx   (unchecked-get d "cx")
        cz   (unchecked-get d "cz")
        {:keys [heights colors props buildings building-parts peds furniture
                bridges verts size origin]}
        (worldgen/chunk-data seed cx cz)
        [x0 z0] origin]
    (.postMessage js/self
                  #js {:cx cx :cz cz :verts verts :size size :x0 x0 :z0 z0
                       :heights heights :colors colors
                       :props props :buildings buildings :peds peds
                       :furniture furniture :parts building-parts
                       :bridges bridges}
                  #js [(.-buffer heights) (.-buffer colors)
                       (.-buffer props) (.-buffer buildings) (.-buffer peds)
                       (.-buffer furniture) (.-buffer building-parts)
                       (.-buffer bridges)])))

(defn init! []
  (set! (.-onmessage js/self) handle))
