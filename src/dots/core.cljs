(ns dots.core
  (:require
   [cljs.core.async :as async
             :refer [<! >! chan close! sliding-buffer put! alts! timeout]]
   [jayq.core :refer [$ append ajax inner css $deferred when done resolve pipe on bind] :as jq]
   [jayq.util :refer [log]]
   [crate.core :as crate]
   [clojure.string :refer [join blank?]])
  (:require-macros [cljs.core.async.macros :as m :refer [go alt!]]))

(defn mouseevent-chan [selector event msg-name]
  (let [rc (chan)]
      (bind ($ selector) event
            #(do
               (put! rc [msg-name {:x (.-pageX %) :y (.-pageY %)}])))
    rc))

(defn drawstart-chan [selector]
  (mouseevent-chan selector "mousedown" :drawstart))

(defn drawend-chan [selector]
  (mouseevent-chan selector "mouseup" :drawend))

(defn drawer-chan [selector]
  (mouseevent-chan selector "mousemove" :draw))

(defn draw-chan [selector]
  (let [down-ch (drawstart-chan selector)
        up-ch   (drawend-chan selector)
        draw-ch (drawer-chan selector)
        rc      (chan)]
    (go (loop []
          (let [[val ch] (alts! [down-ch draw-ch up-ch])]
            (if (= ch down-ch)
              (do
                (put! rc val)
                (loop []
                  (let [[val ch] (alts! [up-ch draw-ch down-ch])]
                    (put! rc val)
                    (if (= ch draw-ch)
                      (recur)))))
              (recur)))
          (recur)))
    rc))

(def board-size 10)

(defn comp-pos [pos]
  (- (dec board-size) pos))

(def peice-colors [:blue :green :yellow :purple :red])

(let [number-colors (count peice-colors)]
  (defn rand-color []
    (get peice-colors (rand-int number-colors))))

(defn get-rand-colors [number]
  (map (fn [x] (rand-color)) (range number)))

(defn dot-pos-to-abs-position [dot-pos]
  [(+ 30 (* 50 (- 9 dot-pos))) 30])

(defn dot-pos-to-center-position [dot-pos]
  (vec (map (partial + 10) (dot-pos-to-abs-position dot-pos))))

(defn dot [i color]
  (let [[top left] (dot-pos-to-abs-position i)
        class (str "dot " (name color))
        style (str "top:" top "px; left: " left "px;")]
    [:div {:class class :style style}]))

(defn board [{:keys [board] :as state}]
  [:div.dots-game
   [:div.chain-line]
   [:div.board]]
)

(defn create-dot [position color]
  {:color color :elem (crate/html (dot position color))})

(defn remove-dot [{:keys [elem] :as dot}]
  (.remove ($ elem)))

(defn update-dot [dot pos]
  (if dot
    (let [[top left] (dot-pos-to-abs-position pos)]
      (css ($ (dot :elem)) {:top top :left left}))))

(defn add-dots-to-board [dots]
  (doseq [{:keys [elem]} dots]
    (append ($ ".board") elem)))

(defn render-view [state]
  (inner ($ ".container")
         (crate/html (board state)))
  (add-dots-to-board (state :board)))

(defn dot-index [{:keys [x y]}]
  (let [board-length 10]
    (if (and (< 40 x 60) (< 30 (mod y 50)))
      (let [position (- 9 (int (/ y 50)))]
        (if (> board-length position)
          position)))))

(defn dot-follows? [state prev-dot cur-dot]
  (let [board (state :board)
        prev-dot-color (-> board (get prev-dot) :color)
        cur-dot-color (-> board (get cur-dot) :color)]
    (or (nil? prev-dot)
        (and
         (= prev-dot-color cur-dot-color)
         (or
          (= cur-dot (inc prev-dot))
          (= cur-dot (dec prev-dot)))))))

(defn add-dot [{:keys [dot-chain] :as state} dot-pos]
  (if (dot-follows? state (last dot-chain) dot-pos)
    (assoc state :dot-chain (conj (or dot-chain []) dot-pos))
    state))

(defn render-chain-element [last-pos pos color]
  (let [[top1 left] (dot-pos-to-center-position last-pos)
        [top2 _] (dot-pos-to-center-position pos)
        style (str "width: 5px; height: 50px; top:"
                   (if (< top1 top2) top1 top2) "px; left: " ( - left 2) "px;")]
    [:div {:style style :class (str "line " (name (or color :blue)))}]))

(defn render-dot-chain [state]
  (let [dot-chain (state :dot-chain)
        color (-> state :board
                  (get (first (state :dot-chain)))
                  :color)
        rends (map render-chain-element
                   (butlast dot-chain)
                   (rest dot-chain)
                   (repeat color))]
    (inner ($ ".dots-game .chain-line")
           (crate/html (concat [:div] rends)))))

(defn erase-dot-chain []
    (inner ($ ".dots-game .chain-line") ""))

(defn get-dots [draw-ch start-state]
  (go
   (loop [state start-state]
     (let [[msg point] (<! draw-ch)]
       (log "drawing" (prn-str msg) (prn-str state))
       (if (< 1 (count (state :dot-chain)))
         (render-dot-chain state))
       (if (= msg :drawend)
         (do
           (erase-dot-chain)
           state)
         (recur
          (if-let [dot-pos (dot-index point)]
            (add-dot state dot-pos)
            state)))))))

(defn render-remove-dots [state dot-chain]
  (let [pos-set        (set dot-chain)
        dots-to-remove (keep-indexed #(if (pos-set %1) %2) (state :board))
        next_board     (keep-indexed #(if (not (pos-set %1)) %2) (state :board))]
    (doseq [dot dots-to-remove]
      (remove-dot dot))
    (assoc state :board (vec next_board))))

(defn add-missing-dots [{:keys [board] :as state}]
  (if (= (count board) board-size)
    state
    (let [new-dots (map create-dot
                        (repeat 13)
                        (get-rand-colors (- board-size (count board))))]
      (add-dots-to-board new-dots)
      (assoc state :board (vec (concat (state :board) new-dots))))
    ))

(defn render-position-updates [{:keys [board]}]
  (go
   (loop [[dot & xd] board
          pos 0]
     (<! (timeout 20))
     (update-dot dot pos)
     (recur xd (inc pos)))))

(defn app-loop [init-state]
  (let [draw-ch (draw-chan "body")]
    (render-view init-state)
    (go
     (loop [state init-state]
       (render-position-updates state)
       (let [state (add-missing-dots state)]
         (<! (timeout 100))
         (render-position-updates state)
         (log "rendering" (prn-str state))
         (recur
          (let [{:keys [dot-chain]} (<! (get-dots draw-ch state))]
            (if (< 1 (count dot-chain))
              (render-remove-dots state dot-chain)
              state))))))))

(app-loop {:board (vec (map-indexed create-dot (get-rand-colors 10)) )
           :dot-chain []})
