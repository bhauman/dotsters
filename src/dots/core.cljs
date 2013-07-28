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

(def board-size 6)

(defn comp-pos [pos]
  (- (dec board-size) pos))

(def peice-colors [:blue :green :yellow :purple :red])

(let [number-colors (count peice-colors)]
  (defn rand-color []
    (get peice-colors (rand-int number-colors))))

(defn get-rand-colors [number]
  (map (fn [x] (rand-color)) (range number)))

(defn dot-pos-to-abs-position [[xpos ypos]]
  [(+ 30 (* 50 (- (dec board-size) ypos)))
   (+ 30 (* 50 xpos))])

(defn dot-pos-to-center-position [dot-pos]
  (vec (map (partial + 10) (dot-pos-to-abs-position dot-pos))))

(defn dot [pos color]
  (let [[top left] (dot-pos-to-abs-position pos)
        class (str "dot " (name color))
        style (str "top:" top "px; left: " left "px;")]
    [:div {:class class :style style}]))

(defn board [{:keys [board] :as state}]
  [:div.dots-game
   [:div.chain-line]
   [:div.board]]
)

(defn create-dot [xpos ypos color]
  {:color color :elem (crate/html (dot [xpos ypos] color))})

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
  (mapv add-dots-to-board (state :board)))

(defn dot-index [{:keys [x y]}]
  (if (and (< 30 (mod x 50)) (< 30 (mod y 50)))
    (let [ypos (- (dec board-size) (int (/ y 50)))
          xpos (int (/ x 50))]
      (if (and
           (> board-size ypos)
           (> board-size xpos))
        [xpos ypos]))))

(defn dot-follows? [state prev-dot cur-dot]
  (let [board (state :board)
        prev-dot-color (-> board (get-in prev-dot) :color)
        cur-dot-color (-> board (get-in cur-dot) :color)]
    (log (prn-str [prev-dot-color cur-dot-color]))
    (or (nil? prev-dot)
        (and
         (= prev-dot-color cur-dot-color)
         (let [[cx cy] cur-dot
               [px py] prev-dot]
           (or
            (or (= cx (inc px))
                (= cx (dec px)))
            (or (= cy (inc py))
                (= cy (dec py)))))))))

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

(defn render-remove-dots-row-helper [dot-chain-set col]
  (log (prn-str dot-chain-set))
  (let [dots-to-remove (keep-indexed #(if (dot-chain-set %1) %2) col)
        next_col     (keep-indexed #(if (not (dot-chain-set %1)) %2) col)]
    (doseq [dot dots-to-remove]
      (remove-dot dot))
    (log (prn-str (count next_col)))
    (vec next_col)))

(defn render-remove-dots [state dot-chain]
  (let [dot-chain-groups  (group-by first dot-chain)
        next_board     (map-indexed #(render-remove-dots-row-helper
                              (set (map last (get dot-chain-groups %1)))
                              %2)
                            (state :board))]
    (assoc state :board (vec next_board))))

(defn add-missing-dots-helper [col-idx col]
  (if (= (count col) board-size)
    col
    (let [new-dots (map create-dot
                        (repeat col-idx)
                        (repeat 13)
                        (get-rand-colors (- board-size (count col))))]
      (add-dots-to-board new-dots)
      (vec (concat col new-dots)))))

(defn add-missing-dots [{:keys [board] :as state}]
    (assoc state :board
           (map-indexed
            #(add-missing-dots-helper %1 %2)
            board)))

(defn render-position-updates-helper [col-idx col]
  (go
   (loop [cur-col col
          pos 0]
     (let [dot (first cur-col)
           xd  (rest cur-col)]
       (if (not (nil? dot))
         (do
           (<! (timeout 40))
           (update-dot dot [col-idx pos])
           (recur xd (inc pos))))))))


(defn render-position-updates [{:keys [board]}]
  (doall
   (map-indexed
    #(render-position-updates-helper %1 %2)
    board)))

(defn app-loop [init-state]
  (let [draw-ch (draw-chan "body")]
    (render-view init-state)
    (go
     (loop [state init-state]
       (log "before update" (prn-str (map count (state :board))))
       (render-position-updates state)
       (let [state (add-missing-dots state)]
         (<! (timeout 300))
         (render-position-updates state)
         (log "rendering" (prn-str state))
         (recur
          (let [{:keys [dot-chain]} (<! (get-dots draw-ch state))]
            (if (< 1 (count dot-chain))
              (render-remove-dots state dot-chain)
              state))))))))

(defn create-board []
  (vec
   (map-indexed
    (fn [i x] (vec (map-indexed (partial create-dot i) (get-rand-colors board-size)))) 
    (range board-size))))

(app-loop {:board (create-board)
           :dot-chain []})

