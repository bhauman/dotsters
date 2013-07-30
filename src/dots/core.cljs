(ns dots.core
  (:require
   [cljs.core.async :as async
             :refer [<! >! chan close! sliding-buffer put! alts! timeout]]
   [jayq.core :refer [$ append ajax inner css $deferred
                      when done resolve pipe on bind attr
                      offset] :as jq]
   [jayq.util :refer [log]]
   [crate.core :as crate]
   [clojure.string :refer [join blank? replace-first]]
   [clojure.set :refer [union]])
  (:require-macros [cljs.core.async.macros :as m :refer [go alt!]]))

(defn mouseevent-chan [rc selector event msg-name]
  (bind ($ selector) event
        #(do
           (put! rc [msg-name {:x (.-pageX %) :y (.-pageY %)}]))))

(defn touchevent-chan [rc selector event msg-name]
  (bind ($ selector) event
        #(let [touch (aget (.-touches (.-originalEvent %)) 0)]
           (put! rc [msg-name {:x (.-pageX touch) :y (.-pageY touch)}]))))

(defn drawstart-chan [ichan selector]
  (mouseevent-chan ichan selector "mousedown" :drawstart)
  (touchevent-chan ichan selector "touchstart" :drawstart))

(defn drawend-chan [ichan selector]
  (mouseevent-chan ichan selector "mouseup" :drawend)
  (mouseevent-chan ichan selector "touchend" :drawend))

(defn drawer-chan [ichan selector]
  (mouseevent-chan ichan selector "mousemove" :draw)
  (touchevent-chan ichan selector "touchmove" :draw))

(defn get-drawing [input-chan out-chan]
  (go (loop [msg (<! input-chan)]
        (put! out-chan msg)
        (if (= (first msg) :draw)
          (recur (<! input-chan))))))

(defn draw-chan [selector]
  (let [input-chan (chan)
        out-chan   (chan)]
    (drawstart-chan input-chan selector)
    (drawend-chan   input-chan selector)
    (drawer-chan    input-chan selector)
    (go (loop [[msg-name _ :as msg] (<! input-chan)]
          (if (= msg-name :drawstart)
            (do
              (put! out-chan msg)
              (<! (get-drawing input-chan out-chan))))
          (recur (<! input-chan))))
    out-chan))

(def peice-colors [:blue :green :yellow :purple :red])
(def offscreen-dot-position 13)
(def board-size 6)
(def number-colors (count peice-colors))
(def grid-unit-size 45)
(def dot-size 20)
(def corner-offset (- grid-unit-size dot-size))

(defn rand-colors []
  (map #(get peice-colors (rand-int %))
   (repeat number-colors)))

(def reverse-board-position (partial - (dec board-size)))
(def pos->coord #(+ corner-offset (* grid-unit-size %)))

(def offscreen-offset (-> offscreen-dot-position reverse-board-position pos->coord -))

(defn pos->corner-coord [[xpos ypos]]
  (mapv pos->coord [(reverse-board-position ypos) xpos]))

(defn pos->center-coord [dot-pos]
  (mapv #(+ (/ dot-size 2) %) (pos->corner-coord dot-pos)))

(defn starting-dot [pos color]
  (let [[start-top left] (pos->corner-coord [(first pos) offscreen-dot-position])
        class (str "dot " (name color))
        style (str "top:" start-top "px; left: " left "px;")]
    [:div {:class class :style style}]))

(defn board [{:keys [board] :as state}]
  [:div.dots-game
   [:div.header 
    [:div.heads "Time " [:span.time-val]]
    [:div.heads "Score " [:span.score-val]]]
   [:div.board-area
    [:div.chain-line]
    [:div.board]]])

(defn create-dot [xpos ypos color]
  {:color color :elem (crate/html (starting-dot [xpos ypos] color))})

(defn top-coord-from-dot-elem [$elem]
  (- (int (last (re-matches #".*translate3d\(.*,(.*)px,.*\).*"
                            (attr $elem "style"))))
     offscreen-offset))

(defn translate-top [top]
  (str "translate3d(0," (+ offscreen-offset top) "px,0) "))

(defn remove-dot [{:keys [elem] :as dot}]
  (go
   (let [$elem ($ elem)
         top (top-coord-from-dot-elem $elem)]
     (css $elem {"-webkit-transform"
                 (str (translate-top top) " scale3d(0.1,0.1,0.1)")})
     (<! (timeout 200))
     (.remove ($ elem)))))

(defn at-correct-postion? [dot pos]
  (let [[ex-top _] (pos->corner-coord pos)
        trans-top (top-coord-from-dot-elem ($ (dot :elem)))]
    (= ex-top trans-top)))

(defn update-dot [dot pos]
  (if dot
    (let [$elem ($ (dot :elem))
          [top left] (pos->corner-coord pos)]
      (css $elem {"-webkit-transform" (translate-top top)}))))

(defn add-dots-to-board [dots]
  (doseq [{:keys [elem]} dots]
    (append ($ ".board") elem)))

(defn render-view [state]
  (let [view-dom (crate/html (board state))]
      (inner ($ ".container") view-dom)
      (mapv add-dots-to-board (state :board))))

(defn dot-index [offset {:keys [x y]}]
  (let [[x y] (map - [x y] offset [12 12])]
    (let [ypos (- (dec board-size) (int (/ y grid-unit-size)))
          xpos (int (/ x grid-unit-size))]
      (if (and
           (> board-size ypos)
           (> board-size xpos))
        [xpos ypos]))))

(defn dot-follows? [state prev-dot cur-dot]
  (let [board (state :board)
        prev-dot-color (-> board (get-in prev-dot) :color)
        cur-dot-color (-> board (get-in cur-dot) :color)]
    (or (nil? prev-dot)
        (and
         (= prev-dot-color cur-dot-color)
         (let [[cx cy] cur-dot
               [px py] prev-dot]
           (or
            (and (or (= cx (inc px))
                     (= cx (dec px)))
                 (= cy py))
            (and (or (= cy (inc py))
                     (= cy (dec py)))
                 (= cx px))))))))

(defn add-dot [{:keys [dot-chain] :as state} dot-pos]
  (if (dot-follows? state (last dot-chain) dot-pos)
    (assoc state :dot-chain (conj (or dot-chain []) dot-pos))
    state))

(defn render-chain-element [last-pos pos color]
  (let [[top1 left1] (pos->center-coord last-pos)
        [top2 left2] (pos->center-coord pos)
        [width height] (if (= left1 left2) [5 50] [50 5])
        style (str "width: " width "px; height: " height "px; top:"
                   (- (min top1 top2) 2)
                   "px; left: "
                   (- (min left1 left2) 2) "px;")]
    [:div {:style style :class (str "line " (name (or color :blue)))}]))

(defn render-dot-chain [state]
  (let [dot-chain (state :dot-chain)
        color (-> state :board
                  (get-in (first (state :dot-chain)))
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
          (if-let [dot-pos ((state :dot-index) point)]
            (add-dot state dot-pos)
            state)))))))

(defn render-remove-dots-row-helper [dot-chain-set col]
  (let [dots-to-remove (keep-indexed #(if (dot-chain-set %1) %2) col)
        next_col     (keep-indexed #(if (not (dot-chain-set %1)) %2) col)]
    (doseq [dot dots-to-remove]
      (remove-dot dot))
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
                        (repeat offscreen-dot-position)
                        (take (- board-size (count col)) (rand-colors)))]
      (add-dots-to-board new-dots)
      (vec (concat col new-dots)))))

(defn add-missing-dots [{:keys [board] :as state}]
    (assoc state :board
           (vec
            (map-indexed
             #(add-missing-dots-helper %1 %2)
             board))))

(defn render-position-updates-helper [col-idx col]
  (go
   (loop [cur-col col
          pos 0]
     (let [dot (first cur-col)
           xd  (rest cur-col)]
       (if (not (nil? dot))
         (do
           (if (not (at-correct-postion? dot [col-idx pos]))
             (do
               (<! (timeout 40))
               (update-dot dot [col-idx pos])))
           (recur xd (inc pos))))))))

(defn render-position-updates [{:keys [board]}]
  (doall
   (map-indexed
    #(render-position-updates-helper %1 %2)
    board)))

(defn render-score [{:keys [score]}]
  (inner ($ ".score-val") score))

(defn game-timer [seconds]
  (go (loop [timer (timeout 1000)
             time  seconds]
        (inner ($ ".time-val") time)
        (<! timer)
        (if (= 0 time)
          time
          (recur (timeout 1000) (dec time))))))

(defn create-board []
  (vec
   (map-indexed
    (fn [i x] (vec (map-indexed (partial create-dot i) (take board-size (rand-colors))))) 
    (range board-size))))

(defn setup-game-state []
  (let [init-state {:board (create-board)}]
    (render-view init-state)
    (let [board-offset ((juxt :left :top) (offset ($ ".board")))]
      (assoc init-state :dot-index
             (partial dot-index board-offset)
             :dot-chain [] :score 0))))

(defn app-loop [init-state]
  (let [draw-ch (draw-chan "body")
        game-over-ch (game-timer 60)]
    (go
     (loop [state init-state]
       (render-score state)
       (render-position-updates state)
       (let [state (add-missing-dots state)]
         (<! (timeout 200))
         (render-position-updates state)
         (let [[value ch] (alts! [(get-dots draw-ch state) game-over-ch])]
           (if (= ch game-over-ch)
             state ;; game over 
             (recur
              (let [{:keys [dot-chain]} value]
                (if (< 1 (count dot-chain))
                  (-> state
                      (render-remove-dots dot-chain)
                      (assoc :score (+ (state :score) (count (set dot-chain)))))
                  state)
                )))))))))

(app-loop (setup-game-state))
