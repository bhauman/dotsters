(ns dots.core
  (:require
   [cljs.core.async :as async
             :refer [<! >! chan close! sliding-buffer put! alts! timeout]]
   [jayq.core :refer [$ append ajax inner css $deferred when done resolve pipe on bind attr] :as jq]
   [jayq.util :refer [log]]
   [crate.core :as crate]
   [clojure.string :refer [join blank? replace-first]])
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

(defn draw-chan [selector]
  (let [input-chan (chan)
        rc      (chan)]
    (drawstart-chan input-chan selector)
    (drawend-chan   input-chan selector)
    (drawer-chan    input-chan selector)
    (go (loop []
          (let [val (<! input-chan)]
            (if (= (first val) :drawstart)
              (do
                (put! rc val)
                (loop []
                  (let [val (<! input-chan)]
                    (put! rc val)
                    (if (= (first val) :draw)
                      (recur)))))
              (recur)))
          (recur)))
    rc))

(def initial-dot-position 13)
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
  [(+ 25 (* 45 (- (dec board-size) ypos)))
   (+ 25 (* 45 xpos))])

(defn dot-pos-to-center-position [dot-pos]
  (vec (map (partial + 10) (dot-pos-to-abs-position dot-pos))))

(defn dot [pos color]
  (let [[start-top _] (dot-pos-to-abs-position [0 initial-dot-position])
        [top left] (dot-pos-to-abs-position pos)
        class (str "dot " (name color))
        style (str "top:" start-top "px; left: " left "px;")]
    (log "start-top" start-top)
    [:div {:class class :style style}]))

(defn board [{:keys [board] :as state}]
  [:div.dots-game
   [:div.chain-line]
   [:div.board]])

(defn create-dot [xpos ypos color]
  {:color color :elem (crate/html (dot [xpos ypos] color))})

(defn top-pos-from-transform [$elem]
  (- (int (last (re-matches #".*translate3d\(.*,(.*)px,.*\).*"
                            (attr $elem "style"))))
     335))

(defn remove-dot [{:keys [elem] :as dot}]
  (go
   (let [$elem ($ elem)
         top (top-pos-from-transform $elem)]
     (css $elem {"-webkit-transform"
                 (str
                  "translate3d(0," (+ 335 top) "px,0)" 
                  "scale3d(0.1,0.1,0.1) ")})
     (<! (timeout 200))
     (.remove ($ elem))
     )))



(defn at-correct-postion? [dot pos]
  (let [[ex-top _] (dot-pos-to-abs-position pos)
        trans-top (top-pos-from-transform ($ (dot :elem)))]
    (log "trans-top " (prn-str [ex-top act-top trans-top]))
    (= ex-top trans-top)))

(defn update-dot [dot pos]
  (if dot
    (let [$elem ($ (dot :elem))
          [top left] (dot-pos-to-abs-position pos)]
      (css $elem {;;:top top
                  :left left
                  "-webkit-transform" (str "translate3d(0," (+ 335 top) "px, 0)")
                  }))))

(defn add-dots-to-board [dots]
  (doseq [{:keys [elem]} dots]
    (append ($ ".board") elem)))

(defn render-view [state]
  (inner ($ ".container")
         (crate/html (board state)))
  (mapv add-dots-to-board (state :board)))

(defn dot-index [offset {:keys [x y]}]
  (let [[x y] (map - [x y] offset [12 12])]
    (if (and (< 0 (mod x 45) 45) (< 0 (mod y 45) 45))
      (let [ypos (- (dec board-size) (int (/ y 45)))
            xpos (int (/ x 45))]
        (if (and
             (> board-size ypos)
             (> board-size xpos))
          [xpos ypos])))))

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
  (let [[top1 left1] (dot-pos-to-center-position last-pos)
        [top2 left2] (dot-pos-to-center-position pos)
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
                        (repeat initial-dot-position)
                        (get-rand-colors (- board-size (count col))))]
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

(defn app-loop [init-state]
  (let [draw-ch (draw-chan "body")]
    (render-view init-state)
    (go
     (loop [state init-state]
       (log "before update" (prn-str (map count (state :board))))
       (render-position-updates state)
       (let [state (add-missing-dots state)]
         (<! (timeout 200))
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
           :dot-index (partial dot-index [10 10])
           :dot-chain []})
