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

(defn select-chan [pred chans]
  (go (loop []
        (let [[value ch] (alts! chans)]
          (if (pred value) value (recur))))))

(defn click-chan [selector msg-name]
  (let [rc (chan)
        handler (fn [e] (jq/prevent e) (put! rc [msg-name]))]
    (on ($ "body") :click selector {} handler)
    (on ($ "body") "touchend" selector {} handler)
    rc))

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
          (when (= msg-name :drawstart)
            (put! out-chan msg)
            (<! (get-drawing input-chan out-chan)))
          (recur (<! input-chan))))
    out-chan))

(def dot-colors [:blue :green :yellow :purple :red])  
(def offscreen-dot-position 8)
(def board-size 6)
(def number-colors (count dot-colors))
(def grid-unit-size 45)
(def dot-size 22)
(def corner-offset (- grid-unit-size dot-size))

(defn rand-colors [exclude-color]
  (log "getting colors" (prn-str exclude-color))
  (let [colors (if exclude-color (vec (remove (partial = exclude-color) dot-colors))
                   dot-colors)
        number-colors (if exclude-color (dec number-colors) number-colors)]
    (log "getting colors" (prn-str colors))
    (map #(get colors (rand-int %))
         (repeat number-colors))))

(def reverse-board-position (partial - (dec board-size)))
(def pos->coord #(+ corner-offset (* grid-unit-size %)))

(def offscreen-offset (-> offscreen-dot-position reverse-board-position pos->coord -))

(defn pos->corner-coord [[xpos ypos]]
  (mapv pos->coord [(reverse-board-position ypos) xpos]))

(defn pos->center-coord [dot-pos]
  (mapv #(+ (/ dot-size 2) %) (pos->corner-coord dot-pos)))

(defn starting-dot [[top-pos _] color]
  (let [[start-top left] (pos->corner-coord [top-pos offscreen-dot-position])
        style (str "top:" start-top "px; left: " left "px;")]
    [:div {:class (str "dot levelish " (name color)) :style style}]))

(defn colorize-word [word]
  (map (fn [x c] [:span {:class (name c)} x]) word (rand-colors)))

(defn start-screen []
  [:div.dots-game
   [:div.notice-square
    [:div.marq (colorize-word "SHAPES")]
    [:div.control-area
     [:a.start-new-game {:href "#"} "new game"]]]])

(defn score-screen [score]
  [:div.dots-game
   [:div.notice-square
    [:div.marq (concat (colorize-word "SCORE") [[:span " "]]
                       (colorize-word (str score)))]
    [:div.control-area
     [:a.start-new-game {:href "#"} "new game"]]]])

(defn board [{:keys [board] :as state}]
  [:div.dots-game
   [:div.header 
    [:div.heads "Time " [:span.time-val]]
    [:div.heads "Score " [:span.score-val]]]
   [:div.board-area
    [:div.chain-line ]
    [:div.dot-highlights]
    [:div.board]]])

(defn create-dot [xpos ypos color]
  {:color color :elem (crate/html (starting-dot [xpos ypos] color))})

(defn top-coord-from-dot-elem [$elem]
  (- (int (last (re-matches #".*translate3d\(.*,(.*)px,.*\).*"
                            (attr $elem "style"))))
     offscreen-offset))

(defn top-pos-from-dot-elem [$elem]
  (if-let [[_ pos-str] (re-matches #".*level-(\d).*" (attr $elem "class"))]
    (reverse-board-position (int pos-str))))

(defn translate-top [top]
  (str "translate3d(0," (+ offscreen-offset top) "px,0) "))

(defn remove-dot [{:keys [elem] :as dot}]
  (go
   (let [$elem ($ elem)
         top (-> (top-pos-from-dot-elem $elem) reverse-board-position pos->coord)
         trans (translate-top top)]
     (css $elem {"-webkit-transition" "all 0.2s"})
     (css $elem {"-webkit-transform"
                 (str trans " scale3d(0.1,0.1,0.1)")
                 "-moz-transform"
                 (str "translate(0," (+ offscreen-offset top) "px) scale(0.1,0.1)")
                 "-ms-transform"
                 (str "translate(0," (+ offscreen-offset top) "px) scale(0.1,0.1)")})
     (<! (timeout 150))
     (.remove ($ elem)))))

(defn at-correct-postion? [dot [_ expected-top]]
  (= expected-top (top-pos-from-dot-elem ($ (dot :elem)))))

(defn update-dot [dot pos]
  (if dot
    (go
     (let [$elem ($ (dot :elem))
           top (top-pos-from-dot-elem $elem)
           previous-level (if top (str "-from" (reverse-board-position top)) "")]
       (.addClass $elem (str "level-"
                             (reverse-board-position (last pos))
                             previous-level))))))

(defn add-dots-to-board [dots]
  (doseq [{:keys [elem]} dots]
    (append ($ ".dots-game .board") elem)))

(defn render-view [state]
  (let [view-dom (crate/html (board state))]
      (inner ($ ".dots-game-container") view-dom)
      (mapv add-dots-to-board (state :board))))

(defn dot-index [offset {:keys [x y]}]
  (let [[x y] (map - [x y] offset [12 12])]
    (let [ypos (reverse-board-position (int (/ y grid-unit-size)))
          xpos (int (/ x grid-unit-size))]
      (if (and (> board-size ypos -1) (> board-size xpos -1))
        [xpos ypos]))))

(defn dot-color [{:keys [board]} dot-pos]
  (-> board (get-in dot-pos) :color))

(def abs #(.abs js/Math %))

(defn dot-follows? [state prev-dot cur-dot]
  (and (not= prev-dot cur-dot)
       (or (nil? prev-dot)
           (and
            (= (dot-color state prev-dot) (dot-color state cur-dot))
            (= 1 (apply + (mapv (comp abs -) cur-dot prev-dot)))))))

(defn chain-element-templ [last-pos pos color]
  (let [[top1 left1] (pos->center-coord last-pos)
        [top2 left2] (pos->center-coord pos)
        length (- grid-unit-size dot-size)
        vertical (= left1 left2)
        [width height] (if vertical [4 length] [length 4])
        [off-left off-top] (if vertical [-3 11] [11 -3])        
        style (str "width: " width "px;"
                   "height: " height "px;" 
                   "top: " (+ (min top1 top2) off-top) "px;"
                   "left: " (+ (min left1 left2) off-left) "px;")]
    [:div {:style style :class (str "line " (name (or color :blue)) (if (< width height) " vert" " horiz" ))}]))

(defn dot-highlight-templ [pos color]
  (let [[top left] (pos->corner-coord pos)
        style (str "top:" top "px; left: " left "px;")]
    [:div {:style style :class (str "dot-highlight " (name color))}]))

(defn render-dot-chain-update [last-state state]
  (let [last-dot-chain (:dot-chain last-state)
        dot-chain      (:dot-chain state)
        last-chain-length (count last-dot-chain)
        chain-length      (count dot-chain)]
    (when (and (not= last-chain-length chain-length) (pos? chain-length))
      (let [color (dot-color state (first dot-chain))
            length-diff            (- chain-length last-chain-length)]
        (if (< 1 chain-length)
          (if (pos? length-diff)
            (append ($ ".dots-game .chain-line")
                    (crate/html (chain-element-templ
                                 (last (butlast dot-chain))
                                 (last dot-chain)
                                 color)))
            (.remove (.last ($ ".dots-game .chain-line .line"))))
          (inner ($ ".dots-game .chain-line") ""))
        (append ($ ".dots-game .dot-highlights")
                (crate/html (dot-highlight-templ (last dot-chain) color)))))))

(defn erase-dot-chain []
  (inner ($ ".dots-game .chain-line") "")
  (inner ($ ".dots-game .dot-highlights") ""))

(defn transition-dot-chain-state [{:keys [dot-chain] :as state} dot-pos]
  (if (dot-follows? state (last dot-chain) dot-pos)
    (if (and (< 1 (count dot-chain))
             (= dot-pos (last (butlast dot-chain))))
      (vec (butlast dot-chain))
      (conj (or dot-chain []) dot-pos))
    dot-chain))

(defn items-with-positions [items]
  (apply concat
         (map-indexed #(map-indexed (fn [i item] (assoc item :pos [%1 i])) %2) items)))

(defn get-all-color-dots [state color]
  (filter #(= color (:color %)) (items-with-positions (state :board))))

(defn dot-positions-for-focused-color [state]
  (let [color (dot-color state (-> state :dot-chain first))]
      (vec (map :pos (get-all-color-dots state color)))))

(defn dot-chain-cycle? [dot-chain]
  (and (< 3 (count dot-chain))
   ((set (butlast dot-chain)) (last dot-chain))))

(defn flash-class [color] (str (name color) "-trans"))

(defn flash-color-on [color]
  (.addClass ($ ".dots-game .board-area") (flash-class color)))

(defn flash-color-off [color]
  (.removeClass ($ ".dots-game .board-area") (flash-class color)))

(defn get-dots-to-remove [draw-ch start-state]
  (go
   (loop [last-state nil
          state start-state]
     (render-dot-chain-update last-state state)
     (if (dot-chain-cycle? (state :dot-chain))
       (let [color (dot-color state (-> state :dot-chain first))]
         (flash-color-on color)
         (<! (select-chan (fn [[msg _]] (= msg :drawend)) [draw-ch]))
         (flash-color-off color)
         (erase-dot-chain)
         (assoc state :dot-chain (dot-positions-for-focused-color state) :exclude-color color))
       (let [[msg point] (<! draw-ch)]
         (if (= msg :drawend)
           (do (erase-dot-chain) state)
           (recur state
                  (if-let [dot-pos ((state :dot-index) point)]
                    (assoc state :dot-chain (transition-dot-chain-state state dot-pos))
                    state))))))))

(defn render-remove-dots-row-helper [dot-chain-set col]
  (let [dots-to-remove (keep-indexed #(if (dot-chain-set %1) %2) col)
        next_col     (keep-indexed #(if (not (dot-chain-set %1)) %2) col)]
    (doseq [dot dots-to-remove]
      (remove-dot dot))
    (vec next_col)))

(defn render-remove-dots [state dot-chain]
  (let [dot-chain-groups  (group-by first dot-chain)
        next_board (map-indexed #(render-remove-dots-row-helper
                                  (set (map last (get dot-chain-groups %1))) %2)
                                (state :board))]
    (assoc state :board (vec next_board))))

(defn add-missing-dots-helper [col-idx col exclude-color]
  (if (= (count col) board-size)
    col
    (let [new-dots (map create-dot
                        (repeat col-idx)
                        (repeat offscreen-dot-position)
                        (take (- board-size (count col)) (rand-colors exclude-color)))]
      (add-dots-to-board new-dots)
      (vec (concat col new-dots)))))

(defn add-missing-dots [{:keys [board exclude-color] :as state}]
    (assoc state :board
           (vec
            (map-indexed
             #(add-missing-dots-helper %1 %2 exclude-color)
             board))
           :exclude-color nil))

(defn render-position-updates-helper [col-idx col]
  (go
   (loop [[dot & xd] col pos 0]
     (when (not (nil? dot))
       (when (not (at-correct-postion? dot [col-idx pos]))
         (<! (timeout 80))
         (update-dot dot [col-idx pos]))
       (recur xd (inc pos))))))

(defn render-position-updates [{:keys [board]}]
  (doall
   (map-indexed
    #(render-position-updates-helper %1 %2)
    board)))

(defn render-score [{:keys [score]}]
  (inner ($ ".score-val") score))

(defn game-timer [seconds]
  (go (loop [timer (timeout 1000) time seconds]
        (inner ($ ".time-val") time)
        (<! timer)
        (if (zero? time)
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
    (let [board-offset ((juxt :left :top) (offset ($ ".dots-game .board")))]
      (assoc init-state :dot-index
             (partial dot-index board-offset)
             :dot-chain [] :score 0))))

(defn game-loop [init-state draw-ch]
  (let [game-over-ch (game-timer 60)]
    (go
     (loop [state init-state]
       (render-score state)
       (render-position-updates state)
       (let [state (add-missing-dots state)]
         (<! (timeout 300))
         (render-position-updates state)
         (let [[value ch] (alts! [(get-dots-to-remove draw-ch state) game-over-ch])]
           (if (= ch game-over-ch)
             state ;; leave game loop
             (recur
              (let [{:keys [dot-chain exclude-color]} value]
                (if (< 1 (count dot-chain))
                  (-> state
                      (render-remove-dots dot-chain)
                      (assoc :score (+ (state :score) (count (set dot-chain)))
                             :exclude-color exclude-color))
                  state)
                )))))))))

(defn render-screen [screen]
  (let [view-dom (crate/html screen)]
    (inner ($ ".dots-game-container") view-dom)))

(defn app-loop []
  (let [draw-ch (draw-chan "body")
        start-chan (click-chan ".dots-game .start-new-game" :start-new-game)]
    (go
     (render-screen (start-screen))
     (<! (select-chan #(= [:start-new-game] %) [start-chan draw-ch]))
     (loop []
       (let [{:keys [score]} (<! (game-loop (setup-game-state) draw-ch))]
         (render-screen (score-screen score)))
       (<! (select-chan #(= [:start-new-game] %) [start-chan draw-ch]))       
       (recur)))))


(app-loop)
