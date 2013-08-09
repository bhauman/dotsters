(ns dots.anim
  (:require [clojure.string]))

(defn ease-out-bounce [time, begin-x, change-x, duration]
  (let [easeit (fn [t offset] (-> (* t t 7.5625)
                                 (+ offset)
                                 (* change-x)
                                 (+ begin-x)))
        time (/ time duration)]
    (condp > time
      (/ 1 2.75) (easeit time 0)
      (/ 2 2.75) (easeit (- time (/ 1.5 2.75)) 0.75)
      (/ 2.5 2.75) (easeit (- time (/ 2.25 2.75)) 0.9375)
      (easeit (- time (/ 2.625 2.75)) 0.984375))))

(defn direction-change [dir [x & xd] pos]
  (if (nil? xd)
    (list [pos x])
    (if (dir x (first xd))
      (cons [pos x] (direction-change (if (= dir <) > <) xd (inc pos)))
      (direction-change dir xd (inc pos)))))

(defn get-bounce [start-y end-y]
  (let [positions (map #(int (ease-out-bounce % start-y (- end-y start-y) 100))
                       (range 100))
        low-high (direction-change > (rest (rest positions)) 0)
        lows     (keep-indexed #(if (even? %1) %2) low-high)
        highs    (keep-indexed #(if (odd? %1) %2) low-high)]
    (concat [[0 start-y]]
            (interleave (map (fn [[p c]] [p end-y]) lows) highs)
            [[100 end-y]])))

(defn translate-y [ycoord] (str "-webkit-transform: translate3d(0," ycoord "px,0);"
                                "-moz-transform: translate(0," ycoord "px);"
                                "-ms-transform: translate(0," ycoord "px);"))

(defn frame [index [perc coord]]
  (str perc "% { "
       (translate-y coord)
       " -webkit-animation-timing: " (if (even? index) "ease-in;" "ease-out;")
       "}"))

(defn frames [start-y end-y]
  (clojure.string/join "\n"
   (map-indexed frame (get-bounce start-y end-y))))

(defn anim-template [level-name start-y end-y]
  (str
   "." level-name " {\n"
   "-ms-transform: translate(0," end-y "px);\n"      
   "-moz-transform: translate(0," end-y "px);\n"   
   "-webkit-transform: translate3d(0," end-y "px,0);\n"
   "-webkit-animation-name: " level-name ";\n"
   "}\n"
   "@-webkit-keyframes " (name level-name) "{\n" 
   (frames start-y end-y)
   "\n}\n"))

(defn create-anim [start offset-units board-size grid-unit-size]
  (let [labeler (if (neg? start) #(str "level-" %) #(str "level-" % "-from" start))
        unit-to-position #(+ (* offset-units grid-unit-size)
                             (* grid-unit-size %))]
    (clojure.string/join "\n"
                         (map anim-template
                              (map labeler (range (inc start) board-size))
                              (repeat (if (neg? start) 0 (unit-to-position start)))
                              (map unit-to-position
                                   (range (inc start) board-size)))
                         )))

(defn create-all-anim-css [offset-units board-size grid-unit-size]
  (clojure.string/join "\n "
                       (map #(create-anim % offset-units board-size grid-unit-size)
                            (range -1 (dec board-size)))))
