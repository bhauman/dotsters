(ns dots.anim
  (:require 'clojure.string))

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
  (let [positions (map #(int (ease-out-bounce % start-y (- end-y start-y) 100)) (range 100))
        low-high (direction-change > (rest (rest positions)) 0)
        lows     (keep-indexed #(if (even? %1) %2) low-high)
        highs    (keep-indexed #(if (odd? %1) %2) low-high)]
    (concat [[0 start-y]]
            (interleave (map (fn [[p c]] [p end-y]) lows) highs)
            [[100 end-y]])))

(defn translate-y [ycoord] (str "-webkit-transform: translate3d(0," ycoord "px,0);"))

(defn frame [index [perc coord]]
  (str perc "% { "
       (translate-y coord)
       " -webkit-animation-timing: " (if (even? index) "ease-in;" "ease-out;")
       "}"))

(defn frames [start-y end-y]
  (clojure.string/join "\n"
   (map-indexed frame (get-bounce start-y end-y))))

(defn anim-template [level-name start-y end-y]
  (str "@-webkit-keyframes " (name level-name) "{\n" 
       (frames start-y end-y)
        "\n}\n"))

(defn )
