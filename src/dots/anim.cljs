(ns dots.anim)

(defn translate-y [ycoord] (str "-webkit-transform: translate3d(0," ycood "px,0);"))

(defn anim-template [level-name start-y end-y]
  (list "." (name level-name) "{"
        "-webkit-transform: " (translate-y end-y)
        "-webkit-animation-name: " (name level-name) ";"
        "}"
        "@-webkit-keyframes " (name level-name) "{"
        "0% { " (translate-y 0) "}"
        "60% { " (translate-y endy) "}"
        "90% { " (translate-y (- endy 15)) "}"
        "100% { " (translate-y endy) "}"
        "}"))
