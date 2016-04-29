(defproject dots "0.1.0-SNAPSHOT"
  :description "A game written using ClojureScript and core.async"
  :url ""
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.7.228"]
                 [org.clojure/core.async "0.2.374"]
                 ;; for todos async
                 [jayq "2.4.0"]
                 [crate "0.2.5"]]

  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"]
  
  :plugins [[lein-cljsbuild "1.1.3"]
            [lein-figwheel "0.5.2"]]
  :cljsbuild {
              :builds [{:id "prod"
                        :source-paths ["src"]
                        :compiler {:output-to "resources/public/js/compiled/dots.js"
                                   :externs ["resources/public/js/externs/jquery-1.9.js"]
                                   ; :optimizations :whitespace
                                   :optimizations :advanced
                                   ;:pseudo-names true
                                   ;; :source-map "resources/public/js/compiled/dots.map"
                                   ;:pretty-print true
                                   }}]})
