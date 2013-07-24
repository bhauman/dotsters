(defproject dots "0.1.0-SNAPSHOT"
  :description "A game written using ClojureScript and core.async"
  :url ""
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                 [org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-1844"]
                 [core.async "0.1.0-SNAPSHOT"]
                 [crate "0.2.4"]
                 [jayq "2.4.0"]
                 [com.cemerick/piggieback "0.0.5"]
                 ]
  :profiles {:dev {:source-paths ["dev"]}}  
  :plugins [[lein-cljsbuild "0.3.2"]]
  ;;:repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
  :cljsbuild {
              :builds [{:source-paths ["src/dots"]
                        :compiler {:output-to "resources/public/js/compiled/dots.js"
                                   :externs ["resources/public/js/externs/jquery-1.9.js"]
                                   :optimizations :simple
                                   ;; :source-map "resources/public/js/compiled/dots.map"
                                   :pretty-print true}}
                       {:id "pigrep"
                        :source-paths ["src/pigrep"]
                        :compiler {:output-to "resources/public/js/compiled/pigrep.js"
                                   :optimizations :simple
                                   :pretty-print true}}]})
