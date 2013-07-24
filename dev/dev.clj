(ns dev
  (:require
   [cemerick.piggieback]
   [cljs.repl.browser]))

(defn brepl []
  (cemerick.piggieback/cljs-repl
   :repl-env (doto (cljs.repl.browser/repl-env :port 9000)
               cljs.repl/-setup)))
