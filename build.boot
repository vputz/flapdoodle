(set-env!
 :resource-paths #{"src"} ; "html"
 :checkouts '[[core.matrix.complex "0.0.1-SNAPSHOT"]; [apache-commons-matrix "0.3.0-SNAPSHOT"]
              ]
 :dependencies '[;[adzerk/boot-cljs            "1.7.48-5"      :scope "test"]
                                        ;[adzerk/boot-cljs-repl       "0.3.3"          :scope "test"]
                                        ;[adzerk/boot-reload          "0.5.1"          :scope "test"]
                                        ;[pandeiro/boot-http          "0.7.6" :scope "test"]
                                        ;[crisptrutski/boot-cljs-test "0.2.2-SNAPSHOT" :scope "test"]
                 [org.clojure/clojure         "1.9.0"]
                 [org.clojure/tools.trace "0.7.9"]
                                        ;[org.clojure/clojurescript   "1.9.518"]
                                        ;[com.cemerick/piggieback     "0.2.1"          :scope "test"]
                                        ;[weasel                      "0.7.0"          :scope "test"]
                                        ;[cljsjs/d3 "4.3.0-4"]
                                        ;[cljsjs/three "0.0.84-0"]
                 [com.taoensso/timbre "4.10.0"]
                                        ;[devcards "0.2.2"]
                                        ;[reagent "0.6.0"]
                                        ;[re-frame "0.8.0"]
                                        ;[hiccup "1.0.5"]
                                        ;                 [net.mikera/clojure-utils "0.8.0"]
                 [net.mikera/core.matrix "0.61.0"]
                 [core.matrix.complex "0.0.1-SNAPSHOT"]
                                        ;[net.mikera/vectorz-clj "0.47.0"]
                 [complex "0.1.11"]
                 [tolitius/boot-check "0.1.6"]
                 [lein-marginalia "0.9.1"]
                 [it.frbracch/boot-marginalia "0.1.3-1" :scope "test"]
                 [org.clojure/tools.nrepl     "0.2.13"         :scope "test"]
                 ])

(require
                                        ;  '[adzerk.boot-cljs      :refer [cljs]]
                                        ;  '[adzerk.boot-cljs-repl :refer [cljs-repl start-repl]]
                                        ;  '[adzerk.boot-reload    :refer [reload]]
                                        ;  '[crisptrutski.boot-cljs-test  :refer [exit! test-cljs]]
 '[tolitius.boot-check :as check]
 '[it.frbracch.boot-marginalia :refer [marginalia]]
                                        ;  '[pandeiro.boot-http    :refer [serve]]
 )

(deftask testing []
                                        ;  (merge-env! :resource-paths #{"test"})
  identity)

(deftask auto-test []
  (comp (testing)
        (watch)
        (speak)
                                        ;       (test-cljs)
        ))

(deftask dev []
  (comp ;(serve :dir "target/")
   (watch)
   (speak)
                                        ;(reload :on-jsload 'app.core/main)
                                        ;(cljs-repl)
                                        ;(cljs :source-map true :optimizations :none)
   ))

(deftask test []
  (comp (testing)
                                        ;(test-cljs)
                                        ;        (exit!)
        ))


(swap! boot.repl/*default-dependencies*
       concat '[[cider/cider-nrepl "0.16.0"]
                [refactor-nrepl "2.4.0-SNAPSHOT"]])

(swap! boot.repl/*default-middleware*
       concat '[cider.nrepl/cider-middleware
                refactor-nrepl.middleware/wrap-refactor])

(deftask cider "CIDER profile"
  []
  (require 'boot.repl)
  (swap! @(resolve 'boot.repl/*default-dependencies*)
         concat '[[org.clojure/tools.nrepl "0.2.13"]
                  [cider/cider-nrepl "0.16.0"]
                  [refactor-nrepl "2.4.0-SNAPSHOT"]])
  (swap! @(resolve 'boot.repl/*default-middleware*)
         concat '[cider.nrepl/cider-middleware
                  refactor-nrepl.middleware/wrap-refactor])
  identity
  )
