(defproject data15-blackjack "0.1.0-SNAPSHOT"
  :description "Multiplayer blackjack game using Tableau JS API"
  :url "http://github.com/tfoldi/data15-blackjack"
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.122"]

                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]

                 [com.taoensso/sente "1.7.0-beta2"]         ; <--- Sente
                 [com.taoensso/timbre "4.1.2"]

                 ;;; ---> Choose (uncomment) a supported web server <---
                 [http-kit "2.1.19"]
                 ;; [org.immutant/web       "2.1.0"] ; v2.1+ recommended
                 ;; [nginx-clojure/nginx-clojure-embed "0.4.2"] ; Needs v0.4.2+

                 [ring "1.4.0"]
                 [ring/ring-defaults "0.1.5"]
                 [compojure "1.4.0"]                        ; Or routing lib of your choice
                 [hiccup "1.0.5"]                           ; Optional, just for HTML

                 ;;; Transit deps optional; may be used to aid perf. of larger data payloads
                 ;;; (see reference example for details):
                 [com.cognitect/transit-clj "0.8.281"]
                 [com.cognitect/transit-cljs "0.8.225"]]

  :plugins [[lein-cljsbuild "1.1.0"]
            [lein-pprint "1.1.2"]
            [lein-ancient "0.6.7"]
            [com.cemerick/austin "0.1.6"]
            [lein-ring "0.9.7"]]
  :hooks [leiningen.cljsbuild]
  :source-paths ["src/clj"]
  :cljsbuild {
              :builds {
                       :main {
                              :source-paths ["src/cljs"]
                              :compiler     {:output-to     "resources/public/js/client.js"
                                             :optimizations :simple
                                             :pretty-print  true}
                              :jar          true}}}
  :main data15-blackjack.server
  :ring {:handler data15-blackjack.server/my-ring-handler})
