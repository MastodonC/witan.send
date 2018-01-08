(defproject witan.send "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[kixi/gg4clj "0.1.1-SNAPSHOT" :exclusion [org.clojure/clojure]]
                 [org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript  "1.9.229"]
                 [reagent "0.6.0"]
                 [re-frame "0.9.4"]
                 [org.hdrhistogram/HdrHistogram "2.1.9"]
                 [net.mikera/core.matrix "0.55.0" :exclusions [org.clojure/clojure]]
                 [org.clojure/test.check "0.9.0"]
                 [org.clojure/data.avl "0.0.17"]
                 [org.clojure/data.csv "0.1.3"]
                 [witan.workspace-api "0.1.22" :exclusions [org.clojure/clojure]]
                 [prismatic/schema "1.1.3"]
                 [schema-contrib "0.1.3"]
                 [kixi/stats "0.3.9"]
                 [instaparse "1.4.3"]
                 [incanter "1.5.7"]
                 [core-matrix-gorilla "0.1.0"]
                 [kixi/gg4clj "0.1.1-SNAPSHOT" :exclusion [org.clojure/clojure]]
                 [vector-of-maps-gorilla "0.1.0-SNAPSHOT"]
                 [incanter-gorilla "0.1.0"][medley "1.0.0"]
                 [cheshire "5.7.1"]
                 [org.apache.commons/commons-math3 "3.6.1"]]
  :plugins [[lein-gorilla "0.4.0"]
            [lein-cljsbuild "1.1.4"]
            [lein-auto "0.1.3"]
            [cider/cider-nrepl "0.14.0"]]
  :main witan.send.main
  :aot [witan.send.main]

  :source-paths ["src/clj" "src/cljs" "src/cljc"]
  :target-path "target/%s"
  :profiles {:dev {:dependencies [[witan.workspace-executor "0.2.6"
                                   :exclusions [witan.workspace-api]]
                                  [criterium "0.4.4"]
                                  [clj-time "0.13.0"]
                                  [binaryage/devtools "0.8.2"]
                                  [figwheel-sidecar "0.5.9"]
                                  [com.cemerick/piggieback "0.2.1"]]}
             :plugins [[lein-figwheel "0.5.9"]]}
  :jvm-opts ["-Xmx8g"]

  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"]

  :figwheel {:css-dirs ["resources/public/css"]}

  :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}

  :cljsbuild
  {:builds
   [{:id           "dev"
     :source-paths ["src/cljs" "src/cljc"]
     :figwheel     {:on-jsload "witan.send.core/mount-root"}
     :compiler     {:main                 witan.send.core
                    :output-to            "resources/public/js/compiled/app.js"
                    :output-dir           "resources/public/js/compiled/out"
                    :asset-path           "js/compiled/out"
                    :source-map-timestamp true
                    :preloads             [devtools.preload]
                    :external-config      {:devtools/config {:features-to-install :all}}
                    }}

    {:id           "min"
     :source-paths ["src/cljs" "src/cljc"]
     :compiler     {:main            witan.send.core
                    :output-to       "resources/public/js/compiled/app.js"
                    :optimizations   :advanced
                    :closure-defines {goog.DEBUG false}
                    :pretty-print    false}}


    ]})
