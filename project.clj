(defproject witan.send "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.hdrhistogram/HdrHistogram "2.1.9"]
                 [net.mikera/core.matrix "0.55.0" :exclusions [org.clojure/clojure]]
                 [org.clojure/test.check "0.9.0"]
                 [org.clojure/data.avl "0.0.17"]
                 [org.clojure/data.csv "0.1.3"]
                 [witan.workspace-api "0.1.22" :exclusions [org.clojure/clojure]]
                 [prismatic/schema "1.1.3"]
                 [schema-contrib "0.1.3"]
                 [kixi/stats "0.3.10-SNAPSHOT"]
                 [instaparse "1.4.3"]
                 [incanter "1.5.7"]
                 [medley "1.0.0"]]
  :plugins [[lein-gorilla "0.4.0"]]
  :main witan.send.main
  :aot [witan.send.main]
  :target-path "target/%s"
  :profiles {:dev {:dependencies [[witan.workspace-executor "0.2.6"
                                   :exclusions [witan.workspace-api]]
                                  [criterium "0.4.4"]
                                  [clj-time "0.13.0"]]}}
  :jvm-opts ["-Xmx8g"])
