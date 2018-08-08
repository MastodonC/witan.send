(defproject witan.send "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript  "1.9.229"]
                 [reagent "0.6.0"]
                 [re-frame "0.9.4"]
                 [org.hdrhistogram/HdrHistogram "2.1.9"]
                 [net.mikera/core.matrix "0.55.0" :exclusions [org.clojure/clojure]]
                 [org.clojure/test.check "0.9.0"]
                 [org.clojure/data.avl "0.0.17"]
                 [org.clojure/data.csv "0.1.3"]
                 [prismatic/schema "1.1.3"]
                 [schema-contrib "0.1.3"]
                 [kixi/stats "0.3.9"]
                 [instaparse "1.4.3"]
                 [incanter "1.5.7"]
                 [core-matrix-gorilla "0.1.0"]
                 [kixi/gg4clj "0.1.1-SNAPSHOT" :exclusion [org.clojure/clojure]]
                 [vector-of-maps-gorilla "0.1.0-SNAPSHOT"]
                 [incanter-gorilla "0.1.0"]
                 [medley "1.0.0"]
                 [cheshire "5.7.1"]
                 [org.apache.commons/commons-math3 "3.6.1"]
                 [me.raynes/fs "1.4.6"]
                 [aero "1.1.3"]
                 [digest "1.4.8"]
                 [clj-time "0.13.0"]
                 [org.clojure/math.combinatorics "0.1.4"]]
  :plugins [[lein-gorilla "0.4.0"]
            [lein-cljsbuild "1.1.4"]
            [cider/cider-nrepl "0.18.0-SNAPSHOT"]]
  :main witan.send.main
  :aot [witan.send.main]

  :source-paths ["src/clj"]
  :target-path "target/%s"
  :profiles {:dev {:dependencies [[criterium "0.4.4"]
                                  [binaryage/devtools "0.8.2"]]
                   :plugins [[lein-binplus "0.6.2"]]}
             :uberjar {:aot :all}}
  :jvm-opts ["-Xmx8g"]

  :bin {:name "send"
        :bin-path "~/bin"}
  )
