(defproject witan.send "0.1.0-SNAPSHOT"
  :description "Project demand for Special Educational Needs and Disability provision"
  :url "http://github.com/mastodonc/witan.send"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.hdrhistogram/HdrHistogram "2.1.9"]
                 [net.mikera/core.matrix "0.55.0" :exclusions [org.clojure/clojure]]
                 [org.clojure/test.check "0.9.0"]
                 [org.clojure/data.csv "0.1.3"]
                 [prismatic/schema "1.1.3"]
                 [schema-contrib "0.1.3"]
                 [kixi/stats "0.3.9"]
                 [instaparse "1.4.3"]
                 [medley "1.0.0"]
                 [org.apache.commons/commons-math3 "3.6.1"]
                 [me.raynes/fs "1.4.6"]
                 [aero "1.1.3"]
                 [digest "1.4.8"]
                 [clj-time "0.13.0"]]
  :plugins [[lein-cloverage "1.0.13"]]
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
