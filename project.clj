(defproject witan.send "0.1.0-SNAPSHOT"
  :description "Project demand for Special Educational Needs and Disability provision"
  :url "http://github.com/mastodonc/witan.send"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/data.csv "0.1.4"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [org.clojure/test.check "0.9.0"]
                 [aero "1.1.3"]
                 [clj-time "0.15.1"]
                 [digest "1.4.8"]
                 [instaparse "1.4.9"]
                 [kixi/stats "0.4.3"]
                 [me.raynes/fs "1.4.6"]
                 [medley "1.0.0"]
                 [org.apache.commons/commons-math3 "3.6.1"]
                 [org.hdrhistogram/HdrHistogram "2.1.9"] ;; upgrading to 2.1.10 causes a test to fail
                 ]
  :main witan.send.main
  :aot [witan.send.main]
  :source-paths ["src/clj"]
  :target-path "target/%s"
  :profiles {:dev {:dependencies [[criterium "0.4.4"]
                                  [binaryage/devtools "0.9.10"]]
                   :plugins [[lein-binplus "0.6.2"]]}
             :uberjar {:aot :all}}
  :jvm-opts ["-Xmx8g"]
  :bin {:name "send"
        :bin-path "~/bin"})
