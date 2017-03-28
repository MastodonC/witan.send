(defproject witan.send "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [net.mikera/core.matrix "0.55.0"]
                 [org.clojure/data.csv "0.1.3"]
                 [witan.workspace-api "0.1.22"]
                 [prismatic/schema "1.1.3"]
                 [schema-contrib "0.1.3"]
                 [instaparse "1.4.3"]
                 [rm-hull/markov-chains "0.1.0"]]
  :main ^:skip-aot witan.send
  :target-path "target/%s"
  :profiles {:dev {:dependencies [[witan.workspace-executor "0.2.6"
                                   :exclusions [witan.workspace-api]]
                                  [criterium "0.4.4"]
                                  [clj-time "0.13.0"]]}})
