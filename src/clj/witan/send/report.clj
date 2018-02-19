(ns witan.send.report
  (:require [clojure.java.io :as io]
            [clj-time.core :as t]
            [clj-time.format :as f]
            [clojure.java.shell :as sh]))

(def send-report (atom []))

(defn reset-send-report []
  (sh/sh "git" "fetch" "--tags")
  (reset! send-report [(str "SEND Model " (:out (sh/sh "git" "describe" "--abbrev=0" "--tags")))])
  (swap! send-report conj (str "Run on " (f/unparse-local (f/formatter "YYYY-MM-dd") (t/today)) " at " (f/unparse (f/formatter "HH:mm") (t/now))))
  (swap! send-report conj (str (System/getProperty "os.name") " " (System/getProperty "os.version")))
  (swap! send-report conj (str "Clojure " (clojure-version) ", JVM " (System/getProperty "java.vm.version")))
  (swap! send-report conj (str "Using branch: " (:out (sh/sh "git" "rev-parse" "--symbolic-full-name" "--abbrev-ref" "HEAD")) "\n")))

(defn info [message]
  (swap! send-report conj message))

(defn write-send-report []
  (io/delete-file "target/SEND_Report.txt" :quiet)
  (doseq [line @send-report]
    (spit "target/SEND_Report.txt" (println-str line) :append true)))
