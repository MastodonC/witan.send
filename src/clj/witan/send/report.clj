(ns witan.send.report
  (:require [clojure.java.io :as io]
            [clj-time.core :as t]
            [clj-time.format :as f]
            [clojure.java.shell :as sh]))

(def send-report (atom []))

(defn reset-send-report []
  (sh/sh "git" "fetch" "--tags")
  (let [model-version (str "SEND Model " (:out (sh/sh "git" "describe" "--abbrev=0" "--tags")))
        run-datetime (str "Run on " (f/unparse-local (f/formatter "YYYY-MM-dd") (t/today)) " at " (f/unparse (f/formatter "HH:mm") (t/now)))
        os-version (str (System/getProperty "os.name") " " (System/getProperty "os.version"))
        clj-version (str "Clojure " (clojure-version) ", JVM " (System/getProperty "java.vm.version"))
        git-branch (str "Using branch: " (:out (sh/sh "git" "rev-parse" "--symbolic-full-name" "--abbrev-ref" "HEAD")) "\n")]
    (reset! send-report [model-version run-datetime os-version clj-version git-branch])))

(defn info [& messages]
  (swap! send-report conj (apply str messages)))

(defn write-send-report []
  (io/delete-file "target/SEND_Report.txt" :quiet)
  (doseq [line @send-report]
    (spit "target/SEND_Report.txt" (println-str line) :append true)))