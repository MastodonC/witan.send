(ns witan.send.report
  (:require [clojure.java.io :as io]
            [clj-time.core :as t]
            [clj-time.format :as f]
            [clojure.java.shell :as sh]
            [clojure.string :as str]))

(def send-report (atom []))

(def send-report-file "target/SEND_Report.md")

(defn italic [string]
  (str "_" string "_"))

(defn bold [string]
  (str "__" string "__"))

(defn heading [string]
  (str "## " string))

(defn link [string url]
  (str "[" string "](" url ")"))

(defn reset-send-report []
  (sh/sh "git" "fetch" "--tags")
  (let [model-version (heading (str "SEND Model " (:out (sh/sh "git" "describe" "--abbrev=0" "--tags"))))
        run-datetime (str (italic "Run on") " " (f/unparse-local (f/formatter "YYYY-MM-dd") (t/today)) " " (italic "at") " " (f/unparse (f/formatter "HH:mm") (t/now)))
        os-version (str (System/getProperty "os.name") " " (System/getProperty "os.version"))
        clj-version (str "Clojure " (clojure-version) ", JVM " (System/getProperty "java.vm.version"))
        git-branch (str "Using branch: " (bold (str/join "" (drop-last (:out (sh/sh "git" "rev-parse" "--symbolic-full-name" "--abbrev-ref" "HEAD"))))))
        git-commit-id (str/join "" (drop-last (str/replace (:out (sh/sh "git" "log" "--format=\"%H\"" "-n" "1")) "\"" "")))
        git-commit-info (str "ID of last commit: " (link (str/join "" (take 7 git-commit-id)) (str "https://github.com/MastodonC/witan.send/commit/" git-commit-id)) "\n\n")]
    (reset! send-report [model-version run-datetime os-version clj-version git-branch git-commit-info])))

(defn info [& messages]
  (swap! send-report conj (apply str messages)))

(defn write-send-report []
  (io/delete-file send-report-file :quiet)
  (spit send-report-file (str/join "\n" @send-report) :append true))
