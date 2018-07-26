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
        run-datetime (str (italic "Run on") " " (f/unparse-local (f/formatter "YYYY-MM-dd") (t/today)) " " (italic "at") " " (f/unparse-local (f/formatter-local "HH:mm") (t/time-now)))
        os-version (str (System/getProperty "os.name") " " (System/getProperty "os.version"))
        clj-version (str "Clojure " (clojure-version) ", JVM " (System/getProperty "java.vm.version"))
        git-branch (str "Using branch: " (bold (str/join "" (drop-last (:out (sh/sh "git" "rev-parse" "--symbolic-full-name" "--abbrev-ref" "HEAD"))))))
        git-commit-id (str/join "" (drop-last (str/replace (:out (sh/sh "git" "log" "--format=\"%H\"" "-n" "1")) "\"" "")))
        git-commit-info (str "ID of last commit: " (link (str/join "" (take 7 git-commit-id)) (str "https://github.com/MastodonC/witan.send/commit/" git-commit-id)) "\n\n")]
    (reset! send-report [model-version run-datetime os-version clj-version git-branch git-commit-info])))

(defn info [& messages]
  (swap! send-report conj (apply str messages)))

(defn write-send-report
  ([]
   (write-send-report send-report-file))
  ([report-file]
   (io/delete-file report-file :quiet)
   (spit report-file (str/join "\n" @send-report) :append true)))

(defn generate-report-header
  "Build a header for the report, expects a full config map"
  [{:keys [file-inputs schema-inputs transition-parameters run-parameters output-parameters]}]
  (let [wt  (:which-transitions? transition-parameters)
        mtb (or (:modify-transition-by transition-parameters) "None")
        tm  (or (:transition-matrix file-inputs) "None")
        mtf (or (:modify-transitions-from run-parameters) "None")
        ftf (or (:filter-transitions-from transition-parameters) "None")
        sn  (or (:splice-ncy transition-parameters) "None")
        report-fn (fn [[k v]] (info k (bold v)))]
    (when (output-parameters :run-report-header)
      (reset-send-report)
      (dorun (map report-fn [["Input Data: " (str/join ", " (vals file-inputs))]
                             ["Number of iterations: " (:simulations run-parameters)]
                             ["Output charts produced: " (:run-reports output-parameters)]
                             ["Modifying: " (if (nil? wt) "None" (str/join ", " wt))]
                             ["Transitions modifier: " mtb]
                             ["Transitions file: " tm]
                             ["Modify transitions from: " mtf]
                             ["Filter transitions from: " ftf]
                             ["Splice NCY: " (str sn "\n")]])))))
