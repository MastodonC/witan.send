(ns witan.send.report
  (:require [clojure.java.io :as io]
            [witan.send.metadata :as md]
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

(defn info [& messages]
  (swap! send-report conj (apply str messages)))

(defn write-send-report
  ([]
   (write-send-report send-report-file))
  ([report-file]
   (io/delete-file report-file :quiet)
   (spit report-file (str/join "\n" @send-report) :append true)))

(defn reset-send-report
  [md]
  (let [model-version (heading (str "SEND Model " (md :model-version)))
        run-datetime (str (italic "Run on") " " (md :date) " " (italic "at") " " (md :time))
        os-version (str (md :os-name) " " (md :os-version))
        clj-version (str "Clojure " (md :clj-version) ", JVM " (md :jvm-version))
        git-branch (str "Using branch: " (bold (md :git-branch)))
        git-commit-info (str "ID of last commit: "
                             (link (md :git-commit-id)
                                   (md :git-url))
                             "\n\n")]
    (reset! send-report [model-version run-datetime os-version clj-version git-branch git-commit-info])))

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
      (reset-send-report (md/runtime-metadata) )
      (dorun (map report-fn [["Input Data: " (str/join ", " (vals file-inputs))]
                             ["Number of iterations: " (:simulations run-parameters)]
                             ["Output charts produced: " (:run-reports output-parameters)]
                             ["Modifying: " (if (nil? wt) "None" (str/join ", " wt))]
                             ["Transitions modifier: " mtb]
                             ["Transitions file: " tm]
                             ["Modify transitions from: " mtf]
                             ["Filter transitions from: " ftf]
                             ["Splice NCY: " (str sn "\n")]])))))
