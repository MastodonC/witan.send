(ns witan.send.acceptance.workspace-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [witan.send.report :as report]
            [witan.send.schemas :as sc]
            [witan.send.send :as send]
            [witan.send.test-utils :as tu]))

(def file-inputs
  {:settings-to-change "data/demo/modify-settings.csv" 
   :transition-matrix "data/demo/transitions.csv" 
   :population "data/demo/population.csv"
   :setting-cost "data/demo/need-setting-costs.csv" 
   :valid-setting-academic-years "data/demo/valid-setting-academic-years.csv"})

(def schema-inputs
  {:settings-to-change sc/SettingsToChange
   :transition-matrix sc/TransitionCounts
   :population sc/PopulationDataset
   :setting-cost sc/NeedSettingCost
   :valid-setting-academic-years sc/ValidSettingAcademicYears})

(def default-transition-parameters 
  {:filter-transitions-from nil
   :which-transitions? nil
   :splice-ncy nil
   :modify-transition-by 1})

(def default-run-parameters
  {:modify-transitions-from nil
   :random-seed 50
   :simulations 10
   :seed-year 2017})

(def default-output-parameters
  {:run-reports false
   :run-report-header true})

(defn build-input-datasets
  "Build a map of the datasets to use for input"
  ([] (build-input-datasets file-inputs schema-inputs))
  ([fi si]
   (into {} (for [[k v] fi]
              [k (tu/csv-to-dataset v (k si))]))))

(defn generate-report-header
  "Build a header for the report"
  [file-inputs schema-inputs transition-parameters run-parameters output-parameters]
  (let [wt  (:which-transitions? transition-parameters)
        mtb (or (:modify-transition-by transition-parameters) "None")
        tm  (or (:transition-matrix file-inputs) "None")
        mtf (or (:modify-transitions-from run-parameters) "None")
        ftf (or (:filter-transitions-from transition-parameters) "None")
        sn  (or (:splice-ncy transition-parameters) "None")
        report-fn (fn [[k v]] (report/info k (report/bold v)))]
    (when (output-parameters :run-report-header)
      (report/reset-send-report)
      (dorun (map report-fn [["Input Data: " (str/join ", " (vals file-inputs))]
                             ["Number of iterations: " (:simulations run-parameters)]
                             ["Output charts produced: " (:run-reports output-parameters)]
                             ["Modifying: " (if (nil? wt) "None" (str/join ", " wt))]
                             ["Transitions modifier: " mtb]
                             ["Transitions file: " tm]
                             ["Modify transitions from: " mtf]
                             ["Filter transitions from: " ftf]
                             ["Splice NCY: " (str sn "\n")]])))))

(defn run-send
  "Function expects a map of the following keys, if a key is not present
  it will use defaults (as will no map at all):
  :file-inputs - map of input files
  :schema-inputs - map of schemas
  :transition-parameters - map of parameters for the preparation phase
  :run-parameters - map of parameters for the run phase
  :output-parameters - map of parameters for the output phase
  "
  ([] (run-send {}))
  ([{fi :file-inputs 
     si :schema-inputs
     tp :transition-parameters
     rp :run-parameters
     op :output-parameters
     :or {fi file-inputs
          si schema-inputs
          tp default-transition-parameters
          rp default-run-parameters
          op default-output-parameters}}]
   (generate-report-header fi si tp rp op)
   ;The main workflow of the model
   (-> (build-input-datasets fi si)
       (send/prepare-send-inputs tp)
       (send/run-send-model rp)
       (send/output-send-results op))))

(deftest send-test
  (is (= #{:total-in-send-by-ay :total-in-send-by-ay-group 
           :total-in-send :total-in-send-by-need :total-in-send-by-setting}
         (-> (run-send)
             (first)
             (keys)
             (set)))))
