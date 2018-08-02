(ns witan.send.multi-config
  (:require [witan.send.schemas :as sc]
            [witan.send.main :as main]
            [witan.send.send :as send]
            [clojure.math.combinatorics :as combo]))


(def default-config
  {:file-inputs {:transition-matrix "data/transitions.csv"
                 :population "data/population.csv"
                 :setting-cost "data/need-setting-costs.csv"
                 :valid-setting-academic-years "data/valid-setting-academic-years.csv"}
   :transition-parameters {:filter-transitions-from nil
                           :which-transitions? nil
                           :splice-ncy nil
                           :modify-transition-by 1}
   :run-parameters {:modify-transitions-from nil
                    :random-seed 50
                    :simulations 1000
                    :seed-year 2017}
   :output-parameters {:run-report-header true
                       :run-reports true
                       :run-charts true
                       :output-dir "results"}})


(def example-params-inputs
  "Inputs for run-multi-configs should take this form"
  [[[:run-parameters :random-seed] '[1 2 3]]
   [[:run-parameters :simulations] '[10 20 30]]])


(defn generate-param-options [param-name-vec values-vec]
  "Takes two vecs and returns form required for combo/cartesian-product"
  (mapv #(conj param-name-vec %) values-vec))


(defn name-output-dir [combo]
  "Adds unique output-dir val to combination map for given combination of parameters"
  (conj [:output-parameters :output-dir]
        (-> (apply str (doall (map #(take-last 2 %) combo)))
            (clojure.string/replace #"[(): ]" ""))))


(defn generate-params [inputs]
  "Generates combinations of input parameters based on input vectors"
  (let [param-value-pairs (map #(generate-param-options (first %) (last %)) inputs)
        combos (apply combo/cartesian-product param-value-pairs)]
    (map #(conj % (name-output-dir %)) combos)))


(defn update-nested-map [acc n]
  "Helper fn to updates nested hashmap given vector output of generate-params"
  (assoc-in acc (subvec n 0 (dec (count n))) (last n)))


(defn run-multi-configs [inputs project-dir]
  "Main function to run multi configs. Edit default-config to specify consistent config settings.

  Args:
    inputs: nested vectors with [:parameter val] structure, see example-params-inputs
    project-dir: full path to project (dir which would normally contain config.edn)

  Returns nil. Model results for each run are output to unique folder in project-dir."

  (let [configs (->> (generate-params inputs)
                     (map #(reduce update-nested-map default-config %))
                     (map #(merge-with merge %
                      {:schema-inputs {:settings-to-change sc/SettingsToChange
                                       :transition-matrix sc/TransitionCounts
                                       :population sc/PopulationDataset
                                       :setting-cost sc/NeedSettingCost
                                       :valid-setting-academic-years sc/ValidSettingAcademicYears}}
                      {:project-dir project-dir}
                      {:output-parameters {:project-dir project-dir}})))]
    (map #(do (-> (main/run-send %)
                  (send/output-send-results (:output-parameters %)))
              (main/save-runtime-config %)) configs)))

