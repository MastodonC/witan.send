(ns witan.send.multi-config
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as string]
            [witan.send.main :as main]))

(def default-config
  "Static settings for all runs with different configs (settings in inputs will be overwritten)"
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
                       :run-outputs true
                       :run-charts true
                       :output-dir "results"}})

(def example-params-inputs
  "Inputs for run-multi-configs should take this nested vec form, with a vec containing the path to
  the config parameter followed by a vec containing the possible values to take."
  [[[:run-parameters :random-seed] [1 42]]
   [[:run-parameters :simulations] [10 20 30]]])

(defn generate-param-options
  "Takes two vecs and returns form required for combo/cartesian-product"
  [param-name-vec values-vec]
  (mapv #(conj param-name-vec %) values-vec))

(defn name-output-dir
  "Adds unique output-dir val to combination map for given combination of parameters"
  [combo]
  (conj [:output-parameters :output-dir]
        (-> (apply str (doall (map #(take-last 2 %) combo)))
            (string/replace #"[(): ]" ""))))

(defn generate-params
  "Generates combinations of input parameters based on input vectors"
  [inputs]
  (let [param-value-pairs (map #(generate-param-options (first %) (last %)) inputs)
        combos (apply combo/cartesian-product param-value-pairs)]
    (map #(conj % (name-output-dir %)) combos)))

(defn update-nested-map
  "Helper fn to update nested hashmap given vector output of generate-params"
  [acc n]
  (assoc-in acc (subvec n 0 (dec (count n))) (last n)))

(defn run-multi-configs
  "Main function to run multi configs. Edit default-config to specify consistent config settings.

  Args:
    inputs: nested vectors with [:parameter val] structure, see example-params-inputs
    project-dir: full path to project (dir which contains file-inputs in default-config)

  Returns nil. Model results for each run are output to unique folder in project-dir."
  [inputs project-dir]
  (let [configs (->> (generate-params inputs)
                     (map #(reduce update-nested-map default-config %))
                     (map #(merge-with merge %
                                       main/default-schemas
                                       {:project-dir project-dir}
                                       {:output-parameters {:project-dir project-dir}})))]
    (map main/run-recorded-send configs)))
