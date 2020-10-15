(ns witan.send.multi-config
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as string]
            [witan.send.main :as main]))

(def default-config
  "Static settings for all runs with different configs (settings in inputs will be overwritten)"
  {:project-dir "data/demo/"
   :file-inputs {:transitions "data/transitions.csv"
                 :population "data/population.csv"
                 :costs "data/costs.csv"
                 :valid-states "data/valid-states.csv"}
   :transition-parameters {:filter-transitions-from nil
                           :modify-transitions-date-range nil
                           :transitions-to-change nil
                           }
   :projection-parameters {:random-seed 50
                           :simulations 10
                           :seed-year 2017}
   :output-parameters {:run-outputs true
                       :run-charts true
                       :output-dir "results"}})

(def example-params-inputs
  "Inputs for run-multi-configs should take this nested vec form, with a vec containing the path to
  the config parameter followed by a vec containing the possible values to take."
  [[[:projection-parameters :random-seed] [1 42]]
   [[:projection-parameters :simulations] [10 20 30]]])

(defn generate-param-options
  "Takes two vecs and returns form required for combo/cartesian-product"
  [param-name-vec values-vec]
  (mapv #(conj param-name-vec %) values-vec))

(defn name-output-dir
  "Adds unique output-dir val to combination map for given combination of parameters"
  [combo]
  (conj [:output-parameters :output-dir]
        (string/replace
         (string/join (doall (map #(take-last 2 %) combo)))
         #"[():,{} ]"
         "")))

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

(defn generate-configs [inputs config]
  (->> (generate-params inputs)
       (map #(reduce update-nested-map config %))
       (map #(merge-with merge %
                         main/default-schemas
                         {:output-parameters {:project-dir (:project-dir config)}}))))

(defn run-multi-configs
  "Main function to run multi configs. Edit default-config to specify consistent config settings.

  Args:
    inputs: nested vectors with [:parameter val] structure, see example-params-inputs
    project-dir: full path to project (dir which contains file-inputs in default-config)

  Returns nil. Model results for each run are output to unique folder in project-dir."
  ([inputs config]
   (map main/run-recorded-send (generate-configs inputs config)))
  ([inputs]
   (run-multi-configs inputs default-config)))
