(ns witan.send.acceptance.workspace-test
  (:require [clojure.test :refer :all]
            [witan.send.send :refer :all]
            [schema.core :as s]
            [witan.send.schemas :as sc]
            [witan.send.model :as m]
            [witan.workspace-api.protocols :as p]
            [witan.workspace-executor.core :as wex]
            [witan.send.test-utils :as tu]))

(def test-inputs
  {:historic-0-25-population ["data/demo/Population_0_25.csv" sc/PopulationSYA]
   :historic-send-population ["data/demo/send_population.csv" sc/SENDSchemaGrouped]
   :population-projection ["data/demo/Population_projection.csv" sc/PopulationSYA]
   :cost-profile ["data/demo/cost_profile.csv" sc/CostProfile]
   :transition-matrix ["data/demo/transition_matrix.csv" sc/DataForMatrix]})

(defn add-input-params
  [input]
  (assoc-in input [:witan/params :fn] (partial tu/read-inputs test-inputs input)))

(deftest send-workspace-test
  (testing "The default model is run on the workspace and returns the outputs expected"
    (let [fixed-catalog (mapv #(if (= (:witan/type %) :input) (add-input-params %) %)
                              (:catalog m/send-model))
          workspace     {:workflow  (:workflow m/send-model)
                         :catalog   fixed-catalog
                         :contracts (p/available-fns (m/model-library))}
          workspace'    (s/with-fn-validation (wex/build! workspace))
          result        (apply merge (wex/run!! workspace' {}))]
      (is result)
      (is (= #{:send-projection :send-costs} (set (keys result)))))))
