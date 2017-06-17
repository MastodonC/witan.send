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
  {:initial-population ["data/camden/initial-population.csv" sc/PopulationSYA]
   :initial-send-population ["data/camden/send-population.csv" sc/SENDPopulation]
   :transition-matrix ["data/camden/transitions3.csv" sc/TransitionCounts]
   :projected-population ["data/camden/projected-population.csv" sc/PopulationSYA]})


(defn add-input-params
  [input]
  (assoc-in input [:witan/params :fn] (fn [a b]
                                        (tu/read-inputs test-inputs input a b))))

(witan.workspace-api/set-api-logging! println)

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

(defn run! []
  (let [fixed-catalog (mapv #(if (= (:witan/type %) :input) (add-input-params %) %)
                            (:catalog m/send-model))
        workspace     {:workflow  (:workflow m/send-model)
                       :catalog   fixed-catalog
                       :contracts (p/available-fns (m/model-library))}
        workspace'    (s/with-fn-validation (wex/build! workspace))
        result        (apply merge (wex/run!! workspace' {}))]
    result))
