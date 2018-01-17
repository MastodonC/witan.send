(ns witan.send.acceptance.workspace-test

  (:require [clojure.core.matrix.dataset :as ds]
            [clojure.test :refer :all]
            [schema.core :as s]
            [witan.send.model :as m]
            [witan.send.schemas :as sc]
            [witan.send.states :as states]
            [witan.send.test-utils :as tu]
            [witan.workspace-api.protocols :as p]
            [witan.workspace-executor.core :as wex]))

(def inputs-path "data/Tower-Hamlets-input-2018-01-15/")

(def test-inputs
  {:initial-send-population [(str inputs-path "send-population.csv") sc/SENDPopulation]
   :transition-matrix [(str inputs-path "transitions.csv") sc/TransitionCounts]
   :population [(str inputs-path "population.csv") sc/PopulationDataset]
   :setting-cost [(str inputs-path "need-setting-costs.csv") sc/NeedSettingCost]
   :valid-setting-academic-years [(str inputs-path "valid-setting-academic-years.csv") sc/ValidSettingAcademicYears]})

(defn add-input-params
  [input]
  (assoc-in input [:witan/params :fn] (fn [a b]
                                        (tu/read-inputs test-inputs input a b))))

(witan.workspace-api/set-api-logging! println)

(deftest send-workspace-test
  (testing "The default model is run on the workspace and returns the outputs expected"
    (let [fixed-catalog (->> (:catalog m/send-model)
                             (mapv #(if (= (:witan/type %) :input)
                                      (add-input-params %)
                                      (assoc-in % [:witan/params :simulations] 10)))
                             (map #(assoc-in % [:witan/params :output] false)))
          workspace     {:workflow  (:workflow m/send-model)
                         :catalog   fixed-catalog
                         :contracts (p/available-fns (m/model-library))}
          workspace'    (s/with-fn-validation (wex/build! workspace))
          result        (apply merge (wex/run!! workspace' {}))]
      (is (= #{:total-in-send-by-ay :total-in-send-by-ay-group :by-state :total-cost :total-in-send :total-in-send-by-need :total-in-send-by-setting}
             (-> result first keys set))))))

(defn run-model []
  (let [fixed-catalog (mapv #(if (= (:witan/type %) :input)
                               (add-input-params %)
                               (assoc-in % [:witan/params :simulations] 10))
                            (:catalog m/send-model))
        workspace     {:workflow  (:workflow m/send-model)
                       :catalog   fixed-catalog
                       :contracts (p/available-fns (m/model-library))}
        workspace'    (s/with-fn-validation (wex/build! workspace))
        result        (apply merge (wex/run!! workspace' {}))]
    nil))

