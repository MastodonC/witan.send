(ns witan.send.acceptance.workspace-test
  (:require [clojure.test :refer :all]
            [witan.send.send :refer :all]
            [schema.core :as s]
            [witan.send.schemas :as sc]
            [witan.send.model :as m]
            [witan.workspace-api.protocols :as p]
            [witan.workspace-executor.core :as wex]))

(def test-inputs
  {:historic-0-25-population {}
   :historic-send-population {}
   :population-projection {}
   :cost-profile {}
   :transitions-default {}
   :transitions-reduced-secondary-joiners {}})

(defn read-inputs [input _ schema]
  (get test-inputs (:witan/name input)))

(defn add-input-params
  [input]
  (assoc-in input [:witan/params :fn] (partial read-inputs input)))

(deftest send-workspace-test
  (testing "The model is run on the workspace and returns the outputs expected"
    (let [fixed-catalog (mapv #(if (= (:witan/type %) :input) (add-input-params %) %)
                              (:catalog m/send-model))
          workspace     {:workflow  (:workflow m/send-model)
                         :catalog   fixed-catalog
                         :contracts (p/available-fns (m/model-library))}
          workspace'    (s/with-fn-validation (wex/build! workspace))
          result        (apply merge (wex/run!! workspace' {}))]
      (is result)
      (println result)
      (is ({:send-projection {}  :send-costs {}} result)))))
