(ns witan.send.main
  (:require [schema.core :as s]
            [witan.send.model :as m]
            [witan.workspace-api.protocols :as p]
            [witan.workspace-executor.core :as wex]
            [witan.send.acceptance.workspace-test :as w]
            [witan.send.send :as send])
  (:gen-class))

(defn -main []
  (let [fixed-catalog (mapv #(if (= (:witan/type %) :input) (w/add-input-params %) %)
                            (:catalog m/send-model))
        workspace     {:workflow  (:workflow m/send-model)
                       :catalog   fixed-catalog
                       :contracts (p/available-fns (m/model-library))}
        workspace'    (s/with-fn-validation (wex/build! workspace))
        result        (apply merge (wex/run!! workspace' {}))]
    result))
