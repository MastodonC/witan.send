(ns witan.send.states-test
  (:require [clojure.core.matrix.dataset :as ds]
            [clojure.test :refer [deftest testing is]]
            [witan.send.model.input :as i]
            [witan.send.schemas :as sc]
            [witan.send.states :refer :all]))

(defn test-inputs []
  {:valid-states ["data/demo/data/valid-states.csv" sc/ValidSettingAcademicYears]})

(deftest calculate-valid-year-settings-from-setting-academic-years-test
  (let [[path schema] (:valid-states (test-inputs))
        data (ds/row-maps (i/csv-to-dataset path schema))]
    (testing ""
      (is (calculate-valid-year-settings-from-setting-academic-years data)))))

(deftest can-move-test
  (let [[path schema] (:valid-states (test-inputs))
        data (ds/row-maps (i/csv-to-dataset path schema))
        valid-year-settings (calculate-valid-year-settings-from-setting-academic-years data)
        valid-transitions (calculate-valid-mover-transitions data)]
    (testing ""
      (is (true? (can-move? valid-year-settings 0 :L-A valid-transitions))))))
