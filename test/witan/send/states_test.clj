(ns witan.send.states-test
  (:require [clojure.core.matrix.dataset :as ds]
            [clojure.test :refer [deftest testing is]]
            [witan.send.model.input :as i]
            [witan.send.schemas :as sc]
            [witan.send.states :refer :all]))

(defn test-inputs []
  {:valid-setting-academic-years ["data/demo/data/valid-setting-academic-years.csv" sc/ValidSettingAcademicYears]})

(deftest calculate-valid-year-settings-from-setting-academic-years-test
  (let [[path schema] (:valid-setting-academic-years (test-inputs))
        data (ds/row-maps (i/csv-to-dataset path schema))]
    (testing ""
      (is (calculate-valid-year-settings-from-setting-academic-years data)))))

(deftest can-move-test
  (let [[path schema] (:valid-setting-academic-years (test-inputs))
        data (ds/row-maps (i/csv-to-dataset path schema))
        valid-year-settings (calculate-valid-year-settings-from-setting-academic-years data)]
    (testing ""
      (is (true? (can-move? valid-year-settings 0 :CL-MSS))))))
