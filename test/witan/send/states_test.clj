(ns witan.send.states-test
  (:require [witan.send.states :refer :all]
            [clojure.test :refer :all]
            [witan.send.schemas :as sc]
            [clojure.core.matrix.dataset :as ds]
            [witan.send.utils :as u]))

(defn test-inputs []
  {:valid-setting-academic-years ["data/demo/data/valid-setting-academic-years.csv" sc/ValidSettingAcademicYears]})

(deftest calculate-valid-year-settings-from-setting-academic-years-test
  (let [[path schema] (:valid-setting-academic-years (test-inputs))
        data (ds/row-maps (u/csv-to-dataset path schema))]
    (testing ""
      (is (calculate-valid-year-settings-from-setting-academic-years data)))))

(deftest can-move-test
  (let [[path schema] (:valid-setting-academic-years (test-inputs))
        data (ds/row-maps (u/csv-to-dataset path schema))
        valid-year-settings (calculate-valid-year-settings-from-setting-academic-years data)]
    (testing ""
      (is (true? (can-move? valid-year-settings 0 :CL-MSS))))))
