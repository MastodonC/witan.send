(ns witan.send.states-test
  (:require [witan.send.states :refer :all]
            [clojure.test :refer :all]
            [witan.send.test-utils :as tu]
            [witan.send.schemas :as sc]
            [clojure.core.matrix.dataset :as ds]))

#_(def valid-states
    {1 #{:foo-foo :bar-foo :baz-bar} 2 #{:foo-foo :bar-bar} 3 #{:foo-bar}})

#_(deftest can-move-test
    (testing ""
      (is (true? (can-move? valid-states 2 :foo-foo)))))

(defn test-inputs []
  {:valid-setting-academic-years ["data/Tower-Hamlets-input-2018-01-15/valid-setting-academic-years.csv" sc/ValidSettingAcademicYears]})

(deftest calculate-valid-year-settings-from-setting-academic-years-test
  (let [[path schema] (:valid-setting-academic-years (test-inputs))
        data (ds/row-maps (tu/csv-to-dataset path schema))]
    (testing ""
      (is (calculate-valid-year-settings-from-setting-academic-years data)))))

(deftest can-move-test
  (let [[path schema] (:valid-setting-academic-years (test-inputs))
        data (ds/row-maps (tu/csv-to-dataset path schema))
        valid-states (calculate-valid-states-from-setting-academic-years data)]
    (testing ""
      (is (true? (can-move? valid-states 0 :CL-MSS))))))
