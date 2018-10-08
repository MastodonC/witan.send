(ns witan.send.utils-test
  (:require [clojure.test :refer :all]
            [witan.send.utils :refer :all]
            [clojure.core.matrix.dataset :as ds]
            [schema.core :as s]
            [witan.send.schemas :as sc]
            [witan.send.send-test :as st]))

(deftest load-csv-test
  (testing "Empty lines in input csv are removed"
    (is (= (load-csv "data/demo/data/test.csv") {:column-names ["need" "setting" "cost"], :columns [["CI" "EO" "4577.3"]]}))))
