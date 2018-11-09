(ns witan.send.model.input-test
  (:require [clojure.test :refer [deftest is testing]]
            [witan.send.model.input :refer [load-csv]]))

(deftest load-csv-test
  (testing "Empty lines in input csv are removed"
    (is (= (load-csv "data/demo/data/test.csv") {:column-names ["need" "setting" "cost"], :columns [["CI" "EO" "4577.3"]]}))))
