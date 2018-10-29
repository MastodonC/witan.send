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

(deftest confidence-intervals-test
  (testing "95% confidence intervals correctly calculated as per https://www.dummies.com/education/math/statistics/how-to-calculate-a-confidence-interval-for-a-population-mean-when-you-know-its-standard-deviation/"
    (is (= 7.0492 (:low-ci (confidence-intervals {:mean 7.5 :std-dev 2.3} 100))))
    (is (= 7.9508 (:high-ci (confidence-intervals {:mean 7.5 :std-dev 2.3} 100))))))
