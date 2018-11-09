(ns witan.send.utils-test
  (:require [clojure.test :refer [deftest is testing]]
            [witan.send.utils :refer [confidence-intervals]]))

(deftest confidence-intervals-test
  (testing "95% confidence intervals correctly calculated as per https://www.dummies.com/education/math/statistics/how-to-calculate-a-confidence-interval-for-a-population-mean-when-you-know-its-standard-deviation/"
    (is (= 7.0492 (:low-ci (confidence-intervals {:mean 7.5 :std-dev 2.3} 100))))
    (is (= 7.9508 (:high-ci (confidence-intervals {:mean 7.5 :std-dev 2.3} 100))))))
