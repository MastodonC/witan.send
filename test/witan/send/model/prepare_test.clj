(ns witan.send.model.prepare-test
  (:require [clojure.test :refer :all]
            [witan.send.model.prepare :refer :all]))

(deftest test-predicates-test
  (testing "filter by single key-value pair"
    (is (every? true? (test-predicates {:a 1 :b 2 :c 3} {:a 1})))
    (is (every? false? (test-predicates {:a 1 :b 2 :c 3} {:a 2}))))
  (testing "filter by multiple key-value pairs"
    (is (every? true? (test-predicates {:a 1 :b 2 :c 3} {:a 1 :b 2})))
    (is (= '(true false) (test-predicates {:a 1 :b 2 :c 3} {:a 1 :b 1}))))
  (testing "filter by multiple values for single key"
    (is (every? true? (test-predicates {:a 1 :b 2 :c 3} {:a [1 2]})))
    (is (every? true? (test-predicates {:a 2 :b 2 :c 3} {:a [1 2]}))))
  (testing "filter by multiple values for single key and by multiple keys"
    (is (every? true? (test-predicates {:a 1 :b 2 :c 3} {:a [1 2] :b 2}))))
  (testing "fitler by single value even when in multiple value format"
    (is (every? true? (test-predicates {:a 1 :b 2 :c 3} {:a [1]})))
    (is (every? false? (test-predicates {:a 1 :b 2 :c 3} {:a [2]})))))
