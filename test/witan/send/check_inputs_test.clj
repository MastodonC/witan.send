(ns witan.send.check-inputs-test
  (:require [clojure.test :refer :all]
            [witan.send.check-inputs :refer :all]))


(def test-setting-costs [{:need :X, :setting :Y, :cost 999.9}])
(def test-valid-setting-ay [{:setting :X, :min-academic-year -4 :max-academic-year 20}])


(deftest cost-for-state?-tests
  (is (= true (cost-for-state? {:need :X, :setting :Y} test-setting-costs)))
  (is (= false (cost-for-state? {:need :X, :setting :Z} test-setting-costs))))


(deftest valid-ay-for-state?-tests
  (is (= false (valid-ay-for-state? {:setting :X :academic-year -5} test-valid-setting-ay)))
  (is (= true (valid-ay-for-state? {:setting :X :academic-year -4} test-valid-setting-ay)))
  (is (= true (valid-ay-for-state? {:setting :X :academic-year 10} test-valid-setting-ay)))
  (is (= true (valid-ay-for-state? {:setting :X :academic-year 20} test-valid-setting-ay)))
  (is (= false (valid-ay-for-state? {:setting :X :academic-year 21} test-valid-setting-ay))))

