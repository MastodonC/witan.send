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

(def transitions [{:academic-year-1 1 :academic-year-2 2 :setting-1 :NONSEND :setting-2 :ABC :need-1 :NONSEND :need-2 :SEMH}
                  {:academic-year-1 2 :academic-year-2 3 :setting-1 :DEF :setting-2 :DEF :need-1 :SEMH :need-2 :SEMH}
                  {:academic-year-1 4 :academic-year-2 3 :setting-1 :XYZ :setting-2 :NONSEND :need-1 :SEMH :need-2 :NONSEND}])

(deftest check-all-ages-present-test
  (testing "No transitions from AY3 or to AY4"
    (is (= (check-all-ages-present transitions) '("There are no transitions from AY 3" "There are no transitions to AY 4")))))

(deftest check-joiner-leaver-gaps-test
  (testing "That there are no leavers from AY5, 3 and 2 and no joiners from AY1, 3 and 4"
    (is (= (check-joiner-leaver-gaps transitions)
           '("There are no leavers from AY 3" "There are no leavers from AY 2" "There are no joiners to AY 1"
             "There are no joiners to AY 3")))))

(deftest check-ages-go-up-one-year-test
  (testing "Aging from AY4 to AY3 is not reported"
    (is (= (check-ages-go-up-one-year transitions)
           '("Academic years 1 and 2 are not incremental for {:academic-year-1 4, :academic-year-2 3, :setting-1 :XYZ, :setting-2 :NONSEND, :need-1 :SEMH, :need-2 :NONSEND}")))))

(deftest check-nonsend-states-valid-test
  (let [wrong-transition {:academic-year-1 4 :academic-year-2 3 :setting-1 :ABC :setting-2 :NONSEND :need-1 :SEMH :need-2 :SEMH}]
    (testing "Non-SEND states are correctly coded"
      (is (= nil (check-nonsend-states-valid transitions)))
      (is (= "There are 1 occurrences where Non-SEND states are miscoded"
             (check-nonsend-states-valid (merge transitions wrong-transition)))))))
