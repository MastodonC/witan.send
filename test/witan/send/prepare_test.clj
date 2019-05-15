(ns witan.send.prepare-test
  (:require [clojure.test :refer [deftest is testing]]
            [witan.send.model.prepare :as p]))


(deftest test-predicates
  (let [data '({:calendar-year 2018, :setting-1 :MU, :need-1 :ASD, :setting-2 :MSSE, :need-2 :ASD, :academic-year-2 12, :academic-year-1 10}
               {:calendar-year 2017, :setting-1 :MU, :need-1 :ASD, :setting-2 :MU, :need-2 :ASD, :academic-year-2 10, :academic-year-1 9}
               {:calendar-year 2018, :setting-1 :NONSEND, :need-1 :NONSEND, :setting-2 :MU, :need-2 :ASD, :academic-year-2 10, :academic-year-1 9})
        predicate-maps [{:setting-2 :MU, :academic-year-2 10}
                        {:setting-2 :MSSE, :academic-year-2 12}
                        {:setting-2 :MU, :academic-year-2 10 :calendar-year 2017}]]
    (testing "returns the same number of booleans as there are predicates within a map"
      (is (= '(false false)
             (p/test-predicates data (nth predicate-maps 0))))
      (is (= '(false false false)
             (p/test-predicates data (nth predicate-maps 2)))))
    (testing "filtering works as expected"
      (is (= (filter (fn [t] (every? identity (p/test-predicates t (nth predicate-maps 0)))) data)
             '({:calendar-year 2017, :setting-1 :MU, :need-1 :ASD, :setting-2 :MU, :need-2 :ASD, :academic-year-2 10, :academic-year-1 9}
               {:calendar-year 2018, :setting-1 :NONSEND, :need-1 :NONSEND, :setting-2 :MU, :need-2 :ASD, :academic-year-2 10, :academic-year-1 9})))
      (is (= (filter (fn [t] (every? identity (p/test-predicates t (nth predicate-maps 1)))) data)
             '({:calendar-year 2018, :setting-1 :MU, :need-1 :ASD, :setting-2 :MSSE, :need-2 :ASD, :academic-year-2 12, :academic-year-1 10})))
      (is (= (filter (fn [t] (every? identity (p/test-predicates t (nth predicate-maps 2)))) data)
             '({:calendar-year 2017, :setting-1 :MU, :need-1 :ASD, :setting-2 :MU, :need-2 :ASD, :academic-year-2 10, :academic-year-1 9}))))))
