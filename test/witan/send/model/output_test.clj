(ns witan.send.model.output-test
  (:require [clojure.test :refer [deftest is testing]]
            [witan.send.constants :as c]
            [witan.send.model.input.population :as population]
            [witan.send.model.input.transitions :as transitions]
            [witan.send.model.output :as so]
            [witan.send.params :as p]))

(deftest joiner-rate-test
  (let [joiner-count (p/calculate-joiners-per-calendar-year (transitions/csv->transitions "data/demo/data/transitions.csv"))
        population-count (p/calculate-joiners-per-calendar-year (population/csv->population "data/demo/data/population.csv"))
        ages (-> population-count first val keys)
        years [2013 2014 2015 2016]
        result (so/joiner-rate joiner-count population-count ages years)]
    (testing "output is not empty"
      (is (not= empty? result)))
    (testing "all ages have rates calculated"
      (is (= (keys result) ages)))))

(deftest mover-rate-test
  (let [mover-count (->> (transitions/csv->transitions "data/demo/data/transitions.csv")
                         (remove (fn [{:keys [setting-1 setting-2]}]
                                   (or (= setting-1 c/non-send)
                                       (= setting-2 c/non-send)))))
        result (so/mover-rate mover-count)]
    (testing "output is not empty"
      (is (not= empty? result)))))

(deftest leaver-rate-test
  (let [leaver-count (->> (transitions/csv->transitions "data/demo/data/transitions.csv")
                          (remove (fn [{:keys [setting-1]}] (= setting-1 c/non-send))))
        result (so/leaver-rate leaver-count)]
    (testing "output is not empty"
      (is (not= empty? result)))))

(deftest confidence-bounds-test
  (let [leaver-rates (->> (transitions/csv->transitions "data/demo/data/transitions.csv")
                          (remove (fn [{:keys [setting-1]}] (= setting-1 c/non-send))) so/leaver-rate)
        result (so/confidence-bounds leaver-rates 2014)]
    (testing "output is not empty"
      (is (not= empty? result) ))
    (testing "all intervals are between 0 and 1"
      (is (every? #(and (<= 0 %) (>= 1 %)) (map #(nth % 1) result)))
      (is (every? #(and (<= 0 %) (>= 1 %)) (map #(nth % 2) result))))
    (testing "all vals are numbers"
      (is (every? #(number? %) (reduce concat result))))))

(deftest transition-present?-test
  (testing "transition state is present in coll"
    (is (so/transition-present? [11 :CI-MSSOB :CI-ISSR] '([6 :OTH-MSSSH :OTH-MSSSH] [6 :SP-MU :SP-MU] [6 :UKN-MMSIB :UKN-MMSIB] [11 :CI-MSSOB :CI-ISSR] [6 :SP-IMS :SP-IMS] [6 :SEMH-MSSCT :SEMH-NMSS] [8 :CI-OOE :CI-ISS] [8 :CI-MMSOB :CI-MMSOB] [6 :SP-MU :SP-NMSS] [6 :CI-MSSSH :CI-MSSSH] [8 :SEMH-MMSIB :NONSEND] [13 :SP-MSSOB :NONSEND] [4 :CL-MSSSH :NONSEND])))))
