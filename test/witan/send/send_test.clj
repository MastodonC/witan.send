(ns witan.send.send-test
  (:require [clojure.test :refer [deftest is testing]]
            [witan.send.constants :as c]
            [witan.send.model.input.population :as population]
            [witan.send.model.input.transitions :as transitions]
            [witan.send.model.output :as so]
            [witan.send.model.prepare :as mp]
            [witan.send.params :as p]))

;; Tests

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

(deftest modify-transitions-test
  (let [transitions {[2014 1 :NONSEND :SEMH-MMSIB] 2, [2014 1 :NONSEND :SP-MMSIB] 3, [2014 1 :SP-MU :SP-MU] 2}
        state-change1 [[2014 1 :SP-MU :SP-MU] [2014 1 :NONSEND :SEMH-MMSIB]]
        state-change2 [[2014 1 :NONSEND :SP-MMSIB] [2014 1 :NONSEND :SEMH-MMSIB]]]
    (testing "modifies transitions"
      (is (not= transitions (mp/modify-transitions transitions state-change1 * 0.5))))
    (testing "state [2014 8 :SP-MU :SP-MU] is divided by two"
      (is (= 1 (get (mp/modify-transitions transitions state-change1 * 0.5) [2014 1 :SP-MU :SP-MU]))))
    (testing "state [2014 0 :NONSEND :SP-MMSIB] takes the joiners of state [2014 8 :SP-MU :SP-MU]"
      (is (= 3 (get (mp/modify-transitions transitions state-change1 * 0.5) [2014 1 :NONSEND :SEMH-MMSIB]))))
    (testing "odd values are rounded and exchanged correctly"
      (is (= 3 (get (mp/modify-transitions transitions state-change2 * 0.5) [2014 1 :NONSEND :SEMH-MMSIB])))
      (is (= 2 (get (mp/modify-transitions transitions state-change2 * 0.5) [2014 1 :NONSEND :SP-MMSIB]))))))

(deftest remove-transitions-xf-test
  "This test needs splitting to simpler cases but for speed captures most things.
  There's some discussion to be had over how we should treat setting-2 as it moves into calendar year 2017
  Note: the test discovered the keyname is required not a string :-("
  (let [transitions (list {:calendar-year 2015, :setting-1 :NONSEND, :need-1 :NA, :academic-year-1 11, :setting-2 :MU, :need-2 :NA, :academic-year-2 12}
                          {:calendar-year 2015, :setting-1 :NONSEND, :need-1 :NA, :academic-year-1 11, :setting-2 :AK, :need-2 :NA, :academic-year-2 12}
                          {:calendar-year 2015, :setting-1 :NONSEND, :need-1 :NA, :academic-year-1 9, :setting-2 :AK, :need-2 :NA, :academic-year-2 10}
                          {:calendar-year 2016, :setting-1 :NONSEND, :need-1 :NA, :academic-year-1 12, :setting-2 :MMSIB, :need-2 :NA, :academic-year-2 13}
                          {:calendar-year 2016, :setting-1 :MU, :need-1 :NA, :academic-year-1 10, :setting-2 :MU, :need-2 :NA, :academic-year-2 11}
                          {:calendar-year 2016, :setting-1 :AK, :need-1 :NA, :academic-year-1 10, :setting-2 :MU, :need-2 :NA, :academic-year-2 11}
                          {:calendar-year 2017, :setting-1 :NONSEND, :need-1 :NA, :academic-year-1 11, :setting-2 :MMSIB, :need-2 :NA, :academic-year-2 12}
                          {:calendar-year 2017, :setting-1 :NONSEND, :need-1 :NA, :academic-year-1 12, :setting-2 :MMSIB, :need-2 :NA, :academic-year-2 13}
                          {:calendar-year 2017, :setting-1 :MU, :need-1 :NA, :academic-year-1 11, :setting-2 :MU, :need-2 :NA, :academic-year-2 12})
        filter-transitions-from-a [:calendar-academic {:< 2017 :>= 12}]
        filter-transitions-from-b [:calendar-setting {:< 2017 := :MU}]
        filter-transitions-from-c {:calendar-academic {:< 2017 :>= 12} :calendar-setting {:< 2017 := :MU}}]
    (testing "filters transitions from before 2017 and academic year 12+"
      (is (= (sequence (mp/remove-transitions-xf filter-transitions-from-a) transitions)
             (list
              {:calendar-year 2015, :setting-1 :NONSEND, :need-1 :NA, :academic-year-1 9, :setting-2 :AK, :need-2 :NA, :academic-year-2 10}
              {:calendar-year 2016, :setting-1 :MU, :need-1 :NA, :academic-year-1 10, :setting-2 :MU, :need-2 :NA, :academic-year-2 11}
              {:calendar-year 2016, :setting-1 :AK, :need-1 :NA, :academic-year-1 10, :setting-2 :MU, :need-2 :NA, :academic-year-2 11}
              {:calendar-year 2017, :setting-1 :NONSEND, :need-1 :NA, :academic-year-1 11, :setting-2 :MMSIB, :need-2 :NA, :academic-year-2 12}
              {:calendar-year 2017, :setting-1 :NONSEND, :need-1 :NA, :academic-year-1 12, :setting-2 :MMSIB, :need-2 :NA, :academic-year-2 13}
              {:calendar-year 2017, :setting-1 :MU, :need-1 :NA, :academic-year-1 11, :setting-2 :MU, :need-2 :NA, :academic-year-2 12}))))
    (testing "filters transitions from before 2017 in the setting MU"
      (is (= (sequence (mp/remove-transitions-xf filter-transitions-from-b) transitions)
             (list
              {:calendar-year 2015, :setting-1 :NONSEND, :need-1 :NA, :academic-year-1 11, :setting-2 :AK, :need-2 :NA, :academic-year-2 12}
              {:calendar-year 2015, :setting-1 :NONSEND, :need-1 :NA, :academic-year-1 9, :setting-2 :AK, :need-2 :NA, :academic-year-2 10}
              {:calendar-year 2016, :setting-1 :NONSEND, :need-1 :NA, :academic-year-1 12, :setting-2 :MMSIB, :need-2 :NA, :academic-year-2 13}
              {:calendar-year 2016, :setting-1 :AK, :need-1 :NA, :academic-year-1 10, :setting-2 :MU, :need-2 :NA, :academic-year-2 11}
              {:calendar-year 2017, :setting-1 :NONSEND, :need-1 :NA, :academic-year-1 11, :setting-2 :MMSIB, :need-2 :NA, :academic-year-2 12}
              {:calendar-year 2017, :setting-1 :NONSEND, :need-1 :NA, :academic-year-1 12, :setting-2 :MMSIB, :need-2 :NA, :academic-year-2 13}
              {:calendar-year 2017, :setting-1 :MU, :need-1 :NA, :academic-year-1 11, :setting-2 :MU, :need-2 :NA, :academic-year-2 12}))))
    (testing "filters transitions from before 2017 and academic year 12+ and filters transitions from before 2017 in the setting MU"
      (is (= (->> transitions
                  (sequence (mp/remove-transitions-xf (first filter-transitions-from-c)))
                  (sequence (mp/remove-transitions-xf (second filter-transitions-from-c))))
             (list
              {:calendar-year 2015, :setting-1 :NONSEND, :need-1 :NA, :academic-year-1 9, :setting-2 :AK, :need-2 :NA, :academic-year-2 10}
              {:calendar-year 2016, :setting-1 :AK, :need-1 :NA, :academic-year-1 10, :setting-2 :MU, :need-2 :NA, :academic-year-2 11}
              {:calendar-year 2017, :setting-1 :NONSEND, :need-1 :NA, :academic-year-1 11, :setting-2 :MMSIB, :need-2 :NA, :academic-year-2 12}
              {:calendar-year 2017, :setting-1 :NONSEND, :need-1 :NA, :academic-year-1 12, :setting-2 :MMSIB, :need-2 :NA, :academic-year-2 13}
              {:calendar-year 2017, :setting-1 :MU, :need-1 :NA, :academic-year-1 11, :setting-2 :MU, :need-2 :NA, :academic-year-2 12}))))
    (testing "with a reduce to allow more than two filters"
      (is (= (reduce #(sequence (mp/remove-transitions-xf %2) %1) transitions filter-transitions-from-c)
             (list
              {:calendar-year 2015, :setting-1 :NONSEND, :need-1 :NA, :academic-year-1 9, :setting-2 :AK, :need-2 :NA, :academic-year-2 10}
              {:calendar-year 2016, :setting-1 :AK, :need-1 :NA, :academic-year-1 10, :setting-2 :MU, :need-2 :NA, :academic-year-2 11}
              {:calendar-year 2017, :setting-1 :NONSEND, :need-1 :NA, :academic-year-1 11, :setting-2 :MMSIB, :need-2 :NA, :academic-year-2 12}
              {:calendar-year 2017, :setting-1 :NONSEND, :need-1 :NA, :academic-year-1 12, :setting-2 :MMSIB, :need-2 :NA, :academic-year-2 13}
              {:calendar-year 2017, :setting-1 :MU, :need-1 :NA, :academic-year-1 11, :setting-2 :MU, :need-2 :NA, :academic-year-2 12}))))))

(deftest transition-present?-test
  (testing "transition state is present in coll"
    (is (so/transition-present? [11 :CI-MSSOB :CI-ISSR] '([6 :OTH-MSSSH :OTH-MSSSH] [6 :SP-MU :SP-MU] [6 :UKN-MMSIB :UKN-MMSIB] [11 :CI-MSSOB :CI-ISSR] [6 :SP-IMS :SP-IMS] [6 :SEMH-MSSCT :SEMH-NMSS] [8 :CI-OOE :CI-ISS] [8 :CI-MMSOB :CI-MMSOB] [6 :SP-MU :SP-NMSS] [6 :CI-MSSSH :CI-MSSSH] [8 :SEMH-MMSIB :NONSEND] [13 :SP-MSSOB :NONSEND] [4 :CL-MSSSH :NONSEND])))))

(deftest update-ifelse-assoc-test
  (testing "if key present +1 to val"
    (is (= 2 (:foo (mp/update-ifelse-assoc {:foo 1 :bar 2} :foo + 1)))))
  (testing "if key not present, insert key with val"
    (is (= 1 (:foo (mp/update-ifelse-assoc {:baz 1 :bar 2} :foo + 1))))))
