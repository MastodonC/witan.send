(ns witan.send.model.prepare-test
  (:require [clojure.test :refer [deftest is testing]]
            [same :refer [ish?]]
            [witan.send.main :as m]
            [witan.send.model.prepare :as sut]
            [witan.send.send :as send]))

(deftest test-predicates-test
  (testing "filter by single key-value pair"
    (is (every? true? (sut/test-predicates {:a 1 :b 2 :c 3} {:a 1})))
    (is (every? false? (sut/test-predicates {:a 1 :b 2 :c 3} {:a 2}))))
  (testing "filter by multiple key-value pairs"
    (is (every? true? (sut/test-predicates {:a 1 :b 2 :c 3} {:a 1 :b 2})))
    (is (= '(true false) (sut/test-predicates {:a 1 :b 2 :c 3} {:a 1 :b 1}))))
  (testing "filter by multiple values for single key"
    (is (every? true? (sut/test-predicates {:a 1 :b 2 :c 3} {:a [1 2]})))
    (is (every? true? (sut/test-predicates {:a 2 :b 2 :c 3} {:a [1 2]}))))
  (testing "filter by multiple values for single key and by multiple keys"
    (is (every? true? (sut/test-predicates {:a 1 :b 2 :c 3} {:a [1 2] :b 2}))))
  (testing "fitler by single value even when in multiple value format"
    (is (every? true? (sut/test-predicates {:a 1 :b 2 :c 3} {:a [1]})))
    (is (every? false? (sut/test-predicates {:a 1 :b 2 :c 3} {:a [2]})))))

(deftest test-prepare-send-inputs
  (let [config (m/read-config "data/demo/config.edn")
        input-datasets (send/build-input-datasets (:project-dir config) (:file-inputs config) (:schema-inputs config))
        {:keys [seed-year standard-projection]}
        (sut/prepare-send-inputs input-datasets (:transition-parameters config) true)
        {:keys [mover-state-alphas mover-beta-params
                leaver-beta-params
                joiner-state-alphas joiner-beta-params]}
        standard-projection]
    (testing "Seed year is 2017"
      (is (= 2017 seed-year)))
    (testing "Mover state alphas sample"
      (is (ish? {:U-L 0.013157894736842106
                 :U-C 0.013157894736842106
                 :U-B 0.013157894736842106
                 :U-D 0.013157894736842106
                 :U-S 0.5921052631578947
                 :U-J 0.3026315789473684
                 :U-A 0.013157894736842106
                 :U-G 0.013157894736842106
                 :U-I 0.013157894736842106
                 :U-N 0.013157894736842106}
                (mover-state-alphas [15 :U-E]))))
    (testing "Mover beta params sample"
      (is (ish? {[ 2 :W-O ] {:alpha 0.053125, :beta 0.946875}
                 [ 16 :Y-S ] {:beta 2.9166666666666665, :alpha 0.08333333333333333 }
                 [ -1 :W-P ] {:alpha 0.2650862068965517, :beta 0.7349137931034483}
                 [ 15 :W-M ] {:alpha 0.22413793103448276, :beta 0.7758620689655172}
                 [ 10 :X-I ] {:alpha 1.0791433891992552, :beta 7.920856610800745}}
                (select-keys mover-beta-params
                             [[ 2 :W-O ] [ 16 :Y-S ] [ -1 :W-P ] [ 15 :W-M ] [ 10 :X-I ]]))))
    (testing "Leaver beta params sample"
      (is (ish? {:alpha 0.017335766423357664
                 :beta 13.982664233576642}
                (leaver-beta-params [6 :Y-F]))))
    (testing "Joiner state alphas sample"
      (is (ish? {:V-F 0.020833333333333332
                 :X-G 0.020833333333333332
                 :Y-B 12.020833333333334
                 :T-R 0.020833333333333332
                 :V-P 0.020833333333333332}
                (select-keys (joiner-state-alphas -1) [:V-F :X-G :Y-B :T-R :V-P]))))
    (testing "Joiner beta params sample"
      (is (ish? {5 {:alpha 12N, :beta 26189/4}
                 14 {:alpha 0.502, :beta 29447/4}
                 16 {:alpha 0.503, :beta 9191N}
                 10 {:alpha 33/4, :beta 11099/2}}
                (select-keys joiner-beta-params [5 14 16 10]))))))

(comment

  (def test-inputs
    (let [config (m/read-config "data/demo/config.edn")
          input-datasets (send/build-input-datasets (:project-dir config) (:file-inputs config) (:schema-inputs config))
          inputs (sut/prepare-send-inputs input-datasets (:transition-parameters config) true)]
      inputs))

  )


;; Tests
(deftest modify-transitions-test
  (let [transitions {[2014 1 :NONSEND :SEMH-MMSIB] 2, [2014 1 :NONSEND :SP-MMSIB] 3, [2014 1 :SP-MU :SP-MU] 2}
        state-change1 [[2014 1 :SP-MU :SP-MU] [2014 1 :NONSEND :SEMH-MMSIB]]
        state-change2 [[2014 1 :NONSEND :SP-MMSIB] [2014 1 :NONSEND :SEMH-MMSIB]]]
    (testing "modifies transitions"
      (is (not= transitions (sut/modify-transitions transitions state-change1 * 0.5))))
    (testing "state [2014 8 :SP-MU :SP-MU] is divided by two"
      (is (= 1 (get (sut/modify-transitions transitions state-change1 * 0.5) [2014 1 :SP-MU :SP-MU]))))
    (testing "state [2014 0 :NONSEND :SP-MMSIB] takes the joiners of state [2014 8 :SP-MU :SP-MU]"
      (is (= 3 (get (sut/modify-transitions transitions state-change1 * 0.5) [2014 1 :NONSEND :SEMH-MMSIB]))))
    (testing "odd values are rounded and exchanged correctly"
      (is (= 3 (get (sut/modify-transitions transitions state-change2 * 0.5) [2014 1 :NONSEND :SEMH-MMSIB])))
      (is (= 2 (get (sut/modify-transitions transitions state-change2 * 0.5) [2014 1 :NONSEND :SP-MMSIB]))))))

(deftest remove-transitions-xf-test
  ;; This test needs splitting to simpler cases but for speed captures
  ;; most things.  There's some discussion to be had over how we
  ;; should treat setting-2 as it moves into calendar year 2017
  ;;
  ;; Note: the test discovered the keyname is required not a string :-(
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
      (is (= (sequence (sut/remove-transitions-xf filter-transitions-from-a) transitions)
             (list
              {:calendar-year 2015, :setting-1 :NONSEND, :need-1 :NA, :academic-year-1 9, :setting-2 :AK, :need-2 :NA, :academic-year-2 10}
              {:calendar-year 2016, :setting-1 :MU, :need-1 :NA, :academic-year-1 10, :setting-2 :MU, :need-2 :NA, :academic-year-2 11}
              {:calendar-year 2016, :setting-1 :AK, :need-1 :NA, :academic-year-1 10, :setting-2 :MU, :need-2 :NA, :academic-year-2 11}
              {:calendar-year 2017, :setting-1 :NONSEND, :need-1 :NA, :academic-year-1 11, :setting-2 :MMSIB, :need-2 :NA, :academic-year-2 12}
              {:calendar-year 2017, :setting-1 :NONSEND, :need-1 :NA, :academic-year-1 12, :setting-2 :MMSIB, :need-2 :NA, :academic-year-2 13}
              {:calendar-year 2017, :setting-1 :MU, :need-1 :NA, :academic-year-1 11, :setting-2 :MU, :need-2 :NA, :academic-year-2 12}))))
    (testing "filters transitions from before 2017 in the setting MU"
      (is (= (sequence (sut/remove-transitions-xf filter-transitions-from-b) transitions)
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
                  (sequence (sut/remove-transitions-xf (first filter-transitions-from-c)))
                  (sequence (sut/remove-transitions-xf (second filter-transitions-from-c))))
             (list
              {:calendar-year 2015, :setting-1 :NONSEND, :need-1 :NA, :academic-year-1 9, :setting-2 :AK, :need-2 :NA, :academic-year-2 10}
              {:calendar-year 2016, :setting-1 :AK, :need-1 :NA, :academic-year-1 10, :setting-2 :MU, :need-2 :NA, :academic-year-2 11}
              {:calendar-year 2017, :setting-1 :NONSEND, :need-1 :NA, :academic-year-1 11, :setting-2 :MMSIB, :need-2 :NA, :academic-year-2 12}
              {:calendar-year 2017, :setting-1 :NONSEND, :need-1 :NA, :academic-year-1 12, :setting-2 :MMSIB, :need-2 :NA, :academic-year-2 13}
              {:calendar-year 2017, :setting-1 :MU, :need-1 :NA, :academic-year-1 11, :setting-2 :MU, :need-2 :NA, :academic-year-2 12}))))
    (testing "with a reduce to allow more than two filters"
      (is (= (reduce #(sequence (sut/remove-transitions-xf %2) %1) transitions filter-transitions-from-c)
             (list
              {:calendar-year 2015, :setting-1 :NONSEND, :need-1 :NA, :academic-year-1 9, :setting-2 :AK, :need-2 :NA, :academic-year-2 10}
              {:calendar-year 2016, :setting-1 :AK, :need-1 :NA, :academic-year-1 10, :setting-2 :MU, :need-2 :NA, :academic-year-2 11}
              {:calendar-year 2017, :setting-1 :NONSEND, :need-1 :NA, :academic-year-1 11, :setting-2 :MMSIB, :need-2 :NA, :academic-year-2 12}
              {:calendar-year 2017, :setting-1 :NONSEND, :need-1 :NA, :academic-year-1 12, :setting-2 :MMSIB, :need-2 :NA, :academic-year-2 13}
              {:calendar-year 2017, :setting-1 :MU, :need-1 :NA, :academic-year-1 11, :setting-2 :MU, :need-2 :NA, :academic-year-2 12}))))))

(deftest update-ifelse-assoc-test
  (testing "if key present +1 to val"
    (is (= 2 (:foo (sut/update-ifelse-assoc {:foo 1 :bar 2} :foo + 1)))))
  (testing "if key not present, insert key with val"
    (is (= 1 (:foo (sut/update-ifelse-assoc {:baz 1 :bar 2} :foo + 1))))))
