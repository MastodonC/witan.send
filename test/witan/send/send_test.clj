(ns witan.send.send-test
  (:require [clojure.test :refer :all]
            [witan.send.send :refer :all]
            [witan.send.schemas :as sc]
            [witan.send.test-utils :as tu]
            [witan.send.utils :as u]
            [clojure.core.matrix.dataset :as ds]
            [witan.datasets :as wds]
            [witan.send.params :as p]))

;; Use real population datasets for testing
(def test-inputs
  {:initial-population ["data/demo/data/population.csv" sc/PopulationDataset]
   :initial-send-population ["data/demo/data/send-population.csv" sc/SENDPopulation]
   :transition-matrix ["data/demo/data/transitions.csv" sc/TransitionCounts]})

(defn get-individual-input [key-name]
  (tu/read-inputs
   test-inputs
   {:witan/name key-name}
   []
   (get test-inputs key-name)))

(defn reduce-input [key-name column value]
  (-> key-name
      get-individual-input
      (wds/select-from-ds {column {:lte value}})))

;; Use fake population datasets for testing
(def grouped-data
  (ds/dataset {:year  [2014 2015 2016]
               :age   [0 1 2]
               :state [:Non-SEND :Non-SEND :PSI-Mainstream]
               :count [3 0 1]}))

(def individuals-data-with-sims
  (ds/dataset {:year  [2014 2014 2014 2014 2014 2014 2016 2016]
               :age   [0 0 0 0 0 0 2 2]
               :state [:Non-SEND :Non-SEND :Non-SEND :Non-SEND
                       :Non-SEND :Non-SEND :PSI-Mainstream :PSI-Mainstream]
               :sim-num [1 2 1 2 1 2 1 2]
               :id    [1 1 2 2 3 3 4 4]}))

(def historic-data
  (ds/dataset {:year  [2016 2016 2016]
               :age   [0 1 2]
               :population [3 0 1]}))

(def projected-population
  (ds/dataset {:year [2017 2017 2017 2018 2018 2018 2019 2019 2019 2020 2020 2020]
               :age [0 1 2 0 1 2 0 1 2 0 1 2]
               :population [2 5 5 1 3 6 2 3 7 3 6 9]}))

(def population-diff
  (ds/dataset {:year [2017 2017 2017 2018 2018 2018 2019 2019 2019]
               :age [0 1 2 0 1 2 0 1 2]
               :population-from-proj [2 5 5 1 3 6 2 3 7]
               :previous-year [2016 2016 2016 2017 2017 2017 2018 2018 2018]
               :population [3 0 1 2 5 5 1 3 6]
               :aged-on-population [0 3 0 0 2 5 0 1 3]
               :population-diff [2 2 5 1 1 1 2 2 4]}))

;; for proj-start-year = 2017, proj-end-year = 2019 and 2 simulations
(def extra-individuals
  (ds/dataset {:year [2017 2017 2017 2017 2017 2017 2017 2017 2017
                      2017 2017 2017 2017 2017 2017 2017 2017 2017
                      2018 2018 2018 2018 2018 2018
                      2019 2019 2019 2019 2019 2019 2019 2019
                      2019 2019 2019 2019 2019 2019 2019 2019]
               :age [0 0 0 0 1 1 1 1 2 2 2 2 2 2 2 2 2 2 ;;18
                     0 0 1 1 2 2                         ;;6
                     0 0 0 0 1 1 1 1 2 2 2 2 2 2 2 2] ;;16 total = 40
               :state [:Non-SEND :Non-SEND :Non-SEND :Non-SEND :Non-SEND :Non-SEND
                       :Non-SEND :Non-SEND :Non-SEND :Non-SEND :Non-SEND :Non-SEND
                       :Non-SEND :Non-SEND :Non-SEND :Non-SEND :Non-SEND :Non-SEND
                       :Non-SEND :Non-SEND :Non-SEND :Non-SEND :Non-SEND :Non-SEND
                       :Non-SEND :Non-SEND :Non-SEND :Non-SEND :Non-SEND :Non-SEND
                       :Non-SEND :Non-SEND :Non-SEND :Non-SEND :Non-SEND :Non-SEND
                       :Non-SEND :Non-SEND :Non-SEND :Non-SEND]
               :sim-num [1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2
                         1 2 1 2 1 2 1 2 1 2]
               :id [5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13 14 14 15 15 16 16
                    17 17 18 18 19 19 20 20 21 21 22 22 23 23 24 24]}))

(def population-total
  (ds/dataset {:year [2014 2014 2014 2014 2014 2014 2016 2016
                      2017 2017 2017 2017 2017 2017 2017 2017
                      2017 2017 2017 2017 2017 2017 2017 2017
                      2017 2017
                      2018 2018 2018 2018 2018 2018
                      2019 2019 2019 2019 2019 2019 2019 2019
                      2019 2019 2019 2019 2019 2019 2019 2019]
               :age [0 0 0 0 0 0 2 2 0 0 0 0 1 1 1 1 2 2 2 2 2 2 2 2 2 2
                     0 0 1 1 2 2 0 0 0 0 1 1 1 1 2 2 2 2 2 2 2 2]
               :state [:Non-SEND :Non-SEND :Non-SEND :Non-SEND :Non-SEND :Non-SEND
                       :PSI-Mainstream :PSI-Mainstream :Non-SEND :Non-SEND :Non-SEND
                       :Non-SEND :Non-SEND :Non-SEND :Non-SEND :Non-SEND :Non-SEND
                       :Non-SEND :Non-SEND :Non-SEND :Non-SEND :Non-SEND :Non-SEND
                       :Non-SEND :Non-SEND :Non-SEND :Non-SEND :Non-SEND :Non-SEND
                       :Non-SEND :Non-SEND :Non-SEND :Non-SEND :Non-SEND :Non-SEND
                       :Non-SEND :Non-SEND :Non-SEND :Non-SEND :Non-SEND :Non-SEND
                       :Non-SEND :Non-SEND :Non-SEND :Non-SEND :Non-SEND :Non-SEND :Non-SEND]
               :sim-num [1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2
                         1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2]
               :id [1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13 14 14 15 15
                    16 16 17 17 18 18 19 19 20 20 21 21 22 22 23 23 24 24]}))


;; Helpers for tests validation
(defmacro is-valid-result?
  [result num-sims result-keys expected-num-rows]
  `(do
     (is (= ~result-keys (set (keys ~result))))
     (when (~result-keys :age)
       (is (= ~expected-num-rows (count (:age ~result))))
       (is (every? number?  (:age ~result))))
     (when (~result-keys :id)
       (is (every? number?  (:id ~result))))
     (when (~result-keys :state)
       (is (every? keyword? (:state ~result))))
     (when (~result-keys :sim-num)
       (is (every? #(and (number? %)
                         (<= % ~num-sims)) (:sim-num ~result))))
     (when (~result-keys :year)
       (is (every? #(and (number? %)
                         (>= % ~2016)) (:year ~result))))))

(defmacro is-valid-result-ds?
  [result num-sims expected-num-rows]
  `(do
     (is (= #{:age :state :year :sim-num :id} (set (:column-names ~result))))
     (is (= ~expected-num-rows (count (ds/column ~result :age))))
     (is (every? number?  (ds/column ~result :age)))
     (is (every? number?  (ds/column ~result :id)))
     (is (every? keyword? (ds/column ~result :state)))
     (is (every? #(and (number? %)
                       (<= % ~num-sims)) (ds/column ~result :sim-num)))
     (is (every? #(and (number? %)
                       (>= % ~2016)) (ds/column ~result :year)))))

;; Tests

(deftest joiner-rate-test
  (let [joiner-count (-> :transition-matrix get-individual-input ds/row-maps p/calculate-joiners-per-calendar-year)
        population-count (-> :initial-population get-individual-input ds/row-maps p/calculate-population-per-calendar-year)
        ages (-> population-count first val keys)
        years [2013 2014 2015 2016]
        result (joiner-rate joiner-count population-count ages years)]
    (testing "output is not empty"
      (is (not= empty? result)))
    (testing "all ages have rates calculated"
      (is (= (keys result) ages)))))

(deftest mover-rate-test
  (let [mover-count (->> :transition-matrix get-individual-input ds/row-maps (remove (fn [{:keys [setting-1 setting-2]}]
                                                                                       (or (= setting-1 sc/non-send)
                                                                                           (= setting-2 sc/non-send)))))
        result (mover-rate mover-count)]
    (testing "output is not empty"
      (is (not= empty? result)))))

(deftest leaver-rate-test
  (let [leaver-count (->> :transition-matrix get-individual-input ds/row-maps (remove (fn [{:keys [setting-1]}] (= setting-1 sc/non-send))))
        result (leaver-rate leaver-count)]
    (testing "output is not empty"
      (is (not= empty? result)))))

(deftest confidence-interval-test
  (let [leaver-rates (->> :transition-matrix get-individual-input ds/row-maps (remove (fn [{:keys [setting-1]}] (= setting-1 sc/non-send))) leaver-rate)
        result (confidence-interval leaver-rates 2014)]
    (testing "output is not empty"
      (is (not= empty? result) ))
    (testing "all intervals are between 0 and 1"
      (is (every? #(and (<= 0 %) (>= 1 %)) (map #(nth % 1) result)))
      (is (every? #(and (<= 0 %) (>= 1 %)) (map #(nth % 2) result))))
    (testing "all vals are numbers"
      (is (every? #(number? %) (reduce concat result))))))


(deftest modify-transitions-test
  (let [transitions {[2014 1 :NONSEND :MMSIB] 2, [2014 1 :NONSEND :MSSIB] 3, [2014 1 :MU :MU] 2}
        state-change1 [[2014 1 :MU :MU] [2014 1 :NONSEND :MMSIB]]
        state-change2 [[2014 1 :NONSEND :MMSIB] [2014 1 :NONSEND :MMSIB]]]
    (testing "modifies transitions"
      (is (not= transitions (modify-transitions transitions state-change1 * 0.5))))
    (testing "state [2014 8 :SP-MU :SP-MU] is divided by two"
      (is (= 1 (get (modify-transitions transitions state-change1 * 0.5) [2014 1 :MU :MU]))))
    (testing "state [2014 0 :NONSEND :SP-MMSIB] takes the joiners of state [2014 8 :SP-MU :SP-MU]"
      (is (= 3 (get (modify-transitions transitions state-change1 * 0.5) [2014 1 :NONSEND :MMSIB]))))
    (testing "odd values are rounded and exchanged correctly"
      (is (= 3 (get (modify-transitions transitions state-change2 * 0.5) [2014 1 :NONSEND :MSSIB])))
      (is (= 2 (get (modify-transitions transitions state-change2 * 0.5) [2014 1 :NONSEND :MMSIB]))))))

(deftest transition-present?-test
  (testing "transition state is present in coll"
    (is (transition-present? [11 :CI-MSSOB :CI-ISSR] '([6 :OTH-MSSSH :OTH-MSSSH] [6 :SP-MU :SP-MU] [6 :UKN-MMSIB :UKN-MMSIB] [11 :CI-MSSOB :CI-ISSR] [6 :SP-IMS :SP-IMS] [6 :SEMH-MSSCT :SEMH-NMSS] [8 :CI-OOE :CI-ISS] [8 :CI-MMSOB :CI-MMSOB] [6 :SP-MU :SP-NMSS] [6 :CI-MSSSH :CI-MSSSH] [8 :SEMH-MMSIB :NONSEND] [13 :SP-MSSOB :NONSEND] [4 :CL-MSSSH :NONSEND])))))

(deftest update-ifelse-assoc-test
  (testing "if key present +1 to val"
    (is (= 2 (:foo (update-ifelse-assoc {:foo 1 :bar 2} :foo + 1)))))
  (testing "if key not present, insert key with val"
    (is (= 1 (:foo (update-ifelse-assoc {:baz 1 :bar 2} :foo + 1))))))

(deftest generate-transition-key-test ;;; needs fixing??
  (testing "generate joiner state transition"
    (is (= [2013 5 :NONSEND :CI-MSSOB]
           (generate-transition-key {:transition-type "joiners" :cy 2013 :ay 5
                                     :need :CI :static-setting :MSSIB
                                     :setting :MSSOB}))))
  (testing "generate leaver state transition"
    (is (= [2013 5 :CI-MSSOB :NONSEND]
           (generate-transition-key {:transition-type "leavers" :cy 2013 :ay 5
                                     :need :CI :static-setting :MSSIB
                                     :setting :MSSOB}))))
  (testing "generate mover-to state transition" ;;; NEEDS FIXING
    (is (= [2013 5 :CI-MSSIB :CI-MSSOB]
           (generate-transition-key {:transition-type "movers-to" :cy 2013 :ay 5
                                     :need :CI :static-setting :MSSIB
                                     :setting :MSSOB}))))
  (testing "generate mover-from state transition" ;;; NEEDS FIXING
    (is (= [2013 5 :CI-MSSOB :CI-MSSIB]
           (generate-transition-key {:transition-type "movers-from" :cy 2013 :ay 5
                                     :need :CI :static-setting :MSSIB
                                     :setting :MSSOB})))))

(deftest build-states-to-change-test
  (let [input (ds/dataset [{:setting-1 :MMSOB :setting-2 :MMSIB}])]
    (testing "move MSSOB joiners to MSSIB"
      (is (= [[2013 5 :NONSEND :CI-MMSOB] [2013 5 :NONSEND :CI-MMSIB]]
             (first (build-states-to-change input [:CI] [:MSSOB] [5] [2013] "joiners")))))
    (testing "move MSSOB leavers to MSSIB"
      (is (= [[2013 5 :CI-MMSOB :NONSEND] [2013 5 :CI-MMSIB :NONSEND]]
             (first (build-states-to-change input [:CI] [:MSSOB] [5] [2013] "leavers")))))
    (testing "move MSSOB movers-to to MSSIB"
      (is (= [[2013 5 :CI-MSSOB :CI-MMSOB] [2013 5 :CI-MSSOB :CI-MMSIB]]
             (first (build-states-to-change input [:CI] [:MSSOB] [5] [2013] "movers-to")))))
    (testing "move MSSOB movers-from to MSSIB"
      (is (= [[2013 5 :CI-MMSOB :CI-MSSOB] [2013 5 :CI-MMSIB :CI-MSSOB]]
             (first (build-states-to-change input [:CI] [:MSSOB] [5] [2013] "movers-from")))))
    (testing "return nil when move to and from state the same"
      (is (= nil
             (first (build-states-to-change input [:CI] [:MMSOB] [5] [2013] "movers-to")))))))
