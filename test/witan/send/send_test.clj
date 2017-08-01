(ns witan.send.send-test
  (:require [clojure.test :refer :all]
            [witan.send.send :refer :all]
            [witan.send.schemas :as sc]
            [witan.send.test-utils :as tu]
            [witan.send.utils :as u]
            [clojure.core.matrix.dataset :as ds]
            [witan.datasets :as wds]))

;; Use real population datasets for testing
(def test-inputs
  {:initial-population ["data/demo/initial-population.csv" sc/PopulationSYA]
   :initial-send-population ["data/demo/send-population.csv" sc/SENDPopulation]
   :transition-matrix ["data/demo/transitions.csv" sc/TransitionCounts]
   :projected-population ["data/demo/projected-population.csv" sc/PopulationSYA]
   :setting-cost ["data/demo/setting-costs.csv" sc/SettingCost]})

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

;; TODO
