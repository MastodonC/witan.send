(ns witan.send.send-test
  (:require [clojure.test :refer :all]
            [witan.send.send :refer :all]
            [witan.send.schemas :as sc]
            [witan.send.test-utils :as tu]
            [clojure.core.matrix.dataset :as ds]
            [witan.datasets :as wds]))

;; Use real population datasets for testing
(def test-inputs
  {:historic-0-25-population ["data/demo/Population_0_25.csv" sc/PopulationSYA]
   :historic-send-population ["data/demo/send_population.csv" sc/SENDSchemaGrouped]
   :population-projection ["data/demo/Population_projection.csv" sc/PopulationSYA]
   :cost-profile ["data/demo/cost_profile.csv" sc/CostProfile]
   :transitions-default []
   :transitions-reduced-secondary-joiners []})

(defn get-individual-input [key-name]
  (tu/read-inputs
   test-inputs
   {:witan/name key-name}
   []
   (get test-inputs key-name)))

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
               :id    [1 1 2 2 3 3 4 4]
               :sim-num [1 2 1 2 1 2 1 2]}))

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
               :id [5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13 14 14 15 15 16 16
                    17 17 18 18 19 19 20 20 21 21 22 22 23 23 24 24]
               :sim-num [1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2
                         1 2 1 2 1 2 1 2 1 2]}))

(def population-with-states
  (let [historic-send-population (get-individual-input :historic-send-population)
        historic-0-25-population (get-individual-input :historic-0-25-population)]
    (-> historic-send-population
        add-state-to-send-population
        (add-non-send-to-send-population historic-0-25-population))))

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
(deftest add-state-to-send-population-test
  (testing "The states are added and need, placement are removed"
    (let [historic-send-population (get-individual-input :historic-send-population)
          send-with-states (add-state-to-send-population historic-send-population)]
      (is (= (:column-names send-with-states)
             [:year :age :state :count]))
      (is (every? keyword? (distinct (wds/subset-ds send-with-states :cols :state)))))))

(deftest add-non-send-to-send-population-test
  (testing "Creates a population with send and non-send"
    (let [historic-send-population (get-individual-input :historic-send-population)
          historic-0-25-population (get-individual-input :historic-0-25-population)
          send-with-states (add-state-to-send-population historic-send-population)
          popn-with-states (add-non-send-to-send-population send-with-states
                                                            historic-0-25-population)]
      (is (every? keyword? (distinct (wds/subset-ds popn-with-states :cols :state))))
      (is (some #(= :Non-SEND %) (distinct (wds/subset-ds popn-with-states :cols :state)))))))

(deftest grps-to-indiv-test
  (testing "Going from group data to individuals data"
    (let [num-sims 1
          {:keys [repeat-data other-cols matrix-other-cols col-freqs range-individuals]}
          (prepare-for-data-transformation
           population-with-states
           :count
           num-sims
           1)
          result (grps-to-indiv repeat-data other-cols matrix-other-cols)]
      (is-valid-result? result num-sims #{:age :state :year} 198372))))

(deftest add-simul-nbs-test
  (testing "Adding simulation numbers to individuals data"
    (let [num-sims 1
          {:keys [repeat-data other-cols matrix-other-cols col-freqs range-individuals]}
          (prepare-for-data-transformation
           population-with-states
           :count
           num-sims
           1)
          indiv-data (grps-to-indiv repeat-data other-cols matrix-other-cols)
          result (add-simul-nbs indiv-data col-freqs 1)]
      (is-valid-result? result num-sims #{:age :state :year :sim-num} 198372))))

(deftest add-ids-test
  (testing "Adding ids to each row, taking into account simulations"
    (let [num-sims 1
          {:keys [repeat-data other-cols matrix-other-cols col-freqs range-individuals]}
          (prepare-for-data-transformation
           population-with-states
           :count
           num-sims
           1)
          indiv-data (grps-to-indiv repeat-data other-cols matrix-other-cols)
          indiv-data-with-sims (add-simul-nbs indiv-data col-freqs 1)
          result (add-ids indiv-data-with-sims 1 range-individuals)]
      (is-valid-result? result num-sims #{:age :state :year :sim-num :id} 198372))))

(deftest data-transformation-test
  (testing "Go from group data to individuals data with ids and simulation number"
    (let [transformed-data (data-transformation grouped-data :count 2)]
      (is (= (set (:column-names individuals-data-with-sims))
             (set (:column-names transformed-data))))
      (is (= (set (:columns individuals-data-with-sims))
             (set (:columns transformed-data)))))))

(deftest calc-population-difference-test
  (testing "the population difference is calculated correctly"
    (let [historic-population (get-individual-input :historic-0-25-population)
          population-projection (get-individual-input :population-projection)
          projection-start-year 2017
          projection-end-year 2019
          hist-popn (wds/filter-dataset historic-population [:year]
                                        (fn [y] (= y (dec projection-start-year))))
          popn-proj (wds/filter-dataset population-projection [:year]
                                        (fn [y] (and (>= y projection-start-year)
                                                     (<= y projection-end-year))))
          diff-population (calc-population-difference hist-popn popn-proj
                                                      projection-start-year
                                                      projection-end-year)
          diff-age1-year2017 (wds/subset-ds (wds/select-from-ds diff-population
                                                                {:year {:eq 2017}
                                                                 :age {:eq 1}})
                                            :cols :population-diff)
          proj-age1-year2017 (wds/subset-ds (wds/select-from-ds
                                             population-projection
                                             {:year {:eq 2017} :age {:eq 1}})
                                            :cols :population)
          hist-popn-age0-year2016 (wds/subset-ds (wds/select-from-ds
                                                  historic-population
                                                  {:year {:eq 2016} :age {:eq 0}})
                                                 :cols :population)]
      (is (set (:column-names population-diff)) (set (:column-names diff-population)))
      (is (set (:columns population-diff)) (set (:columns diff-population)))
      (is (= (- proj-age1-year2017 hist-popn-age0-year2016)
             diff-age1-year2017))))
  (testing "It works on fake data"
    (let [hist-popn (wds/filter-dataset historic-data [:year]
                                        (fn [y] (= y (dec 2017))))
          popn-proj (wds/filter-dataset projected-population [:year]
                                        (fn [y] (and (>= y 2017)
                                                     (<= y 2019))))
          diff-popn (calc-population-difference hist-popn popn-proj 2017 2019)
          diff-age1-year2017 (wds/subset-ds (wds/select-from-ds diff-popn
                                                                {:year {:eq 2017}
                                                                 :age {:eq 1}})
                                            :cols :population-diff)
          proj-age1-year2017 (wds/subset-ds (wds/select-from-ds
                                             projected-population
                                             {:year {:eq 2017} :age {:eq 1}})
                                            :cols :population)
          hist-popn-age0-year2016 (wds/subset-ds (wds/select-from-ds
                                                  historic-data
                                                  {:year {:eq 2016} :age {:eq 0}})
                                                 :cols :population)]
      (is (some #{:previous-year} (:column-names diff-popn)))
      (is (some #{:aged-on-population} (:column-names diff-popn)))
      (is (some #{:population-diff} (:column-names diff-popn)))
      (is (some #{:population-from-proj} (:column-names diff-popn)))
      (is (= (- proj-age1-year2017 hist-popn-age0-year2016)
             diff-age1-year2017)))))

(deftest get-historic-population-1-0-0-test
  (testing "The historic population has send states, ids and simulation numbers associated to it"
    (time
     (let [{:keys [historic-population]}
           (get-historic-population-1-0-0 {:historic-0-25-population
                                           (get-individual-input :historic-0-25-population)
                                           :historic-send-population
                                           (get-individual-input :historic-send-population)}
                                          {:projection-start-year 2017
                                           :number-of-simulations 1})]
       (is (= 198372 (first (:shape historic-population))))
       (is (some #{:state} (:column-names historic-population)))
       (is (some #{:id} (:column-names historic-population)))
       (is (some #{:sim-num} (:column-names historic-population)))
       (is (some #{:year} (:column-names historic-population)))
       (is (some #{:age} (:column-names historic-population)))
       (is-valid-result-ds? historic-population 1 198372)))))

(deftest population-change-1-0-0-test
  (testing "The extra population has send states, ids and simulation numbers associated to it"
    (let [{:keys [extra-population]}
          (population-change-1-0-0 {:historic-0-25-population
                                    (get-individual-input :historic-0-25-population)
                                    :population-projection
                                    (get-individual-input :population-projection)}
                                   {:projection-start-year 2017
                                    :projection-end-year 2019
                                    :number-of-simulations 1})]
      (is (= 30747 (first (:shape extra-population))))
      (is (some #{:state} (:column-names extra-population)))
      (is (some #{:id} (:column-names extra-population)))
      (is (some #{:sim-num} (:column-names extra-population)))
      (is (some #{:year} (:column-names extra-population)))
      (is (some #{:age} (:column-names extra-population)))
      (is-valid-result-ds? extra-population 1 30747)))
  (testing "The extra population from test data has the correct individuals for 2017-2019 projection"
    (let [{:keys [extra-population]}
          (population-change-1-0-0 {:historic-0-25-population historic-data
                                    :population-projection projected-population}
                                   {:projection-start-year 2017
                                    :projection-end-year 2019
                                    :number-of-simulations 2})]
      (is (= (first (:shape extra-individuals))
             (first (:shape extra-population))))
      (is (= (set (:column-names extra-population)) (set (:column-names extra-individuals))))
      (is (= (set (:columns extra-population)) (set (:columns extra-individuals))))
      (is-valid-result-ds? extra-population 2 40))))
