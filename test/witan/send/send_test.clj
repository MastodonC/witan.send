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
   :transition-matrix ["data/demo/transition_matrix.csv" sc/DataForMatrix]})

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

(def population-with-states
  (let [historic-send-population (reduce-input :historic-send-population :age 6)
        historic-0-25-population (reduce-input :historic-0-25-population :age 6)]
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
    (let [historic-send-population (reduce-input :historic-send-population :age 6)
          send-with-states (add-state-to-send-population historic-send-population)]
      (is (= (set (:column-names send-with-states))
             #{:year :age :state :population}))
      (is (every? keyword? (distinct (wds/subset-ds send-with-states :cols :state)))))))

(deftest add-non-send-to-send-population-test
  (testing "Creates a population with send and non-send"
    (let [historic-send-population (reduce-input :historic-send-population :age 6)
          historic-0-25-population (reduce-input :historic-0-25-population :age 6)
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
      (is-valid-result? result num-sims #{:age :state :year} 52243))))

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
      (is-valid-result? result num-sims #{:age :state :year :sim-num} 52243))))

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
      (is-valid-result? result num-sims #{:age :state :year :sim-num :id} 52243))))

(deftest data-transformation-test
  (testing "Go from group data to individuals data with ids and simulation number"
    (let [transformed-data (data-transformation grouped-data :count 2)]
      (is (= (set (:column-names individuals-data-with-sims))
             (set (:column-names transformed-data))))
      (is (= (set (:columns individuals-data-with-sims))
             (set (:columns transformed-data)))))))

(deftest calc-population-difference-test
  (testing "the population difference is calculated correctly"
    (let [historic-population (reduce-input :historic-0-25-population :age 6)
          population-projection (reduce-input :population-projection :age 6)
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
      (is (= (set (:columns population-diff)) (set (:columns diff-popn))))
      (is (= (- proj-age1-year2017 hist-popn-age0-year2016)
             diff-age1-year2017)))))

(deftest get-historic-population-1-0-0-test
  (testing "The historic population has send states, ids and simulation numbers associated to it"
    (let [{:keys [historic-population]}
          (get-historic-population-1-0-0 {:historic-0-25-population
                                          (reduce-input :historic-0-25-population :age 6)
                                          :historic-send-population
                                          (reduce-input :historic-send-population :age 6)}
                                         {:projection-start-year 2017
                                          :number-of-simulations 1})]
      (is (= 52243 (first (:shape historic-population))))
      (is (some #{:state} (:column-names historic-population)))
      (is (some #{:id} (:column-names historic-population)))
      (is (some #{:sim-num} (:column-names historic-population)))
      (is (some #{:year} (:column-names historic-population)))
      (is (some #{:age} (:column-names historic-population)))
      (is-valid-result-ds? historic-population 1 52243))))

(deftest population-change-1-0-0-test
  (testing "The extra population has send states, ids and simulation numbers associated to it"
    (let [{:keys [extra-population]}
          (population-change-1-0-0 {:historic-0-25-population
                                    (reduce-input :historic-0-25-population :age 6)
                                    :population-projection
                                    (reduce-input :population-projection :age 6)}
                                   {:projection-start-year 2017
                                    :projection-end-year 2019
                                    :number-of-simulations 1})]
      (is (= 24048 (first (:shape extra-population))))
      (is (some #{:state} (:column-names extra-population)))
      (is (some #{:id} (:column-names extra-population)))
      (is (some #{:sim-num} (:column-names extra-population)))
      (is (some #{:year} (:column-names extra-population)))
      (is (some #{:age} (:column-names extra-population)))
      (is-valid-result-ds? extra-population 1 24048)))
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

(deftest add-extra-population-1-0-0-test
  (testing "The datasets out of both data prep steps are joined correctly"
    (let [{:keys [total-population]}
          (add-extra-population-1-0-0 {:historic-population individuals-data-with-sims
                                       :extra-population extra-individuals}
                                      {:projection-start-year 2017})]
      (is (= (set (:column-names total-population))
             (set (:column-names population-total))))
      (is (= (set (:columns total-population))
             (set (:columns population-total))))
      (is (= (first (:shape total-population))
             (+ (first (:shape individuals-data-with-sims))
                (first (:shape extra-individuals))))))))

(def old-matrix (ds/dataset [{:age 10 :from-state :Non-SEND
                              :to-state :ASD-Mainstream :probability 0.3}
                             {:age 10 :from-state :Non-SEND
                              :to-state :Non-SEND :probability 0.7}
                             {:age 11 :from-state :Non-SEND
                              :to-state :ASD-Mainstream :probability 1.0}
                             {:age 11 :from-state :Non-SEND
                              :to-state :Non-SEND :probability 0.0}
                             {:age 12 :from-state :Non-SEND
                              :to-state :ASD-Mainstream :probability 0.2}
                             {:age 12 :from-state :Non-SEND
                              :to-state :Non-SEND :probability 0.8}]))

(def new-matrix (ds/dataset [{:age 10 :from-state :Non-SEND
                              :to-state :ASD-Mainstream :probability 0.3}
                             {:age 10 :from-state :Non-SEND
                              :to-state :Non-SEND :probability 0.7}
                             {:age 11 :from-state :Non-SEND
                              :to-state :ASD-Mainstream :probability 0.1}
                             {:age 11 :from-state :Non-SEND
                              :to-state :Non-SEND :probability 0.9}
                             {:age 12 :from-state :Non-SEND
                              :to-state :ASD-Mainstream :probability 0.2}
                             {:age 12 :from-state :Non-SEND
                              :to-state :Non-SEND :probability 0.8}]))

(deftest adjust-joiners-transition-1-0-0-test
  (testing "The right age group probability is updated using the multiplier param"
    (let [{:keys [transition-matrix]} (adjust-joiners-transition-1-0-0
                                       {:transition-matrix old-matrix}
                                       {:age 11 :multiplier 0.1})]
      (is (= (set (wds/subset-ds new-matrix :cols :age))
             (set (wds/subset-ds transition-matrix :cols :age))))
      (is (= (set (wds/subset-ds new-matrix :cols :from-state))
             (set (wds/subset-ds transition-matrix :cols :from-state))))
      (is (= (set (wds/subset-ds new-matrix :cols :to-state))
             (set (wds/subset-ds transition-matrix :cols :to-state))))
      (is (= (set (wds/subset-ds new-matrix :cols :probability))
             (set (wds/subset-ds transition-matrix :cols :probability)))))))

(deftest select-starting-population-1-0-0-test
  (testing "select current year from total population"
    (let [{:keys [current-population]} (select-starting-population-1-0-0
                                        {:total-population population-total
                                         :current-year-in-loop 2017})]
      (is (every? (partial =  2017)
                  (ds/column current-population :year))))))

(def trans-matrix (ds/dataset [{:age 2 :from-state :PSI-Mainstream
                                :to-state :PSI-Special :probability 0.7}
                               {:age 2 :from-state :PSI-Mainstream
                                :to-state :Non-SEND :probability 0.3}]))

(defn modify-trans-matrix [transition-matrix]
  (let [matrix-rows (ds/row-maps transition-matrix)
        from-non-send-and-age (fn [m] (and (= (:from-state m) :Non-SEND) (= (:age m) 0)))
        bucket-fn (fn [row] (cond
                              (not (from-non-send-and-age row)) :non-selected-rows
                              (and (from-non-send-and-age row)
                                   (= (:to-state row) :ASD-Mainstream)) :selected-rows-send
                              :else :selected-rows))
        {:keys [selected-rows selected-rows-send non-selected-rows]}
        (group-by bucket-fn matrix-rows)
        update-non-send (mapv (fn [m]
                                (assoc m :probability 0.0)) selected-rows)
        update-send (mapv (fn [m]
                            (assoc m :probability 1.0)) selected-rows-send)]
    (ds/dataset (concat non-selected-rows update-non-send update-send))))

(deftest apply-state-changes-1-0-0-test
  (testing "The new population is created correctly"
    (let [{:keys [historic-population]}
          (get-historic-population-1-0-0 {:historic-0-25-population
                                          (reduce-input :historic-0-25-population :age 6)
                                          :historic-send-population
                                          (reduce-input :historic-send-population :age 6)}
                                         {:projection-start-year 2017
                                          :number-of-simulations 1})
          {:keys [extra-population]}
          (population-change-1-0-0 {:historic-0-25-population
                                    (reduce-input :historic-0-25-population :age 6)
                                    :population-projection
                                    (reduce-input :population-projection :age 6)}
                                   {:projection-start-year 2017
                                    :projection-end-year 2019
                                    :number-of-simulations 1})
          {:keys [total-population]}
          (add-extra-population-1-0-0 {:historic-population historic-population
                                       :extra-population extra-population}
                                      {:projection-start-year 2017})
          starting-population (:current-population (select-starting-population-1-0-0
                                                    {:total-population total-population
                                                     :current-year-in-loop 2016}))
          transition-matrix (reduce-input :transition-matrix :age 6)
          {:keys [current-population]} (apply-state-changes-1-0-0
                                        {:current-population starting-population
                                         :transition-matrix transition-matrix
                                         :total-population total-population
                                         :current-year-in-loop 2016})]
      (is (every? #(= % 2017) (ds/column current-population :year)))))
  (testing "The probabilities are applied correctly. Here we modify the transition matrix for
            all the individuals age 0 non-send to transition to asd-mainstream at age 1."
    (let [{:keys [historic-population]}
          (get-historic-population-1-0-0 {:historic-0-25-population
                                          (reduce-input :historic-0-25-population :age 6)
                                          :historic-send-population
                                          (reduce-input :historic-send-population :age 6)}
                                         {:projection-start-year 2017
                                          :number-of-simulations 1})
          {:keys [extra-population]}
          (population-change-1-0-0 {:historic-0-25-population
                                    (reduce-input :historic-0-25-population :age 6)
                                    :population-projection
                                    (reduce-input :population-projection :age 6)}
                                   {:projection-start-year 2017
                                    :projection-end-year 2019
                                    :number-of-simulations 1})
          {:keys [total-population]}
          (add-extra-population-1-0-0 {:historic-population historic-population
                                       :extra-population extra-population}
                                      {:projection-start-year 2017})
          starting-population (:current-population (select-starting-population-1-0-0
                                                    {:total-population total-population
                                                     :current-year-in-loop 2016}))
          transition-matrix (modify-trans-matrix (reduce-input :transition-matrix :age 6))
          {:keys [current-population]} (apply-state-changes-1-0-0
                                        {:current-population starting-population
                                         :transition-matrix transition-matrix
                                         :total-population total-population
                                         :current-year-in-loop 2016})]
      (is (every? #(= :ASD-Mainstream (:state %))
                  (ds/row-maps (wds/select-from-ds current-population {:age {:eq 1}})))))))

(deftest append-to-total-population-1-0-0-test
  (testing "The newly adjusted individuals are added to the total population"
    (let [current-year-in-loop 2016
          {:keys [historic-population]}
          (get-historic-population-1-0-0 {:historic-0-25-population
                                          (reduce-input :historic-0-25-population :age 6)
                                          :historic-send-population
                                          (reduce-input :historic-send-population :age 6)}
                                         {:projection-start-year 2017
                                          :number-of-simulations 1})
          {:keys [extra-population]}
          (population-change-1-0-0 {:historic-0-25-population
                                    (reduce-input :historic-0-25-population :age 6)
                                    :population-projection
                                    (reduce-input :population-projection :age 6)}
                                   {:projection-start-year 2017
                                    :projection-end-year 2019
                                    :number-of-simulations 1})
          {:keys [total-population]}
          (add-extra-population-1-0-0 {:historic-population historic-population
                                       :extra-population extra-population}
                                      {:projection-start-year 2017})
          starting-population (:current-population (select-starting-population-1-0-0
                                                    {:total-population total-population
                                                     :current-year-in-loop
                                                     current-year-in-loop}))
          transition-matrix (reduce-input :transition-matrix :age 6)
          {:keys [current-population]} (apply-state-changes-1-0-0
                                        {:current-population starting-population
                                         :transition-matrix transition-matrix
                                         :total-population total-population
                                         :current-year-in-loop current-year-in-loop})
          [new-total-population new-current-year-in-loop]
          ((juxt :total-population
                 :current-year-in-loop) (append-to-total-population-1-0-0
                                         {:total-population total-population
                                          :current-population current-population
                                          :current-year-in-loop current-year-in-loop}))]
      (is (= (inc current-year-in-loop) new-current-year-in-loop))
      (is (= (first (:shape new-total-population))
             (+ (first (:shape current-population))
                (first (:shape total-population)))))
      ;; assumes that prior to appending, all individuals from
      ;; new-current-year-in-loop would be :Non-SEND.
      ;; Post-append, this will no longer be true
      (is (not (every? #(= :Non-SEND %)
                       (-> new-total-population
                           (wds/select-from-ds {:year {:eq new-current-year-in-loop}})
                           (ds/column :state))))))))

(deftest group-send-projection-test
  (let [current-year-in-loop 2016
        {:keys [historic-population]}
        (get-historic-population-1-0-0 {:historic-0-25-population
                                        (reduce-input :historic-0-25-population :age 3)
                                        :historic-send-population
                                        (reduce-input :historic-send-population :age 3)}
                                       {:projection-start-year 2017
                                        :number-of-simulations 2})
        {:keys [extra-population]}
        (population-change-1-0-0 {:historic-0-25-population
                                  (reduce-input :historic-0-25-population :age 3)
                                  :population-projection
                                  (reduce-input :population-projection :age 3)}
                                 {:projection-start-year 2017
                                  :projection-end-year 2019
                                  :number-of-simulations 2})
        {:keys [total-population]}
        (add-extra-population-1-0-0 {:historic-population historic-population
                                     :extra-population extra-population}
                                    {:projection-start-year 2017})
        starting-population (:current-population (select-starting-population-1-0-0
                                                  {:total-population total-population
                                                   :current-year-in-loop
                                                   current-year-in-loop}))
        transition-matrix (reduce-input :transition-matrix :age 3)
        {:keys [current-population]} (apply-state-changes-1-0-0
                                      {:current-population starting-population
                                       :transition-matrix transition-matrix
                                       :total-population total-population
                                       :current-year-in-loop current-year-in-loop})
        [new-total-population new-current-year-in-loop]
        ((juxt :total-population
               :current-year-in-loop) (append-to-total-population-1-0-0
                                       {:total-population total-population
                                        :current-population current-population
                                        :current-year-in-loop current-year-in-loop}))
        transformed-popn (:send-projection (group-send-projection new-total-population))]
    (testing "The projections are grouped by age, need and placement"
      (is (some #{:need} (:column-names transformed-popn)))
      (is (some #{:placement} (:column-names transformed-popn)))
      (is (some #{:low-ci} (:column-names transformed-popn)))
      (is (some #{:high-ci} (:column-names transformed-popn)))
      (is (some #{:average-population} (:column-names transformed-popn)))
      (is (some #{:year} (:column-names transformed-popn)))
      (is (some #{:age} (:column-names transformed-popn))))
    (testing "Average and confidence interval are calculated over the number of simulations")))

(deftest apply-costs-test
  (testing "cost profile is used to calculate send costs projections"
    (let [current-year-in-loop 2016
          {:keys [historic-population]}
          (get-historic-population-1-0-0 {:historic-0-25-population
                                          (reduce-input :historic-0-25-population :age 3)
                                          :historic-send-population
                                          (reduce-input :historic-send-population :age 3)}
                                         {:projection-start-year 2017
                                          :number-of-simulations 2})
          {:keys [extra-population]}
          (population-change-1-0-0 {:historic-0-25-population
                                    (reduce-input :historic-0-25-population :age 3)
                                    :population-projection
                                    (reduce-input :population-projection :age 3)}
                                   {:projection-start-year 2017
                                    :projection-end-year 2019
                                    :number-of-simulations 2})
          {:keys [total-population]}
          (add-extra-population-1-0-0 {:historic-population historic-population
                                       :extra-population extra-population}
                                      {:projection-start-year 2017})
          starting-population (:current-population (select-starting-population-1-0-0
                                                    {:total-population total-population
                                                     :current-year-in-loop
                                                     current-year-in-loop}))
          transition-matrix (reduce-input :transition-matrix :age 3)
          {:keys [current-population]} (apply-state-changes-1-0-0
                                        {:current-population starting-population
                                         :transition-matrix transition-matrix
                                         :total-population total-population
                                         :current-year-in-loop current-year-in-loop})
          [new-total-population new-current-year-in-loop]
          ((juxt :total-population
                 :current-year-in-loop) (append-to-total-population-1-0-0
                                         {:total-population total-population
                                          :current-population current-population
                                          :current-year-in-loop current-year-in-loop}))
          popn-projections (group-send-projection new-total-population)
          send-costs (get-individual-input :cost-profile)
          send-costs (:send-costs (apply-costs popn-projections send-costs))]
      (doall
       (for [{:keys [need placement low-ci high-ci average-population cost] :as row}
             (ds/row-maps send-costs)]
         (do
           (is (<= low-ci average-population high-ci))
           (cond
             (= :Non-SEND need)
             (do
               (is (= :Non-SEND placement))
               (is (= 0.0 cost)))

             (= :UO need)
             (do
               (is (= 0.0 cost)))
             :else
             (do
               (is (> cost 0.0))))))))))
