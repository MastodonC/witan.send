(ns witan.send.send-test
  (:require [clojure.test :refer :all]
            [witan.send.send :refer :all]
            [witan.send.schemas :as sc]
            [witan.send.test-utils :as tu]
            [clojure.core.matrix.dataset :as ds]
            [witan.datasets :as wds]
            [criterium.core :refer [with-progress-reporting bench]]
            [clj-time.core :as t]))

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

(def grouped-data
  (ds/dataset {:year  [2014 2015 2016]
               :age   [0 0 0]
               :state [:Non-SEND :Non-SEND :Non-SEND]
               :count [3 0 1]}))

(def individuals-data-with-sims
  (ds/dataset {:year  [2014 2014 2014 2014 2014 2014 2016 2016]
               :age   [0 0 0 0 0 0 0 0]
               :state [:Non-SEND :Non-SEND :Non-SEND :Non-SEND
                       :Non-SEND :Non-SEND :Non-SEND :Non-SEND]
               :id    [1 1 2 2 3 3 4 4]
               :sim-num [1 2 1 2 1 2 1 2]}))

(def population-with-states
  (let [historic-send-population (get-individual-input :historic-send-population)
        historic-0-25-population (get-individual-input :historic-0-25-population)]
    (-> historic-send-population
        add-state-to-send-population
        (add-non-send-to-send-population historic-0-25-population))))

<<<<<<< HEAD
(defmacro is-valid-result?
  [result num-sims result-keys]
  `(do
     (is (= ~result-keys (set (keys ~result))))
     (when (~result-keys :age)
       (is (= 198372 (count (:age ~result))))
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
  [result num-sims]
  `(do
     (is (= #{:age :state :year :sim-num :id} (set (:column-names ~result))))
     (is (= 198372 (count (ds/column ~result :age))))
     (is (every? number?  (ds/column ~result :age)))
     (is (every? number?  (ds/column ~result :id)))
     (is (every? keyword? (ds/column ~result :state)))
     (is (every? #(and (number? %)
                       (<= % ~num-sims)) (ds/column ~result :sim-num)))
     (is (every? #(and (number? %)
                       (>= % ~2016)) (ds/column ~result :year)))))

(deftest grps-to-indiv-test
  (testing "Going from group data to individuals data"
    (let [num-sims 1
          {:keys [repeat-data other-cols matrix-other-cols col-freqs range-individuals]}
          (prepare-for-data-transformation
           population-with-states
           :count
           num-sims)
          result (grps-to-indiv repeat-data other-cols matrix-other-cols)]
      (is-valid-result? result num-sims #{:age :state :year}))))

(deftest add-simul-nbs-test
  (testing "Adding simulation numbers to individuals data"
    (let [num-sims 1
          {:keys [repeat-data other-cols matrix-other-cols col-freqs range-individuals]}
          (prepare-for-data-transformation
           population-with-states
           :count
           num-sims)
          indiv-data (grps-to-indiv repeat-data other-cols matrix-other-cols)
          result (add-simul-nbs indiv-data col-freqs 1)]
      (is-valid-result? result num-sims #{:age :state :year :sim-num}))))

(deftest add-ids-test
  (testing "Adding ids to each row, taking into account simulations"
    (let [num-sims 1
          {:keys [repeat-data other-cols matrix-other-cols col-freqs range-individuals]}
          (prepare-for-data-transformation
           population-with-states
           :count
           num-sims)
          indiv-data (grps-to-indiv repeat-data other-cols matrix-other-cols)
          indiv-data-with-sims (add-simul-nbs indiv-data col-freqs 1)
          result (add-ids indiv-data-with-sims 1 range-individuals)]
      (is-valid-result? result num-sims #{:age :state :year :sim-num :id}))))
=======
(deftest grps-to-indiv-test
  (testing "Going from group data to individuals data"
    (let [groups-data (wds/select-from-ds population-with-states {:count {:gt 0}})
          groups-matrix-data (:columns groups-data)
          index-col (.indexOf (:column-names population-with-states) :count)
          col-freqs (nth groups-matrix-data index-col)
          other-cols (vec (clojure.set/difference (set (:column-names groups-data))
                                                  #{:count}))
          index-other-col (mapv #(.indexOf (:column-names groups-data) %) other-cols)
          matrix-other-cols (vec (clojure.set/difference (set groups-matrix-data)
                                                         (set [col-freqs])))
          counts-individs-with-sims (map #(* 1 %) col-freqs)
          repeat-data (fn [col-data] (mapcat (fn [count val] (repeat count val))
                                             counts-individs-with-sims col-data))
          result (grps-to-indiv repeat-data other-cols matrix-other-cols)]
      (is (= '(:age :state :year) (keys result)))
      (is (= 198372 (count (:age result)))))))

(deftest add-simul-nbs-test
  (testing "Adding simulation numbers to individuals data"
    (let [groups-data (wds/select-from-ds population-with-states {:count {:gt 0}})
          groups-matrix-data (:columns groups-data)
          index-col (.indexOf (:column-names population-with-states) :count)
          col-freqs (nth groups-matrix-data index-col)
          other-cols (vec (clojure.set/difference (set (:column-names groups-data))
                                                  #{:count}))
          index-other-col (mapv #(.indexOf (:column-names groups-data) %) other-cols)
          matrix-other-cols (vec (clojure.set/difference (set groups-matrix-data)
                                                         (set [col-freqs])))
          counts-individs-with-sims (map #(* 1 %) col-freqs)
          repeat-data (fn [col-data] (mapcat (fn [count val] (repeat count val))
                                             counts-individs-with-sims col-data))
          indiv-data (grps-to-indiv repeat-data other-cols matrix-other-cols)
          result (add-simul-nbs indiv-data col-freqs 1)]
      (is (= '(:age :state :year :sim-num) (keys result)))
      (is (= 198372 (count (:age result)))))))

(deftest add-ids-test
  (testing "Adding ids to each row, taking into account simulations"
    (let [groups-data (wds/select-from-ds population-with-states {:count {:gt 0}})
          groups-matrix-data (:columns groups-data)
          index-col (.indexOf (:column-names population-with-states) :count)
          col-freqs (nth groups-matrix-data index-col)
          other-cols (vec (clojure.set/difference (set (:column-names groups-data))
                                                  #{:count}))
          index-other-col (mapv #(.indexOf (:column-names groups-data) %) other-cols)
          matrix-other-cols (vec (clojure.set/difference (set groups-matrix-data)
                                                         (set [col-freqs])))
          counts-individs-with-sims (map #(* 1 %) col-freqs)
          repeat-data (fn [col-data] (mapcat (fn [count val] (repeat count val))
                                             counts-individs-with-sims col-data))
          range-individuals (range 1 (inc (apply + col-freqs)))
          indiv-data (grps-to-indiv repeat-data other-cols matrix-other-cols)
          indiv-data-with-sims (add-simul-nbs indiv-data col-freqs 1)
          result (add-ids indiv-data-with-sims 1 range-individuals)]
      (is (= '(:age :state :year :sim-num :id) (keys result)))
      (is (= 198372 (count (:age result)))))))
>>>>>>> Debugging issues when running fn on full dataset

(deftest data-transformation-test
  (testing "Go from group data to individuals data with ids and simulation number"
    (let [transformed-data (data-transformation grouped-data :count 2)]
      (is (= (set (:column-names individuals-data-with-sims))
             (set (:column-names transformed-data))))
      (is (= (set (:columns individuals-data-with-sims))
             (set (:columns transformed-data)))))))

<<<<<<< HEAD
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
       (is-valid-result-ds? historic-population 1)))))
=======
;; (deftest get-historic-population-1-0-0-test
;;   (testing "The historic population has send states, ids and simulation numbers associated to it"
;;     (let [historic-popn (get-historic-population-1-0-0 {:historic-0-25-population
;;                                                         historic-0-25-population
;;                                                         :historic-send-population
;;                                                         historic-send-population}
;;                                                        {:projection-start-year 2017
;;                                                         :number-of-simulations 1})]
;;       (is (some :state (:column-names historic-popn)))
;;       (is (some :id (:column-names historic-popn)))
;;       (is (some :sim-num (:column-names historic-popn))))))
>>>>>>> Debugging issues when running fn on full dataset
