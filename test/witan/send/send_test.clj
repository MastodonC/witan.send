(ns witan.send.send-test
  (:require [clojure.test :refer :all]
            [witan.send.send :refer :all]
            [witan.send.schemas :as sc]
            [witan.send.test-utils :as tu]
            [clojure.core.matrix.dataset :as ds]
            [witan.datasets :as wds]))

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

(def historic-0-25-population
  (get-individual-input :historic-0-25-population))

(def historic-send-population
  (get-individual-input :historic-send-population))

(def population-projection
  (get-individual-input :population-projection))

(def cost-profile
  (get-individual-input :cost-profile))

(deftest add-state-to-send-population-test
  (testing "The states are added and need, placement are removed"
    (let [send-with-states (add-state-to-send-population historic-send-population)]
      (is (= (:column-names send-with-states)
             [:year :age :state :count]))
      (is (every? keyword? (distinct (wds/subset-ds send-with-states :cols :state)))))))

(deftest add-non-send-to-send-population-test
  (testing "Creates a population with send and non-send"
    (let [send-with-states (add-state-to-send-population historic-send-population)
          popn-with-states (add-non-send-to-send-population send-with-states
                                                            historic-0-25-population)]
      (is (every? keyword? (distinct (wds/subset-ds popn-with-states :cols :state))))
      (is (some #(= :Non-SEND %) (distinct (wds/subset-ds popn-with-states :cols :state)))))))

(deftest groups-to-individuals-test
  (testing "It returns rows for each individual"
    (let [grouped-ds (ds/dataset [{:year 2014 :age 0 :state :Non-SEND :count 3}
                                  {:year 2015 :age 0 :state :Non-SEND :count 0}
                                  {:year 2016 :age 0 :state :Non-SEND :count 1}])
          grped-popn (groups-to-individuals grouped-ds :count)
          grped-popn2 (groups-to-individuals grouped-ds :count 10)]
      (is (= grped-popn
             (ds/dataset [{:year 2014 :age 0 :state :Non-SEND :id 1}
                          {:year 2014 :age 0 :state :Non-SEND :id 2}
                          {:year 2014 :age 0 :state :Non-SEND :id 3}
                          {:year 2016 :age 0 :state :Non-SEND :id 4}])))
      (is (= grped-popn2
             (ds/dataset [{:year 2014 :age 0 :state :Non-SEND :id 10}
                          {:year 2014 :age 0 :state :Non-SEND :id 11}
                          {:year 2014 :age 0 :state :Non-SEND :id 12}
                          {:year 2016 :age 0 :state :Non-SEND :id 13}]))))))

(deftest add-simulation-numbers-test
  (testing "Simulation numbers are added"
    (let [population (ds/dataset [{:year 2014 :age 0 :state :Non-SEND :id 1}
                                  {:year 2014 :age 0 :state :Non-SEND :id 2}
                                  {:year 2014 :age 0 :state :Non-SEND :id 3}
                                  {:year 2016 :age 0 :state :Non-SEND :id 4}])
          popn-with-simul-nb (add-simulation-numbers population 2)]
      (is (= popn-with-simul-nb
             (ds/dataset [{:year 2014 :age 0 :state :Non-SEND :id 1 :sim-num 1}
                          {:year 2014 :age 0 :state :Non-SEND :id 1 :sim-num 2}
                          {:year 2014 :age 0 :state :Non-SEND :id 2 :sim-num 1}
                          {:year 2014 :age 0 :state :Non-SEND :id 2 :sim-num 2}
                          {:year 2014 :age 0 :state :Non-SEND :id 3 :sim-num 1}
                          {:year 2014 :age 0 :state :Non-SEND :id 3 :sim-num 2}
                          {:year 2016 :age 0 :state :Non-SEND :id 4 :sim-num 1}
                          {:year 2016 :age 0 :state :Non-SEND :id 4 :sim-num 2}]))))))
