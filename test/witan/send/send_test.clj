(ns witan.send.send-test
  (:require [clojure.test :refer :all]
            [witan.send.send :refer :all]
            [witan.send.schemas :as sc]
            [witan.send.test-utils :as tu]))

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
