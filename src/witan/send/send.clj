(ns witan.send.send
  (:require [witan.workspace-api :refer [defworkflowfn
                                         definput
                                         defworkflowpred
                                         defworkflowoutput]]
            [schema.core :as s]
            [witan.send.schemas :as sc]
            [clojure.core.matrix.dataset :as ds]
            [witan.datasets :as wds]
            [witan.datasets.stats :as wst]
            [witan.send.utils :as u]))

(defn state [need placement]
  (keyword (str need "-" placement)))

(def total-by-age
  (partial reduce (fn [coll {:keys [age population]}]
                    (update coll age (fnil + 0) population))
           {}))

(defn format-cohorts
  [{:keys [age need placement population]}]
  {:age age :state (state need placement) :population population})

(defn incorporate-non-send-population [{:keys [send-population total-population]}]
  (let [population-by-age (total-by-age total-population)
        send-population-by-age (total-by-age send-population)
        send-cohorts (reduce (fn [coll {:keys [age need placement population]}]
                               (update coll [age (state need placement)] (fnil + 0) population))
                             {} send-population)]
    (reduce (fn [coll age]
              (let [non-send-population (- (get population-by-age age)
                                           (get send-population-by-age age))]
                (assoc coll [age :Non-SEND] non-send-population)))
            send-cohorts (keys send-population-by-age))))

(defn run-model-iteration [transition-probabilities cohorts]
  (reduce (fn [coll [[age state] population]]
            (if (< age 26)
              (let [next-states (get-in transition-probabilities [age state])]
                (reduce (fn [coll [next-state prob]]
                          (update coll [(inc age) next-state] (fnil + 0) (* prob population)))
                        coll next-states))
              coll))
          {} cohorts))

;; Workflow functions

(definput initial-population-1-0-0
  {:witan/name :send/initial-population
   :witan/version "1.0.0"
   :witan/key :initial-population
   :witan/schema sc/PopulationSYA})

(definput initial-send-population-1-0-0
  {:witan/name :send/initial-send-population
   :witan/version "1.0.0"
   :witan/key :initial-send-population
   :witan/schema sc/SENDSchemaGrouped})

(definput transition-matrix-1-0-0
  {:witan/name :send/transition-matrix
   :witan/version "1.0.0"
   :witan/key :transition-matrix
   :witan/schema sc/DataForMatrix})

(defworkflowfn prepare-send-inputs-1-0-0
  "Outputs the population for the last year of historic data, with one
   row for each individual/year/simulation. Also includes age & state columns"
  {:witan/name :send/prepare-send-inputs
   :witan/version "1.0.0"
   :witan/input-schema {:initial-population sc/PopulationSYA
                        :initial-send-population sc/SENDSchemaGrouped
                        :transition-matrix sc/DataForMatrix}
   :witan/param-schema {}
   :witan/output-schema {:population-by-age-state sc/SENDSchema
                         :transition-probabilities sc/TransitionMatrix}
   }
  [{:keys [initial-population initial-send-population transition-matrix]} _]
  {:population-by-age-state (incorporate-non-send-population {:send-population (ds/row-maps initial-send-population)
                                                              :total-population (ds/row-maps initial-population)})
   :transition-probabilities (u/transition-probabilities transition-matrix)})

(defworkflowfn run-send-model-1-0-0
  "Outputs the population for the last year of historic data, with one
   row for each individual/year/simulation. Also includes age & state columns"
  {:witan/name :send/run-send-model
   :witan/version "1.0.0"
   :witan/input-schema {:population-by-age-state sc/SENDSchema
                        :transition-probabilities sc/TransitionMatrix}
   :witan/param-schema {:seed-year sc/YearSchema
                        :projection-year sc/YearSchema}
   :witan/output-schema {:send-output sc/SENDFloatSchema}}
  [{:keys [population-by-age-state transition-probabilities]}
   {:keys [seed-year projection-year]}]
  (let [iterations (inc (- projection-year seed-year))]
    {:send-output (last (take iterations (iterate (partial run-model-iteration transition-probabilities) population-by-age-state)))}))

(defworkflowoutput output-send-results-1-0-0
  "Groups the individual data from the loop to get a demand projection, and applies the cost profile
   to get the total cost."
  {:witan/name :send/output-send-results
   :witan/version "1.0.0"
   :witan/input-schema {:send-output sc/SENDFloatSchema}}
  [{:keys []} _]
  "Done!")
