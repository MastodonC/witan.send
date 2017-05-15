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

(defn map-keys [f coll]
  (->> (map (fn [[k v]] (vector (f k) v)) coll)
       (into (empty coll))))

(def total-by-age
  (partial reduce (fn [coll {:keys [age population]}]
                    (update coll age (fnil + 0) population))
           {}))

(defn subtract-map
  "Unlike (merge-with - &args), only returns keys from the first map"
  [a b]
  (reduce (fn [coll k]
            (update coll k - (get b k)))
          a (keys a)))

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

(defn population-deltas [{:keys [initial-population projected-population]}]
  (->> (concat initial-population projected-population)
       (reduce (fn [coll {:keys [year age population]}]
                 (assoc-in coll [year age] population)) (sorted-map))
       (vals)
       (partition 2 1)
       (map (fn [[y1 y2]]
              (subtract-map y2 (assoc (map-keys inc y1) 0 0))))))

(defn incorporate-population-deltas [cohorts deltas]
  (reduce (fn [cohorts [age population]]
            (update cohorts [age :Non-SEND] (fnil + 0) population))
          cohorts deltas))

(defn run-model-iteration [transition-probabilities cohorts population-delta]
  (->> (incorporate-population-deltas cohorts population-delta)
       (reduce (fn [coll [[age state] population]]
                 (if (< age 26)
                   (let [next-states (get-in transition-probabilities [age state])]
                     (reduce (fn [coll [next-state prob]]
                               (update coll [(inc age) next-state] (fnil + 0) (* prob population)))
                             coll next-states))
                   coll))
               {})))

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

(definput projected-population-1-0-0
  {:witan/name :send/projected-population
   :witan/version "1.0.0"
   :witan/key :projected-population
   :witan/schema sc/PopulationSYA})

(defworkflowfn prepare-send-inputs-1-0-0
  "Outputs the population for the last year of historic data, with one
   row for each individual/year/simulation. Also includes age & state columns"
  {:witan/name :send/prepare-send-inputs
   :witan/version "1.0.0"
   :witan/input-schema {:initial-population sc/PopulationSYA
                        :initial-send-population sc/SENDSchemaGrouped
                        :transition-matrix sc/DataForMatrix
                        :projected-population sc/PopulationSYA}
   :witan/param-schema {}
   :witan/output-schema {:population-by-age-state sc/SENDSchema
                         :transition-probabilities sc/TransitionMatrix}
   }
  [{:keys [initial-population initial-send-population
           transition-matrix projected-population]} _]
  {:population-by-age-state (incorporate-non-send-population {:send-population (ds/row-maps initial-send-population)
                                                              :total-population (ds/row-maps initial-population)})
   :transition-probabilities (u/transition-probabilities transition-matrix)
   :population-deltas (doto (population-deltas {:initial-population (ds/row-maps initial-population)
                                                :projected-population (ds/row-maps projected-population)})
                        (clojure.pprint/pprint))})

(defworkflowfn run-send-model-1-0-0
  "Outputs the population for the last year of historic data, with one
   row for each individual/year/simulation. Also includes age & state columns"
  {:witan/name :send/run-send-model
   :witan/version "1.0.0"
   :witan/input-schema {:population-by-age-state sc/SENDSchema
                        :transition-probabilities sc/TransitionMatrix}
   :witan/param-schema {:seed-year sc/YearSchema
                        :projection-year sc/YearSchema}
   :witan/output-schema {:send-output sc/SENDSchema}}
  [{:keys [population-by-age-state transition-probabilities population-deltas]}
   {:keys [seed-year projection-year]}]
  (let [iterations (inc (- projection-year seed-year))
        runs (for [i (range 1000)]
               (reduce (partial run-model-iteration transition-probabilities) population-by-age-state population-deltas))]
    {:send-output (last runs)}))

(defworkflowoutput output-send-results-1-0-0
  "Groups the individual data from the loop to get a demand projection, and applies the cost profile
   to get the total cost."
  {:witan/name :send/output-send-results
   :witan/version "1.0.0"
   :witan/input-schema {:send-output sc/SENDSchema}}
  [{:keys [send-output]} _]
  (count send-output))
