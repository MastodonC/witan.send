(ns witan.send.send
  (:require [witan.workspace-api :refer [defworkflowfn
                                         definput
                                         defworkflowpred
                                         defworkflowoutput]]
            [schema.core :as s]
            [witan.send.schemas :as sc]
            [clojure.core.matrix.dataset :as ds]
            [clojure.data.csv :as csv]
            [witan.datasets :as wds]
            [witan.datasets.stats :as wst]
            [witan.send.utils :as u :refer [round]]
            [clojure.java.io :as io]
            [incanter.stats :as stats]))

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

(defn incorporate-non-send-population
  "Include in the cohorts a Non-SEND population.
  Initialise this to figures which bring the population sum for each age to equal the total population counts."
  [{:keys [send-population total-population]}]
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

(defn population-deltas
  "For each projected population year, calculate the population deltas for each age assuming the whole cohort gets one year older."
  [{:keys [initial-population projected-population]}]
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
  (-> (reduce (fn [coll [[age state] population]]
                (if (< age 26)
                  (let [next-states (get transition-probabilities [age state])
                        next-states-sample (u/sample-transitions population next-states)]
                    (reduce (fn [coll [next-state count]]
                              (update coll [(inc age) next-state] (fnil + 0) count))
                            coll next-states-sample))
                  coll))
              {} cohorts)
      (incorporate-population-deltas population-delta)))

(defn calculate-confidence-intervals
  [simulations]
  (let [vectorify-keys (fn [coll]
                         (->> coll (map (fn [[k v]] (vector k (vector v))))
                              (into {})))]
    (->> (map vectorify-keys simulations)
         (apply merge-with concat)
         (map (fn [[k vs]]
                (vector k {:median (stats/median vs)
                           :mean (stats/mean vs)
                           :quantiles (stats/quantile vs :probs [0.025 0.975])})))
         (into {}))))

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
                         :transition-probabilities sc/TransitionMatrixSchema
                         :population-deltas sc/PopulationDeltas}
   }
  [{:keys [initial-population initial-send-population
           transition-matrix projected-population]} _]
  {:population-by-age-state (incorporate-non-send-population {:send-population (ds/row-maps initial-send-population)
                                                              :total-population (ds/row-maps initial-population)})
   :transition-probabilities (u/transition-probabilities transition-matrix)
   :population-deltas (population-deltas {:initial-population (ds/row-maps initial-population)
                                               :projected-population (ds/row-maps projected-population)})})

(defworkflowfn run-send-model-1-0-0
  "Outputs the population for the last year of historic data, with one
   row for each individual/year/simulation. Also includes age & state columns"
  {:witan/name :send/run-send-model
   :witan/version "1.0.0"
   :witan/input-schema {:population-by-age-state sc/SENDSchema
                        :transition-probabilities sc/TransitionMatrixSchema
                        :population-deltas sc/PopulationDeltas}
   :witan/param-schema {:seed-year sc/YearSchema
                        :projection-year sc/YearSchema}
   :witan/output-schema {:send-output sc/SENDOutputSchema1}}
  [{:keys [population-by-age-state transition-probabilities population-deltas]}
   {:keys [seed-year projection-year]}]
  (println "Count population deltas" (count population-deltas))
  (let [iterations (inc (- projection-year seed-year))
        ]
    {:send-output (->> (for [simulation (range 100)]
                         (reductions (partial run-model-iteration transition-probabilities) population-by-age-state population-deltas))
                       (apply map vector)
                       (map calculate-confidence-intervals))}))

(defworkflowoutput output-send-results-1-0-0
  "Groups the individual data from the loop to get a demand projection, and applies the cost profile
   to get the total cost."
  {:witan/name :send/output-send-results
   :witan/version "1.0.0"
   :witan/input-schema {:send-output sc/SENDOutputSchema1}}
  [{:keys [send-output]} _]
  (println (count send-output))
  (with-open [writer (io/writer (io/file "output-b.csv"))]
    (->> (mapcat (fn [output year]
                   (map (fn [[[age state] {:keys [mean median quantiles]}]]
                          (hash-map :age age :state state :year year :mean (round mean) :median (round median) :q1 (round (first quantiles)) :q3 (round (last quantiles)))) output)) send-output (range 2016 3000))
         (map (juxt :age :state :mean :median :q1 :q3 :year))
         (concat [["age" "state" "population" "year"]])
         (csv/write-csv writer)))
  "Done")
