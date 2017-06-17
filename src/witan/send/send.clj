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
            [incanter.stats :as stats]
            [medley.core :as medley]))

(defn incorporate-non-send-population
  "Include in the cohorts a Non-SEND population.
  Initialise this to figures which bring the population sum for each age to equal the total population counts."
  [{:keys [send-population total-population]}]
  (let [population-by-ay (u/total-by-academic-year total-population)
        send-population-by-ay (u/total-by-academic-year send-population)
        send-cohorts (reduce (fn [coll {:keys [academic-year need setting population]}]
                               (update coll [academic-year (u/state need setting)] u/some+ population))
                             {} send-population)]
    (reduce (fn [coll age]
              (let [non-send-population (- (get population-by-ay age)
                                           (get send-population-by-ay age))]
                (assoc coll [age sc/non-send] non-send-population)))
            send-cohorts (keys send-population-by-ay))))

(defn look [arg]
  (doto arg
    (clojure.pprint/pprint)))

(defn population-deltas
  "For each projected population year, calculate the population deltas for each age assuming the whole cohort gets one year older."
  [{:keys [initial-population projected-population]}]
  (doto (->> (concat initial-population projected-population)
             (reduce (fn [coll {:keys [calendar-year academic-year population]}]
                       (assoc-in coll [calendar-year academic-year] population)) (sorted-map))
             (vals)
             (partition 2 1)
             (map (fn [[y1 y2]]
                    (u/subtract-map y2 (assoc (medley/map-keys inc y1) -5 0)))))
    clojure.pprint/pprint))

(defn incorporate-population-deltas [cohorts deltas]
  (reduce (fn [cohorts [age population]]
            (update cohorts [age sc/non-send] u/some+ population))
          cohorts deltas))

(defn next-age [age]
  (cond-> age
    (<= age 25) inc))

(defn next-year [year]
  (cond-> year
    (<= year 20) inc))

(defn update! [coll k f & args]
  (assoc! coll k (apply f (get coll k) args)))

(defn run-model-iteration [transition-probabilities cohorts population-delta]
  (let [out (-> (reduce (fn [coll [[year state :as k] population]]
                          (if (< year 21)
                            (let [alphas (get transition-probabilities k)
                                  next-states-sample (u/sample-transitions population alphas)]
                              (reduce (fn [coll [next-state count]]
                                        (cond-> coll
                                          (pos? count)
                                          (update! [(next-year year) next-state] u/some+ count)))
                                      coll next-states-sample))
                            coll))
                        (transient {}) cohorts)
                (persistent!)
                (incorporate-population-deltas population-delta))]
    (doto out
      #_clojure.pprint/pprint)))

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
   :witan/schema sc/SENDPopulation})

(definput transition-matrix-1-0-0
  {:witan/name :send/transition-matrix
   :witan/version "1.0.0"
   :witan/key :transition-matrix
   :witan/schema sc/TransitionCounts})

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
                        :initial-send-population sc/SENDPopulation
                        :transition-matrix sc/TransitionCounts
                        :projected-population sc/PopulationSYA}
   :witan/param-schema {}
   :witan/output-schema {:population-by-age-state sc/ModelState
                         :transition-alphas sc/TransitionAlphas
                         :population-deltas sc/PopulationDeltas}}
  [{:keys [initial-population initial-send-population
           transition-matrix projected-population]} _]
  {:population-by-age-state (incorporate-non-send-population {:send-population (ds/row-maps initial-send-population)
                                                              :total-population (ds/row-maps initial-population)})
   :transition-alphas (u/transition-alphas transition-matrix)
   :population-deltas (population-deltas {:initial-population (ds/row-maps initial-population)
                                          :projected-population (ds/row-maps projected-population)})})

(defworkflowfn run-send-model-1-0-0
  "Outputs the population for the last year of historic data, with one
   row for each individual/year/simulation. Also includes age & state columns"
  {:witan/name :send/run-send-model
   :witan/version "1.0.0"
   :witan/input-schema {:population-by-age-state sc/ModelState
                        :transition-alphas sc/TransitionAlphas
                        :population-deltas sc/PopulationDeltas}
   :witan/param-schema {:seed-year sc/YearSchema
                        :projection-year sc/YearSchema}
   :witan/output-schema {:send-output sc/Results}}
  [{:keys [population-by-age-state transition-alphas population-deltas]}
   {:keys [seed-year projection-year]}]
  (let [iterations (inc (- projection-year seed-year))]
    {:send-output (->> (for [simulation (range 1000)]
                         (let [projection (doall (reductions (partial run-model-iteration transition-alphas) population-by-age-state population-deltas))]
                           (println (format "Created projection %d" simulation))
                           projection))
                       (transduce identity (u/partition-rf iterations (u/merge-with-rf u/int-summary-rf)))
                       (doall))}))

(defworkflowoutput output-send-results-1-0-0
  "Groups the individual data from the loop to get a demand projection, and applies the cost profile
   to get the total cost."
  {:witan/name :send/output-send-results
   :witan/version "1.0.0"
   :witan/input-schema {:send-output sc/Results}}
  [{:keys [send-output]} _]
  (println (count send-output))
  (with-open [writer (io/writer (io/file "output-b.csv"))]
    (->> (mapcat (fn [output year]
                   (map (fn [[[academic-year state] {mean :mean {:keys [median low-ci high-ci]} :histogram}]]
                          (let [median (or median mean)
                                low-ci (or low-ci mean)
                                high-ci (or high-ci mean)]
                           (hash-map :academic-year academic-year :state state :calendar-year year :mean (round mean) :median (round median) :low-ci (round low-ci) :high-ci (round high-ci)))) output)) send-output (range 2016 3000))
         (map (juxt :academic-year :state :mean :median :low-ci :high-ci :calendar-year))
         (concat [["academic-year" "state" "mean" "median" "low ci" "high ci" "calendar-year"]])
         (csv/write-csv writer)))
  "Done")
