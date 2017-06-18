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

(defn next-age [age]
  (cond-> age
    (<= age 25) inc))

(defn next-year [year]
  (cond-> year
    (<= year 20) inc))

(defn update! [coll k f & args]
  (assoc! coll k (apply f (get coll k) args)))

(defn age-population
  [projection model-state]
  (-> (reduce (fn [coll [[year state :as k] population]]
                (assoc! coll [(next-year year) state] population))
              (transient {})
              model-state)
      (assoc! [-5 sc/non-send] (get projection -5))
      (persistent!)))

(defn reconcile-to-projection
  [projection model-state]
  (let [age-counts (reduce (fn [coll [[year state] population]]
                             (update coll year u/some+ population))
                           {} model-state)]
    (reduce (fn [coll [[year state] population]]
              (cond-> coll
                (= state sc/non-send)
                (update [year state] + (- (get projection year)
                                          (get age-counts year)))))
            model-state
            model-state)))

(defn run-model-iteration [transition-probabilities model-state projected-population]
  (println "Projected population" projected-population)
  (let [out (-> (->> model-state
                     (age-population projected-population)
                     (reconcile-to-projection projected-population)
                     (reduce (fn [coll [[year state :as k] population]]
                               (if (< year 21)
                                 (let [alphas (get transition-probabilities k)
                                       next-states-sample (u/sample-transitions population alphas)]
                                   (when (= state sc/non-send)
                                     #_(println "alphas" alphas)
                                     (println "population" population)
                                     (println "non-send" (get next-states-sample :NON-SEND))
                                     #_(println "states" next-states-sample))
                                   (reduce (fn [coll [next-state count]]
                                             (cond-> coll
                                               (pos? count)
                                               (update! [year next-state] u/some+ count)))
                                           coll next-states-sample))
                                 coll))
                             (transient {})))
                (persistent!))]
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
                         :projected-population sc/PopulationByAcademicYear}}
  [{:keys [initial-population initial-send-population
           transition-matrix projected-population]} _]
  (let [y1 (->> (ds/row-maps initial-population)
                (u/total-by-academic-year))
        y2 (->> (ds/row-maps projected-population)
                (filter #(= (:calendar-year %) 2018))
                (u/total-by-academic-year))
        population-by-ay (->> (ds/row-maps projected-population)
                              (partition-by :calendar-year)
                              (map #(into {} (map (juxt :academic-year :population) %))))
        initial-state (incorporate-non-send-population {:send-population (ds/row-maps initial-send-population)
                                                        :total-population (ds/row-maps initial-population)})]
    {:population-by-age-state initial-state
     :transition-alphas (u/transition-alphas transition-matrix y1 y2)
     :projected-population population-by-ay}))

(defworkflowfn run-send-model-1-0-0
  "Outputs the population for the last year of historic data, with one
   row for each individual/year/simulation. Also includes age & state columns"
  {:witan/name :send/run-send-model
   :witan/version "1.0.0"
   :witan/input-schema {:population-by-age-state sc/ModelState
                        :transition-alphas sc/TransitionAlphas
                        :projected-population sc/PopulationByAcademicYear}
   :witan/param-schema {:seed-year sc/YearSchema
                        :projection-year sc/YearSchema}
   :witan/output-schema {:send-output sc/Results}}
  [{:keys [population-by-age-state transition-alphas projected-population]}
   {:keys [seed-year projection-year]}]
  (let [iterations (inc (- projection-year seed-year))]
    {:send-output (->> (for [simulation (range 10)]
                         (let [projection (doall (reductions (partial run-model-iteration transition-alphas) population-by-age-state projected-population))]
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
  (with-open [writer (io/writer (io/file "output-b.csv"))]
    (->> (mapcat (fn [output year]
                   (println (first output))
                   (map (fn [[[academic-year state] {:keys [median low-ci high-ci mean]}]]
                          (hash-map :academic-year academic-year :state state :calendar-year year :mean (round mean) :median (round median) :low-ci (round low-ci) :high-ci (round high-ci))) output)) send-output (range 2016 3000))
         (map (juxt :academic-year :state :mean :median :low-ci :high-ci :calendar-year))
         (concat [["academic-year" "state" "mean" "median" "low ci" "high ci" "calendar-year"]])
         (csv/write-csv writer)))
  "Done")
