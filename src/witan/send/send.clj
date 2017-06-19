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
            [medley.core :as medley]
            [redux.core :as r]))

(defn incorporate-non-send-population
  "Include in the cohorts a Non-SEND population.
  Initialise this to figures which bring the population sum for each age to equal the total population counts."
  [{:keys [send-population total-population]}]
  (let [population-by-ay (u/total-by-academic-year total-population)
        send-population-by-ay (u/total-by-academic-year send-population)
        model-state (reduce (fn [coll {:keys [academic-year need setting population]}]
                              (update coll [academic-year (u/state need setting)] u/some+ population))
                            {} send-population)]
    (reduce (fn [coll age]
              (let [non-send-population (- (get population-by-ay age)
                                           (get send-population-by-ay age 0))]
                (assoc coll [age sc/non-send] non-send-population)))
            model-state (keys population-by-ay))))

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

(defn run-model-iteration [simulation transition-probabilities model-state projected-population]
  (let [out (-> (->> model-state
                     (age-population projected-population)
                     (reconcile-to-projection projected-population)
                     (map vector (range)) ;; For random seed
                     (reduce (fn [coll [i [[year state :as k] population]]]
                               (if (< year 21)
                                 (let [alphas (get transition-probabilities k)
                                       next-states-sample (u/sample-transitions (+ simulation i) population alphas)]
                                   (when (and (= state sc/non-send)
                                              (= year -5))
                                     #_(println "alphas" alphas)
                                     #_(println "population" population)
                                     #_(println (get transition-probabilities k))
                                     #_(println next-states-sample)
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
                        :projection-year sc/YearSchema
                        :random-seed s/Int}
   :witan/output-schema {:send-output sc/Results}}
  [{:keys [population-by-age-state transition-alphas projected-population]}
   {:keys [seed-year projection-year]}]
  (let [iterations (inc (- projection-year seed-year))]
    {:send-output (->> (for [simulation (range 10)]
                         (let [projection (doall (reductions (partial run-model-iteration simulation transition-alphas) population-by-age-state projected-population))]
                           (println (format "Created projection %d" simulation))
                           projection))
                       (transduce identity (u/partition-rf iterations (r/fuse {:by-state (u/model-states-rf u/int-summary-rf)
                                                                               :total-in-send-by-ay (r/pre-step (u/merge-with-rf u/int-summary-rf) u/model-population-by-ay)
                                                                               :total-in-send (r/pre-step u/int-summary-rf u/model-send-population)})))
                       (doall))}))

(defworkflowoutput output-send-results-1-0-0
  "Groups the individual data from the loop to get a demand projection, and applies the cost profile
   to get the total cost."
  {:witan/name :send/output-send-results
   :witan/version "1.0.0"
   :witan/input-schema {:send-output sc/Results}}
  [{:keys [send-output]} _]
  (with-open [writer (io/writer (io/file "output-b.csv"))]
    (let [columns [:calendar-year :academic-year :state :mean :std-dev :iqr :min :low-ci :q1 :median :q3 :high-ci :max]]
      (->> (mapcat (fn [output year]
                     (map (fn [[[academic-year state] stats]]
                            (-> (medley/map-vals round stats)
                                (assoc :academic-year academic-year :state state :calendar-year year))) (:by-state output))) send-output (range 2016 3000))
           (map (apply juxt columns))
           (concat [(map name columns)])
           (csv/write-csv writer))))
  (with-open [writer (io/writer (io/file "output-c.csv"))]
    (let [columns [:calendar-year :academic-year :mean :std-dev :iqr :min :low-ci :q1 :median :q3 :high-ci :max]]
      (->> (mapcat (fn [output year]
                     (map (fn [[academic-year stats]]
                            (-> (medley/map-vals round stats)
                                (assoc :academic-year academic-year :calendar-year year)))
                          (:total-in-send-by-ay output))) send-output (range 2016 3000))
           (map (apply juxt columns))
           (concat [(map name columns)])
           (csv/write-csv writer))))
  (with-open [writer (io/writer (io/file "output-d.csv"))]
    (let [columns [:calendar-year :mean :std-dev :iqr :min :low-ci :q1 :median :q3 :high-ci :max]]
      (->> (map (fn [stats year]
                  (-> (medley/map-vals round stats)
                      (assoc :calendar-year year)))
                (map :total-in-send send-output) (range 2016 3000))
           (map (apply juxt columns))
           (concat [(map name columns)])
           (csv/write-csv writer))))
  "Done")
