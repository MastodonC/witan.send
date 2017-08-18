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
            [witan.send.params :as p]
            [witan.send.step :as step]
            [witan.send.states :as state]
            [witan.send.utils :as u :refer [round]]
            [clojure.java.io :as io]
            [incanter.stats :as stats]
            [medley.core :as medley]
            [redux.core :as r]))

(defn initialise-model [send-data]
  (reduce (fn [coll {:keys [academic-year need setting population]}]
            (assoc coll [academic-year (u/state need setting)] population))
          {} send-data))

(defn run-model-iteration [simulation {:keys [joiner-beta-params joiner-state-alphas joiner-age-alphas
                                              leaver-beta-params
                                              mover-beta-params mover-state-alphas
                                              target-growth target-variance]}
                           {:keys [model transitions]} projected-population]
  (let [[model transitions leavers variance] (->> model
                                                  (step/age-population projected-population)
                                                  (reduce (fn [[model transitions leavers variance] [[year state :as k] population]]
                                                            (cond (or (<= year sc/min-academic-year)
                                                                      (> year sc/max-academic-year))
                                                                  [model
                                                                   (cond-> transitions
                                                                     (pos? population)
                                                                     (update [(dec year) state sc/non-send] u/some+ population))
                                                                   leavers variance]
                                                                  :else
                                                                  (if-let [probs (get mover-state-alphas [(dec year) state])]
                                                                    (let [leaver-params (get leaver-beta-params [year state])
                                                                          l (u/sample-beta-binomial population leaver-params)
                                                                          v (if leaver-params
                                                                              (u/beta-binomial-variance population leaver-params)
                                                                              0.0)
                                                                          next-states-sample (if (state/can-move? year state)
                                                                                               (let [mover-params (get mover-beta-params [(dec year) state])]
                                                                                                 (u/sample-send-transitions state (- population l) probs mover-params))
                                                                                               {state (- population l)})]
                                                                      [(reduce (fn [coll [next-state n]]
                                                                                 (cond-> coll
                                                                                   (pos? n)
                                                                                   (update [year next-state] u/some+ n)))
                                                                               model next-states-sample)
                                                                       (-> (reduce (fn [coll [next-state n]]
                                                                                     (cond-> coll
                                                                                       (pos? n)
                                                                                       (update [(dec year) state next-state] u/some+ n)))
                                                                                   transitions next-states-sample)
                                                                           (update [year state sc/non-send] u/some+ l))
                                                                       (+ leavers l)
                                                                       (+ variance v)])
                                                                    [model transitions (+ leavers population) variance])))
                                                          [{} transitions 0 0]))
        projected-base (reduce (fn [n ay]
                                 (+ n (get projected-population ay 0)))
                               0 sc/academic-years)

        in-send-count (->> (vals model) (apply +))

        target-joiners (+ leavers target-growth)
        joiner-variance (- target-variance variance)

        joiner-beta-params (u/adjust-beta-mean target-joiners projected-base joiner-beta-params)
        joiner-beta-params (u/adjust-beta-variance joiner-variance projected-base joiner-beta-params)
        
        joiners (u/sample-beta-binomial projected-base joiner-beta-params)
        
        [model transitions] (->> (doto (u/sample-dirichlet-multinomial joiners joiner-age-alphas))
                                 (reduce (fn [[coll transitions] [year n]]
                                           (->> (u/sample-dirichlet-multinomial n (get joiner-state-alphas year))
                                                (reduce (fn [[coll transitions] [state m]]
                                                          [(cond-> coll
                                                             (pos? m)
                                                             (update [year state] u/some+ m))
                                                           (cond-> transitions
                                                             (pos? m)
                                                             (update [(dec year) sc/non-send state] u/some+ m))])
                                                        [coll transitions])))
                                         [model transitions]))]
    {:model model :transitions transitions}))

;; Workflow functions

(definput initial-population-1-0-0
  {:witan/name :send/initial-population
   :witan/version "1.0.0"
   :witan/key :initial-population
   :witan/schema sc/PopulationDataset})

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
   :witan/schema sc/PopulationDataset})

(definput setting-cost-1-0-0
  {:witan/name :send/setting-cost
   :witan/version "1.0.0"
   :witan/key :setting-cost
   :witan/schema sc/SettingCost})

(defworkflowfn prepare-send-inputs-1-0-0
  "Outputs the population for the last year of historic data, with one
   row for each individual/year/simulation. Also includes age & state columns"
  {:witan/name :send/prepare-send-inputs
   :witan/version "1.0.0"
   :witan/input-schema {:initial-population sc/PopulationDataset
                        :projected-population sc/PopulationDataset
                        :initial-send-population sc/SENDPopulation
                        :transition-matrix sc/TransitionCounts
                        :setting-cost sc/SettingCost}
   :witan/param-schema {}
   :witan/output-schema {:population-by-age-state sc/ModelState
                         :projected-population sc/PopulationByAcademicYear
                         :joiner-beta-params sc/BetaParams
                         :leaver-beta-params sc/YearStateBetaParams
                         :joiner-state-alphas sc/AcademicYearStateAlphas
                         :joiner-age-alphas sc/AgeAlphas
                         :mover-beta-params sc/YearStateBetaParams
                         :mover-state-alphas sc/TransitionAlphas
                         :setting-cost-lookup sc/SettingCostLookup}}
  [{:keys [initial-population initial-send-population
           transition-matrix projected-population setting-cost]} _]
  (let [initial-population (->> (ds/row-maps initial-population)
                                (u/total-by-academic-year))
        transitions (u/transitions-map transition-matrix)
        leaver-beta-params (p/beta-params-leavers transitions 0.5 0.5 0.5)
        joiner-beta-params (p/beta-params-joiners transitions initial-population 0.5)
        joiner-state-alphas (p/alpha-params-joiner-states transitions 0.5 0.5 0.5)
        joiner-age-alphas (p/alpha-params-joiner-ages transitions)
        mover-beta-params (p/beta-params-movers transitions 0.5 0.5 0.5)
        mover-state-alphas (p/alpha-params-movers transitions 0.5 0.5 0.5)
        initial-state (initialise-model (ds/row-maps initial-send-population))]
    {:population-by-age-state initial-state
     :joiner-beta-params joiner-beta-params
     :leaver-beta-params leaver-beta-params
     :joiner-state-alphas joiner-state-alphas
     :joiner-age-alphas joiner-age-alphas
     :projected-population (->> (ds/row-maps projected-population)
                                (partition-by :calendar-year)
                                (map u/total-by-academic-year))
     :mover-beta-params mover-beta-params
     :mover-state-alphas mover-state-alphas
     :setting-cost-lookup (->> (ds/row-maps setting-cost)
                               (map (juxt :setting :cost))
                               (into {}))}))

(defworkflowfn run-send-model-1-0-0
  "Outputs the population for the last year of historic data, with one
   row for each individual/year/simulation. Also includes age & state columns"
  {:witan/name :send/run-send-model
   :witan/version "1.0.0"
   :witan/input-schema {:population-by-age-state sc/ModelState
                        :projected-population sc/PopulationByAcademicYear
                        :joiner-beta-params sc/BetaParams
                        :leaver-beta-params sc/YearStateBetaParams
                        :joiner-state-alphas sc/AcademicYearStateAlphas
                        :joiner-age-alphas sc/AgeAlphas
                        :mover-beta-params sc/YearStateBetaParams
                        :mover-state-alphas sc/TransitionAlphas
                        :setting-cost-lookup sc/SettingCostLookup}
   :witan/param-schema {:seed-year sc/YearSchema
                        :simulations s/Int
                        :projection-year sc/YearSchema
                        :random-seed s/Int
                        :target-growth s/Num
                        :target-variance s/Num}
   :witan/output-schema {:send-output sc/Results}}
  [{:keys [population-by-age-state projected-population joiner-beta-params joiner-state-alphas joiner-age-alphas leaver-beta-params mover-beta-params mover-state-alphas setting-cost-lookup] :as inputs}
   {:keys [seed-year projection-year random-seed simulations target-growth target-variance]}]
  (u/set-seed! random-seed)
  (let [iterations (inc (- projection-year seed-year))
        inputs (assoc inputs :target-growth target-growth :target-variance target-variance)
        projections (for [simulation (range simulations)]
                      (let [projection (doall (reductions (partial run-model-iteration simulation inputs) {:model population-by-age-state
                                                                                                           :transitions {}}
                                                          projected-population))]
                        (println (format "Created projection %d" simulation))
                        projection))
        by-setting (fn [coll]
                     (reduce (fn [coll [[ay s1 s2] n]]
                               (let [s1 (if (= s1 sc/non-send)
                                          sc/non-send
                                          (second (u/need-setting s1)))
                                     s2 (if (= s2 sc/non-send)
                                          sc/non-send
                                          (second (u/need-setting s2)))]
                                 (update coll [ay s1 s2] u/some+ n)))
                             {} coll))
        transitions (apply merge-with + (mapcat #(map (comp by-setting :transitions) %) projections))]
    (prn transitions)
    {:send-output (->> (map #(map :model %) projections)
                       (transduce identity (u/partition-rf iterations (r/fuse {:by-state (u/model-states-rf u/summary-rf)
                                                                               :total-in-send-by-ay (r/pre-step (u/with-keys-rf u/summary-rf sc/academic-years) u/model-population-by-ay)
                                                                               :total-in-send (r/pre-step u/summary-rf u/model-send-population)
                                                                               :total-in-send-by-need (r/pre-step (u/merge-with-rf u/summary-rf) u/model-population-by-need)
                                                                               :total-in-send-by-setting (r/pre-step (u/merge-with-rf u/summary-rf) u/model-population-by-setting)
                                                                               :total-cost (r/pre-step u/summary-rf (comp (partial u/total-setting-cost setting-cost-lookup)
                                                                                                                              u/model-population-by-setting))
                                                                               :total-in-send-by-ay-group (r/pre-step (u/merge-with-rf u/summary-rf)
                                                                                                                      u/model-population-by-ay-group)})))
                       (doall))}))

(defworkflowoutput output-send-results-1-0-0
  "Groups the individual data from the loop to get a demand projection, and applies the cost profile
   to get the total cost."
  {:witan/name :send/output-send-results
   :witan/version "1.0.0"
   :witan/input-schema {:send-output sc/Results}}
  [{:keys [send-output]} _]
  (with-open [writer (io/writer (io/file "output-ay-state.csv"))]
    (let [columns [:calendar-year :academic-year :state :mean :std-dev :iqr :min :low-ci :q1 :median :q3 :high-ci :max]]
      (->> (mapcat (fn [output year]
                     (map (fn [[[academic-year state] stats]]
                            (-> (medley/map-vals round stats)
                                (assoc :academic-year academic-year :state state :calendar-year year))) (:by-state output))) send-output (range 2017 3000))
           (map (apply juxt columns))
           (concat [(map name columns)])
           (csv/write-csv writer))))
  (with-open [writer (io/writer (io/file "output-ay.csv"))]
    (let [columns [:calendar-year :academic-year :mean :std-dev :iqr :min :low-ci :q1 :median :q3 :high-ci :max]]
      (->> (mapcat (fn [output year]
                     (map (fn [[academic-year stats]]
                            (-> (medley/map-vals round stats)
                                (assoc :academic-year academic-year :calendar-year year)))
                          (:total-in-send-by-ay output))) send-output (range 2017 3000))
           (map (apply juxt columns))
           (concat [(map name columns)])
           (csv/write-csv writer))))
  (with-open [writer (io/writer (io/file "output-need.csv"))]
    (let [columns [:calendar-year :need :mean :std-dev :iqr :min :low-ci :q1 :median :q3 :high-ci :max]]
      (->> (mapcat (fn [output year]
                     (map (fn [[need stats]]
                            (-> (medley/map-vals round stats)
                                (assoc :need (name need) :calendar-year year)))
                          (:total-in-send-by-need output))) send-output (range 2017 3000))
           (map (apply juxt columns))
           (concat [(map name columns)])
           (csv/write-csv writer))))
  (with-open [writer (io/writer (io/file "output-setting.csv"))]
    (let [columns [:calendar-year :setting :mean :std-dev :iqr :min :low-ci :q1 :median :q3 :high-ci :max]]
      (->> (mapcat (fn [output year]
                     (map (fn [[setting stats]]
                            (-> (medley/map-vals round stats)
                                (assoc :setting (name setting) :calendar-year year)))
                          (:total-in-send-by-setting output))) send-output (range 2017 3000))
           (map (apply juxt columns))
           (concat [(map name columns)])
           (csv/write-csv writer))))
  (with-open [writer (io/writer (io/file "output-count.csv"))]
    (let [columns [:calendar-year :mean :std-dev :iqr :min :low-ci :q1 :median :q3 :high-ci :max]]
      (->> (map (fn [stats year]
                  (-> (medley/map-vals round stats)
                      (assoc :calendar-year year)))
                (map :total-in-send send-output) (range 2017 3000))
           (map (apply juxt columns))
           (concat [(map name columns)])
           (csv/write-csv writer))))
  (with-open [writer (io/writer (io/file "output-cost.csv"))]
    (let [columns [:calendar-year :mean :std-dev :iqr :min :low-ci :q1 :median :q3 :high-ci :max]]
      (->> (map (fn [stats year]
                  (-> (medley/map-vals round stats)
                      (assoc :calendar-year year)))
                (map :total-cost send-output) (range 2017 3000))
           (map (apply juxt columns))
           (concat [(map name columns)])
           (csv/write-csv writer))))
  (with-open [writer (io/writer (io/file "output-ay-group.csv"))]
    (let [columns [:calendar-year :ay-group :mean :std-dev :iqr :min :low-ci :q1 :median :q3 :high-ci :max]]
      (->> (mapcat (fn [output year]
                     (map (fn [[ay-group stats]]
                            (-> (medley/map-vals round stats)
                                (assoc :ay-group ay-group :calendar-year year)))
                          (:total-in-send-by-ay-group output))) send-output (range 2017 3000))
           (map (apply juxt columns))
           (concat [(map name columns)])
           (csv/write-csv writer))))
  send-output)

