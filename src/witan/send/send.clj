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


(def min-academic-year (apply min sc/academic-years))
(def max-academic-year (apply max sc/academic-years))

(defn age-population
  [projection model-state]
  (-> (reduce (fn [coll [[year state :as k] population]]
                (cond-> coll
                  (< year max-academic-year)
                  (assoc! [(inc year) state] population)))
              (transient {})
              model-state)
      (assoc! [min-academic-year sc/non-send] (get projection min-academic-year))
      (persistent!)))

(defn reconcile-to-projection
  [projection model-state]
  (let [age-counts (reduce (fn [coll [[year state] population]]
                             (update coll year u/some+ population))
                           {} model-state)]
    (doto (reduce (fn [coll [year population]]
                    (update coll [year sc/non-send] u/some+ (- population (get age-counts year 0))))
                  model-state
                  projection)
      #_clojure.pprint/pprint)))

(defn initialise-model [send-data]
  (reduce (fn [coll {:keys [academic-year need setting population]}]
            (assoc coll [academic-year (u/state need setting)] population))
          {} send-data))

(defn format-projection [projection]
  (reduce (fn [coll {:keys [academic-year population]}]
            (assoc coll academic-year population))
          {} projection))

(defn update! [coll k f & args]
  (assoc! coll k (apply f (get coll k) args)))

(defn run-model-iteration [simulation {:keys [joiner-beta-params joiner-state-alphas joiner-age-alphas
                                              leaver-beta-params
                                              mover-beta-params mover-state-alphas]}
                           model-state projected-population]
  (let [[state leavers variance] (->> model-state
                                      (age-population projected-population)
                                      (reduce (fn [[coll leavers variance] [[year state :as k] population]]
                                                (cond (or (<= year min-academic-year)
                                                          (> year max-academic-year))
                                                      (do #_(let [[need setting] (u/need-setting state)]
                                                            (when (= setting :FEC)
                                                              (prn {:year year :expire population})))
                                                          [coll leavers variance])
                                                      :else
                                                      (if-let [probs (get mover-state-alphas [year state])]
                                                        (let [leaver-params (get leaver-beta-params [year state])
                                                              mover-params (get mover-beta-params [year state])
                                                              l (u/sample-beta-binomial population leaver-params)
                                                              v (u/beta-binomial-variance population leaver-params)
                                                              next-states-sample (u/sample-send-transitions state (- population l) probs mover-params)]
                                                          #_(let [[need setting] (u/need-setting state)]
                                                            (let [[move stay] (reduce (fn [[move stay] [state n]]
                                                                                        (let [[need setting] (u/need-setting state)]
                                                                                          (if (= setting :FEC)
                                                                                            [move (+ stay n)]
                                                                                            [(+ move n) stay])))
                                                                                      [0 0]
                                                                                      next-states-sample)]
                                                              (if (= setting :FEC)
                                                                (prn {:year year
                                                                      :leave l
                                                                      :stay stay
                                                                      :outbound move})
                                                                (prn {:year year
                                                                      :inbound stay}))))
                                                          [(reduce (fn [coll [next-state count]]
                                                                     (cond-> coll
                                                                       (pos? count)
                                                                       (update! [year next-state] u/some+ count)))
                                                                   coll next-states-sample)
                                                           (+ leavers l)
                                                           (+ variance v)])
                                                        [coll (+ leavers population) variance])))
                                              [(transient {}) 0 0]))
        projected-base (reduce (fn [n ay]
                                 (+ n (get projected-population ay 0)))
                               0 sc/academic-years)
        out (-> state
                (persistent!))

        
        in-send-count (->> (vals out)
                           (apply +))

        target-growth 19.5
        target-joiners (+ leavers target-growth)
        
        target-variance 4092
        joiner-variance (- target-variance variance)

        joiner-beta-params (u/adjust-beta-mean target-joiners projected-base joiner-beta-params)
        joiner-beta-params (u/adjust-beta-variance joiner-variance projected-base joiner-beta-params)
        
        joiners (u/sample-beta-binomial projected-base joiner-beta-params)

        _ (prn {:joiners joiners :leavers leavers})

        out (->> (doto (u/sample-dirichlet-multinomial joiners joiner-age-alphas))
                 (reduce (fn [coll [year n]]
                           (->> (u/sample-dirichlet-multinomial n joiner-state-alphas)
                                (reduce (fn [coll [state m]]
                                          (cond-> coll
                                            (pos? m)
                                            (update [year state] u/some+ m)))
                                        coll)))
                         out))

        ]
    (doto out
      #_clojure.pprint/pprint)))

#_(u/sample-beta-binomial (+ simulation i) population (get leaver-beta-params year))

#_(let [joiners (u/sample-beta-binomial (+ simulation i) population (get joiner-beta-params year))]
                                                                  (if (pos? joiners)
                                                                    (u/sample-joiners (+ simulation i) joiners joiner-state-alphas)
                                                                    {}))
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
   :witan/input-schema {:initial-population sc/PopulationSYA
                        :initial-send-population sc/SENDPopulation
                        :transition-matrix sc/TransitionCounts
                        :projected-population sc/PopulationSYA
                        :setting-cost sc/SettingCost}
   :witan/param-schema {}
   :witan/output-schema {:population-by-age-state sc/ModelState
                         :projected-population sc/PopulationByAcademicYear
                         :joiner-beta-params sc/BetaParams
                         :leaver-beta-params sc/YearStateBetaParams
                         :joiner-state-alphas sc/StateAlphas
                         :joiner-age-alphas sc/AgeAlphas
                         :mover-beta-params sc/YearStateBetaParams
                         :mover-state-alphas sc/TransitionAlphas
                         :setting-cost-lookup sc/SettingCostLookup}}
  [{:keys [initial-population initial-send-population
           transition-matrix projected-population setting-cost]} _]
  (let [y1 (->> (ds/row-maps initial-population)
                (u/total-by-academic-year))
        send-population-by-ay (-> (ds/row-maps initial-send-population)
                                  (u/total-by-academic-year))
        population-by-ay (->> (ds/row-maps projected-population)
                              (partition-by :calendar-year)
                              (map u/total-by-academic-year))
        joiner-beta-params (u/joiner-beta-params transition-matrix y1)
        
        leaver-beta-params (u/leaver-beta-params transition-matrix
                                                 send-population-by-ay)
        joiner-state-alphas (u/joiner-state-alphas transition-matrix)
        joiner-age-alphas (u/joiner-age-alphas transition-matrix)
        initial-state (initialise-model (ds/row-maps initial-send-population))
        mover-beta-params (u/mover-beta-params transition-matrix)]
    {:population-by-age-state initial-state
     :joiner-beta-params joiner-beta-params
     :leaver-beta-params leaver-beta-params
     :joiner-state-alphas joiner-state-alphas
     :joiner-age-alphas joiner-age-alphas
     :projected-population population-by-ay
     :mover-beta-params mover-beta-params
     :mover-state-alphas (u/mover-state-alphas transition-matrix)
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
                        :joiner-state-alphas sc/StateAlphas
                        :joiner-age-alphas sc/AgeAlphas
                        :mover-beta-params sc/YearStateBetaParams
                        :mover-state-alphas sc/TransitionAlphas
                        :setting-cost-lookup sc/SettingCostLookup}
   :witan/param-schema {:seed-year sc/YearSchema
                        :simulations s/Int
                        :projection-year sc/YearSchema
                        :random-seed s/Int}
   :witan/output-schema {:send-output sc/Results}}
  [{:keys [population-by-age-state projected-population joiner-beta-params joiner-state-alphas joiner-age-alphas leaver-beta-params mover-beta-params mover-state-alphas setting-cost-lookup] :as inputs}
   {:keys [seed-year projection-year random-seed simulations]}]
  (let [iterations (inc (- projection-year seed-year))]
    (u/set-seed! random-seed)
    {:send-output (->> (for [simulation (range simulations)]
                         (let [projection (doall (reductions (partial run-model-iteration simulation inputs) population-by-age-state projected-population))]
                           (println (format "Created projection %d" simulation))
                           projection))
                       (transduce identity (u/partition-rf iterations (r/fuse {:by-state (u/model-states-rf u/int-summary-rf)
                                                                               :total-in-send-by-ay (r/pre-step (u/with-keys-rf u/int-summary-rf sc/academic-years) u/model-population-by-ay)
                                                                               :total-in-send (r/pre-step u/int-summary-rf u/model-send-population)
                                                                               :total-in-send-by-need (r/pre-step (u/merge-with-rf u/int-summary-rf) u/model-population-by-need)
                                                                               :total-in-send-by-setting (r/pre-step (u/merge-with-rf u/int-summary-rf) u/model-population-by-setting)
                                                                               :total-cost (r/pre-step u/int-summary-rf (comp (partial u/total-setting-cost setting-cost-lookup)
                                                                                                                              u/model-population-by-setting))
                                                                               :total-in-send-by-ay-group (r/pre-step (u/merge-with-rf u/int-summary-rf)
                                                                                                              u/model-population-by-ay-group)})))
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
                                (assoc :academic-year academic-year :state state :calendar-year year))) (:by-state output))) send-output (range 2017 3000))
           (map (apply juxt columns))
           (concat [(map name columns)])
           (csv/write-csv writer))))
  (with-open [writer (io/writer (io/file "output-c.csv"))]
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
  (with-open [writer (io/writer (io/file "output-d.csv"))]
    (let [columns [:calendar-year :mean :std-dev :iqr :min :low-ci :q1 :median :q3 :high-ci :max]]
      (->> (map (fn [stats year]
                  (-> (medley/map-vals round stats)
                      (assoc :calendar-year year)))
                (map :total-in-send send-output) (range 2017 3000))
           (map (apply juxt columns))
           (concat [(map name columns)])
           (csv/write-csv writer))))
  (with-open [writer (io/writer (io/file "total-cost.csv"))]
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
  "Done")
