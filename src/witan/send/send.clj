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

(defn run-model-iteration [simulation transition-probabilities joiner-beta-params joiner-state-alphas joiner-age-alphas leaver-beta-params leaver-age-alphas mover-beta-params model-state projected-population]
  (let [[state leavers variance] (->> model-state
                                      (age-population projected-population)
                                      #_(reconcile-to-projection projected-population)
                                      (map vector (range)) ;; For random seed
                                      (reduce (fn [[coll leavers variance] [i [[year state :as k] population]]]
                                                (cond (or (<= year min-academic-year)
                                                          (> year max-academic-year))
                                                      [coll leavers variance]
                                                      :else
                                                      (if-let [probs (get transition-probabilities [(dec year) state])]
                                                        (let [leaver-params (get leaver-beta-params year)
                                                              mover-params (get mover-beta-params year)
                                                              l (u/sample-beta-binomial (+ simulation i) population leaver-params)
                                                              v (u/beta-binomial-variance population leaver-params)
                                                              next-states-sample (u/sample-send-transitions (+ simulation 2) state (- population l) probs mover-params)]
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
        
        #_[joiner-mean leaver-mean] #_(let [joiner-mean (u/beta-binomial-mean projected-base joiner-beta-params)
                                            leaver-mean (u/beta-binomial-mean in-send-count leaver-beta-params)
                                            mid (/ (+ joiner-mean leaver-mean) 2)]
                                        [(+ mid (/ target-growth 2))
                                         (- mid (/ target-growth 2))])

        joiner-beta-params (u/adjust-beta-mean target-joiners projected-base joiner-beta-params)
        joiner-beta-params (u/adjust-beta-variance joiner-variance projected-base joiner-beta-params)
        #_leaver-beta-params #_(u/adjust-beta-mean leaver-mean in-send-count leaver-beta-params)

        #_joiner-variance #_(u/beta-binomial-variance projected-base joiner-beta-params)
        #_leaver-variance #_(u/beta-binomial-variance in-send-count leaver-beta-params)
        #_variance-factor #_(/ target-variance (+  joiner-variance leaver-variance))

        #_joiner-beta-params #_(u/adjust-beta-variance (* variance-factor joiner-variance)
                                                       projected-base
                                                       joiner-beta-params)
        #_leaver-beta-params #_(u/adjust-beta-variance (* variance-factor leaver-variance)
                                                       in-send-count
                                                       leaver-beta-params)
        
        
        ;; Create a joiner / leaver pair
        ;; We want an average growth of 12 per year
        ;; And a combined variance of 730
        ;; Start by figuring out average joiners and leavers each year
        
        joiners (u/sample-beta-binomial (rand-int 1000) projected-base joiner-beta-params)

        #_leavers #_(int leaver-mean) #_(let [l (- (u/sample-beta-binomial (rand-int 1000) in-send-count leaver-beta-params) leavers)]
                                          (if (< l 0) 0 l))

        _ (prn {:joiners joiners :leavers leavers})

        out (->> (doto (u/sample-joiners (inc simulation) joiners joiner-age-alphas))
                 (reduce (fn [coll [year n]]
                           (->> (u/sample-joiners (+ simulation 2) n joiner-state-alphas)
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
                         :leaver-probabilities sc/LeaverProbabilities
                         :projected-population sc/PopulationByAcademicYear
                         :joiner-beta-params sc/BetaParams
                         :leaver-beta-params sc/AcademicYearBetaParams
                         :leaver-age-alphas sc/AgeAlphas
                         :joiner-state-alphas sc/StateAlphas
                         :joiner-age-alphas sc/AgeAlphas
                         :mover-beta-params sc/AcademicYearBetaParams}}
  [{:keys [initial-population initial-send-population
           transition-matrix projected-population]} _]
  (let [y1 (->> (ds/row-maps initial-population)
                (u/total-by-academic-year))
        y2 (->> (ds/row-maps projected-population)
                (filter #(= (:calendar-year %) 2018))
                (u/total-by-academic-year))
        send-population-by-ay (-> (ds/row-maps initial-send-population)
                                  (u/total-by-academic-year))
        population-by-ay (->> (ds/row-maps projected-population)
                              (partition-by :calendar-year)
                              (map u/total-by-academic-year))
        joiner-beta-params (u/joiner-beta-params transition-matrix y1)
        
        leaver-beta-params (u/leaver-beta-params transition-matrix
                                                 send-population-by-ay)
        leaver-age-alphas (u/leaver-age-alphas transition-matrix)
        joiner-state-alphas (u/joiner-state-alphas transition-matrix)
        joiner-age-alphas (u/joiner-age-alphas transition-matrix)
        initial-state (initialise-model (ds/row-maps initial-send-population))
        mover-beta-params (u/mover-beta-params transition-matrix)]
    {:population-by-age-state initial-state
     :transition-alphas (u/transition-alphas transition-matrix y1 y2)
     :joiner-beta-params joiner-beta-params
     :leaver-beta-params leaver-beta-params
     :leaver-probabilities (u/leaver-probabilities transition-matrix)
     :leaver-age-alphas leaver-age-alphas
     :joiner-state-alphas joiner-state-alphas
     :joiner-age-alphas joiner-age-alphas
     :projected-population population-by-ay
     :mover-beta-params mover-beta-params}))

(defworkflowfn run-send-model-1-0-0
  "Outputs the population for the last year of historic data, with one
   row for each individual/year/simulation. Also includes age & state columns"
  {:witan/name :send/run-send-model
   :witan/version "1.0.0"
   :witan/input-schema {:population-by-age-state sc/ModelState
                        :transition-alphas sc/TransitionAlphas
                        :leaver-probabilities sc/LeaverProbabilities
                        :projected-population sc/PopulationByAcademicYear
                        :joiner-beta-params sc/BetaParams
                        :leaver-beta-params sc/AcademicYearBetaParams
                        :leaver-age-alphas sc/AgeAlphas
                        :joiner-state-alphas sc/StateAlphas
                        :joiner-age-alphas sc/AgeAlphas
                        :mover-beta-params sc/AcademicYearBetaParams}
   :witan/param-schema {:seed-year sc/YearSchema
                        :projection-year sc/YearSchema
                        :random-seed s/Int}
   :witan/output-schema {:send-output sc/Results}}
  [{:keys [population-by-age-state transition-alphas leaver-probabilities projected-population joiner-beta-params leaver-beta-params leaver-age-alphas joiner-state-alphas joiner-age-alphas mover-beta-params]}
   {:keys [seed-year projection-year]}]
  (let [iterations (inc (- projection-year seed-year))]
    {:send-output (->> (for [simulation (range 10)]
                         (let [projection (doall (reductions (partial run-model-iteration simulation transition-alphas joiner-beta-params joiner-state-alphas joiner-age-alphas leaver-beta-params leaver-age-alphas mover-beta-params) population-by-age-state projected-population))]
                           (println (format "Created projection %d" simulation))
                           projection))
                       (transduce identity (u/partition-rf iterations (r/fuse {:by-state (u/model-states-rf u/int-summary-rf)
                                                                               :total-in-send-by-ay (r/pre-step (u/with-keys-rf u/int-summary-rf sc/academic-years) u/model-population-by-ay)
                                                                               :total-in-send (r/pre-step u/int-summary-rf u/model-send-population)
                                                                               :total-in-send-by-need (r/pre-step (u/merge-with-rf u/int-summary-rf) u/model-population-by-need)
                                                                               :total-in-send-by-setting (r/pre-step (u/merge-with-rf u/int-summary-rf) u/model-population-by-setting)})))
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
  "Done")
