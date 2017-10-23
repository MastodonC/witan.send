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
            [witan.send.states :as states]
            [witan.send.utils :as u :refer [round]]
            [clojure.java.io :as io]
            [incanter.stats :as stats]
            [medley.core :as medley]
            [redux.core :as r]
            [clojure.string :as str]))

(defn initialise-model [send-data]
  (reduce (fn [coll {:keys [academic-year need setting population]}]
            (assoc coll [academic-year (states/state need setting)] population))
          {} send-data))

(defn run-model-iteration [simulation {:keys [joiner-beta-params joiner-state-alphas joiner-age-alphas
                                              leaver-beta-params
                                              mover-beta-params mover-state-alphas
                                              target-growth target-variance
                                              valid-year-settings]}
                           {:keys [model transitions]} projected-population]
  (let [[model transitions leavers variance] (->> model
                                                  (step/age-population projected-population)
                                                  (reduce (fn [[model transitions leavers variance] [[year state :as k] population]]
                                                            #_(println k)
                                                            (cond
                                                              (= state sc/non-send)
                                                              [model transitions leavers variance]

                                                              (or (<= year sc/min-academic-year)
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
                                                                      next-states-sample (if (states/can-move? valid-year-settings year state)
                                                                                           (let [mover-params (get mover-beta-params [(dec year) state])]
                                                                                             (u/sample-send-transitions state (- population l) probs mover-params))
                                                                                           {state (- population l)})]
                                                                  #_(println "Sampled next states....")
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
                                                                (do
                                                                  #_(prn "No probs for population" [k population])
                                                                  [model transitions (+ leavers population) variance]))))
                                                          [{} transitions 0 0]))
        _ (println "Completed transitions....")]
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

(definput valid-setting-academic-years-1-0-0
  {:witan/name :send/valid-setting-academic-years
   :witan/version "1.0.0"
   :witan/key :valid-setting-academic-years
   :witan/schema sc/ValidSettingAcademicYears})

(defworkflowfn prepare-send-inputs-1-0-0
  "Outputs the population for the last year of historic data, with one
   row for each individual/year/simulation. Also includes age & state columns"
  {:witan/name :send/prepare-send-inputs
   :witan/version "1.0.0"
   :witan/input-schema {:initial-population sc/PopulationDataset
                        :projected-population sc/PopulationDataset
                        :initial-send-population sc/SENDPopulation
                        :transition-matrix sc/TransitionCounts
                        :setting-cost sc/SettingCost
                        :valid-setting-academic-years sc/ValidSettingAcademicYears}
   :witan/param-schema {}
   :witan/output-schema {:population-by-age-state sc/ModelState
                         :projected-population sc/PopulationByAcademicYear
                         :joiner-beta-params sc/JoinerBetaParams
                         :leaver-beta-params sc/YearStateBetaParams
                         :joiner-state-alphas sc/AcademicYearStateAlphas
                         :joiner-age-alphas sc/AgeAlphas
                         :mover-beta-params sc/YearStateBetaParams
                         :mover-state-alphas sc/TransitionAlphas
                         :setting-cost-lookup sc/SettingCostLookup
                         :valid-setting-academic-years sc/ValidSettingAcademicYears}}
  [{:keys [initial-population initial-send-population
           transition-matrix projected-population setting-cost
           valid-setting-academic-years]} _]
  (let [initial-population (->> (ds/row-maps initial-population)
                                (u/total-by-academic-year))
        transitions (u/transitions-map transition-matrix)
        initial-state (initialise-model (ds/row-maps initial-send-population))

        valid-settings (->> (ds/row-maps valid-setting-academic-years)
                            (states/calculate-valid-settings-from-setting-academic-years))

        valid-needs (->> (ds/row-maps valid-setting-academic-years)
                         (states/calculate-valid-needs-from-setting-academic-years))

        valid-states (->> (ds/row-maps valid-setting-academic-years)
                          (states/calculate-valid-states-from-setting-academic-years))

        valid-year-settings (->> (ds/row-maps valid-setting-academic-years)
                                 (states/calculate-valid-year-settings-from-setting-academic-years))]

    #_(prn valid-states)

    (s/validate (sc/SENDPopulation+ valid-settings) initial-send-population)
    (s/validate (sc/TransitionsMap+ valid-needs valid-settings) transitions)
    (s/validate (sc/SettingCost+ valid-settings) setting-cost)
    (prn (p/alpha-params-joiner-states valid-states transitions 0.5 0.5 0.5))
    {:population-by-age-state initial-state
     :joiner-beta-params (p/beta-params-joiners transitions initial-population 0.5)
     :leaver-beta-params (p/beta-params-leavers valid-states transitions 0.5 0.5 0.5)
     :joiner-state-alphas (p/alpha-params-joiner-states valid-states transitions 0.5 0.5 0.5)
     :joiner-age-alphas (p/alpha-params-joiner-ages transitions)
     :projected-population (->> (ds/row-maps projected-population)
                                (partition-by :calendar-year)
                                (map u/total-by-academic-year))
     :mover-beta-params (p/beta-params-movers valid-states transitions 0.5 0.5 0.5)
     :mover-state-alphas (p/alpha-params-movers valid-states valid-year-settings transitions 0.5 0.5 0.5)
     :setting-cost-lookup (->> (ds/row-maps setting-cost)
                               (map (juxt :setting :cost))
                               (into {}))
     :valid-setting-academic-years valid-setting-academic-years}
    ))

(defn projection->transitions
  [projections]
  (let [by-setting (fn [coll]
                     (reduce (fn [coll [[ay s1 s2] n]]
                               (let [s1 (if (= s1 sc/non-send)
                                          sc/non-send
                                          (second (states/need-setting s1)))
                                     s2 (if (= s2 sc/non-send)
                                          sc/non-send
                                          (second (states/need-setting s2)))]
                                 (update coll [ay s1 s2] u/some+ n)))
                             {} coll))
        transitions (apply merge-with + (mapcat #(map (comp by-setting :transitions) %) projections))]
    #_(prn "Transitions:" transitions)))

(defn values-rf
  "Associate a reducing function to be used for each value of map indexed by key"
  [kvs]
  (->> (for [[k v] kvs]
         [k (r/pre-step v k)])
       (into {})
       (r/fuse)))

(def number-of-significant-digits 3)

(defn reduce-rf [iterations valid-states setting-cost-lookup]
  (u/partition-rf iterations
                  (r/fuse {:by-state (u/model-states-rf valid-states (u/histogram-rf number-of-significant-digits))
                           :total-in-send-by-ay (r/pre-step (u/with-keys-rf (u/histogram-rf number-of-significant-digits) sc/academic-years) u/model-population-by-ay)
                           :total-in-send (r/pre-step (u/histogram-rf number-of-significant-digits) u/model-send-population)
                           :total-in-send-by-need (r/pre-step (u/merge-with-rf (u/histogram-rf number-of-significant-digits)) u/model-population-by-need)
                           :total-in-send-by-setting (r/pre-step (u/merge-with-rf (u/histogram-rf number-of-significant-digits)) u/model-population-by-setting)
                           :total-cost (r/pre-step (u/histogram-rf number-of-significant-digits) (comp (partial u/total-setting-cost setting-cost-lookup)
                                                                            u/model-population-by-setting))
                           :total-in-send-by-ay-group (r/pre-step (u/merge-with-rf (u/histogram-rf number-of-significant-digits))
                                                                  u/model-population-by-ay-group)})))

(defn combine-rf [iterations]
  (u/partition-rf iterations
                  (values-rf {:by-state (u/merge-with-rf (u/histogram-combiner-rf number-of-significant-digits))
                              :total-in-send-by-ay (u/merge-with-rf (u/histogram-combiner-rf number-of-significant-digits))
                              :total-in-send (u/histogram-combiner-rf number-of-significant-digits)
                              :total-in-send-by-need (u/merge-with-rf (u/histogram-combiner-rf number-of-significant-digits))
                              :total-in-send-by-setting (u/merge-with-rf (u/histogram-combiner-rf number-of-significant-digits))
                              :total-cost (u/histogram-combiner-rf number-of-significant-digits)
                              :total-in-send-by-ay-group (u/merge-with-rf (u/histogram-combiner-rf number-of-significant-digits))})))

(defworkflowfn run-send-model-1-0-0
  "Outputs the population for the last year of historic data, with one
   row for each individual/year/simulation. Also includes age & state columns"
  {:witan/name :send/run-send-model
   :witan/version "1.0.0"
   :witan/input-schema {:population-by-age-state sc/ModelState
                        :projected-population sc/PopulationByAcademicYear
                        :joiner-beta-params sc/JoinerBetaParams
                        :leaver-beta-params sc/YearStateBetaParams
                        :joiner-state-alphas sc/AcademicYearStateAlphas
                        :joiner-age-alphas sc/AgeAlphas
                        :mover-beta-params sc/YearStateBetaParams
                        :mover-state-alphas sc/TransitionAlphas
                        :setting-cost-lookup sc/SettingCostLookup
                        :valid-setting-academic-years sc/ValidSettingAcademicYears}
   :witan/param-schema {:seed-year sc/YearSchema
                        :simulations s/Int
                        :projection-year sc/YearSchema
                        :random-seed s/Int
                        :target-growth s/Num
                        :target-variance s/Num}
   :witan/output-schema {:send-output sc/Results}}
  [{:keys [population-by-age-state projected-population joiner-beta-params joiner-state-alphas joiner-age-alphas leaver-beta-params mover-beta-params mover-state-alphas setting-cost-lookup valid-setting-academic-years] :as inputs}
   {:keys [seed-year projection-year random-seed simulations target-growth target-variance]}]
  (u/set-seed! random-seed)
  (let [iterations (inc (- projection-year seed-year))
        valid-states (->> (ds/row-maps valid-setting-academic-years)
                          (states/calculate-valid-states-from-setting-academic-years))
        inputs (assoc inputs :target-growth target-growth :target-variance target-variance
                      :valid-year-settings (->> (ds/row-maps valid-setting-academic-years)
                                                (states/calculate-valid-year-settings-from-setting-academic-years)))
        projections (->> (range simulations)
                         (partition-all (int (/ simulations 8)))
                         (map (fn [simulations]
                                (->> (for [simulation simulations]
                                       (let [projection (reductions (partial run-model-iteration simulation inputs) {:model population-by-age-state
                                                                                                                     :transitions {}}
                                                                    projected-population)]
                                         (println (format "Created projection %d" simulation))
                                         projection))
                                     (doall))))
                         (doall))
        ;; _ (println "Printing....")
        ;; _ (prn (first (first projections)))
        reduced (doall
                 (for [projection projections]
                   (do (println "Reducing...")
                       (transduce (map #(map :model %)) (reduce-rf iterations valid-states setting-cost-lookup) projection))))]
    (projection->transitions (apply concat projections))
    (println "Combining...")
    {:send-output (transduce identity (combine-rf iterations) reduced)}))

(defworkflowoutput output-send-results-1-0-0
  "Groups the individual data from the loop to get a demand projection, and applies the cost profile
   to get the total cost."
  {:witan/name :send/output-send-results
   :witan/version "1.0.0"
   :witan/input-schema {:send-output sc/Results}}
  [{:keys [send-output]} _]
  (with-open [writer (io/writer (io/file "target/output-ay-state.csv"))]
    (let [columns [:calendar-year :academic-year :state :mean :std-dev :iqr :min :low-ci :q1 :median :q3 :high-ci :max]]
      (->> (mapcat (fn [output year]
                     (map (fn [[[academic-year state] stats]]
                            (-> (medley/map-vals round stats)
                                (assoc :academic-year academic-year :state state :calendar-year year))) (:by-state output))) send-output (range 2017 3000))
           (map (apply juxt columns))
           (concat [(map name columns)])
           (csv/write-csv writer))))
  (with-open [writer (io/writer (io/file "target/output-ay.csv"))]
    (let [columns [:calendar-year :academic-year :mean :std-dev :iqr :min :low-ci :q1 :median :q3 :high-ci :max]]
      (->> (mapcat (fn [output year]
                     (map (fn [[academic-year stats]]
                            (-> (medley/map-vals round stats)
                                (assoc :academic-year academic-year :calendar-year year)))
                          (:total-in-send-by-ay output))) send-output (range 2017 3000))
           (map (apply juxt columns))
           (concat [(map name columns)])
           (csv/write-csv writer))))
  (with-open [writer (io/writer (io/file "target/output-need.csv"))]
    (let [columns [:calendar-year :need :mean :std-dev :iqr :min :low-ci :q1 :median :q3 :high-ci :max]]
      (->> (mapcat (fn [output year]
                     (map (fn [[need stats]]
                            (-> (medley/map-vals round stats)
                                (assoc :need (name need) :calendar-year year)))
                          (:total-in-send-by-need output))) send-output (range 2017 3000))
           (map (apply juxt columns))
           (concat [(map name columns)])
           (csv/write-csv writer))))
  (with-open [writer (io/writer (io/file "target/output-setting.csv"))]
    (let [columns [:calendar-year :setting :mean :std-dev :iqr :min :low-ci :q1 :median :q3 :high-ci :max]]
      (->> (mapcat (fn [output year]
                     (map (fn [[setting stats]]
                            (-> (medley/map-vals round stats)
                                (assoc :setting (name setting) :calendar-year year)))
                          (:total-in-send-by-setting output))) send-output (range 2017 3000))
           (map (apply juxt columns))
           (concat [(map name columns)])
           (csv/write-csv writer))))
  (with-open [writer (io/writer (io/file "target/output-count.csv"))]
    (let [columns [:calendar-year :mean :std-dev :iqr :min :low-ci :q1 :median :q3 :high-ci :max]]
      (->> (map (fn [stats year]
                  (-> (medley/map-vals round stats)
                      (assoc :calendar-year year)))
                (map :total-in-send send-output) (range 2017 3000))
           (map (apply juxt columns))
           (concat [(map name columns)])
           (csv/write-csv writer))))
  (with-open [writer (io/writer (io/file "target/output-cost.csv"))]
    (let [columns [:calendar-year :mean :std-dev :iqr :min :low-ci :q1 :median :q3 :high-ci :max]]
      (->> (map (fn [stats year]
                  (-> (medley/map-vals round stats)
                      (assoc :calendar-year year)))
                (map :total-cost send-output) (range 2017 3000))
           (map (apply juxt columns))
           (concat [(map name columns)])
           (csv/write-csv writer))))
  (with-open [writer (io/writer (io/file "target/output-ay-group.csv"))]
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
