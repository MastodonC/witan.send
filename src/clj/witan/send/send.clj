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
            [witan.send.charts :as ch]
            [clojure.java.io :as io]
            [incanter.stats :as stats]
            [medley.core :as medley]
            [redux.core :as r]
            [clojure.string :as str]))

(defn initialise-model [send-data]
  (reduce (fn [coll {:keys [academic-year need setting population]}]
            (assoc coll [academic-year (states/state need setting)] population))
          {} send-data))

(defn incorporate-new-states-for-academic-year-state
  "Take a model + transitions tuple as its first argument.
  Returns a model + transitions tuple with `next-states-sample` incorporated."
  [[model transitions] academic-year state next-states-sample calendar-year]
  (vector
   (reduce (fn [coll [next-state n]]
             (cond-> coll
               (pos? n)
               (update [academic-year next-state] u/some+ n)))
           model next-states-sample)
   (reduce (fn [coll [next-state n]]
             (cond-> coll
               (pos? n)
               (update [calendar-year academic-year state next-state] u/some+ n)))
           transitions next-states-sample)))

(defn apply-leavers-movers-for-cohort-unsafe
  "We're calling this function 'unsafe' because it doesn't check whether the state or
  or academic year range is valid."
  [[model transitions] [[year state] population]
   {:keys [joiner-beta-params joiner-state-alphas
           leaver-beta-params
           mover-beta-params mover-state-alphas
           valid-year-settings] :as params}
   calendar-year]
  (if-let [probs (get mover-state-alphas [year state])]
    (let [leaver-params (get leaver-beta-params [year state])
          l (u/sample-beta-binomial population leaver-params)
          next-states-sample (if (states/can-move? valid-year-settings year state)
                               (let [mover-params (get mover-beta-params [year state])]
                                 (u/sample-send-transitions state (- population l) probs mover-params))
                               {state (- population l)})
          [model transitions] (incorporate-new-states-for-academic-year-state [model transitions] year state next-states-sample calendar-year)]
      [model
       (update transitions [calendar-year year state sc/non-send] u/some+ l)])
    [model transitions]))

(defn apply-leavers-movers-for-cohort
  "Take single cohort of users and process them into the model state.
  Calls 'unsafe' equivalent once we've removed non-send and children outside
  valid academic year range."
  [[model transitions :as model-state] [[year state] population :as cohort]
   {:keys [joiner-beta-params joiner-state-alphas
           leaver-beta-params
           mover-beta-params mover-state-alphas
           valid-year-settings] :as params}
   calendar-year]
  (cond
    (= state sc/non-send)
    model-state

    (or (<= year sc/min-academic-year)
        (> year sc/max-academic-year))
    [model
     (cond-> transitions
       (pos? population)
       (update [calendar-year year state sc/non-send] u/some+ population))]
    :else
    (apply-leavers-movers-for-cohort-unsafe model-state cohort params calendar-year)))

(defn apply-joiners-for-academic-year
  [[model transitions] academic-year population {:keys [joiner-beta-params joiner-state-alphas]} calendar-year]
  (let [betas (get joiner-beta-params academic-year)
        alphas (get joiner-state-alphas academic-year)
        pop (get population academic-year)]
    (if (and alphas betas pop (every? pos? (vals betas)))
      (let [joiners (u/sample-beta-binomial pop betas)]
        (if (zero? joiners)
          [model transitions]
          (let [joiner-states (u/sample-dirichlet-multinomial joiners alphas)]
            (incorporate-new-states-for-academic-year-state [model transitions] academic-year sc/non-send joiner-states calendar-year))))
      [model transitions])))

(defn run-model-iteration
  "Takes the model & transitions, transition params, and the projected population and produce the next state of the model & transitions"
  [simulation {:keys [joiner-beta-params joiner-state-alphas
                      leaver-beta-params
                      mover-beta-params mover-state-alphas
                      valid-year-settings] :as params}
   {:keys [model transitions]} [calendar-year projected-population]]
  (let [cohorts (step/age-population projected-population model)
        [model transitions] (reduce (fn [model-state cohort]
                                      (apply-leavers-movers-for-cohort model-state cohort params calendar-year))
                                    [{} {}]
                                    cohorts)
        [model transitions] (reduce (fn [model-state academic-year]
                                      (apply-joiners-for-academic-year model-state academic-year projected-population params calendar-year))
                                    [model transitions]
                                    sc/academic-years)]
    (println "Completed transitions....")
    {:model model :transitions transitions}))

;; Workflow functions

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

(definput population-1-0-0
  {:witan/name :send/population
   :witan/version "1.0.0"
   :witan/key :population
   :witan/schema sc/PopulationDataset})

(definput setting-cost-1-0-0
  {:witan/name :send/setting-cost
   :witan/version "1.0.0"
   :witan/key :setting-cost
   :witan/schema sc/NeedSettingCost})

(definput valid-setting-academic-years-1-0-0
  {:witan/name :send/valid-setting-academic-years
   :witan/version "1.0.0"
   :witan/key :valid-setting-academic-years
   :witan/schema sc/ValidSettingAcademicYears})

(defn stitch-ay-state-params
  [x a b]
  (reduce
   (fn [coll [[ay state] v]]
     (cond-> coll
       (>= ay x)
       (assoc [ay state] v)))
   (reduce (fn [coll [[ay state] v]]
             (cond-> coll
               (< ay x)
               (assoc [ay state] v)))
           {} a)
   b))

(defn stitch-ay-params
  [x a b]
  (reduce
   (fn [coll [ay v]]
     (cond-> coll
       (>= ay x)
       (assoc ay v)))
   (reduce (fn [coll [ay v]]
             (cond-> coll
               (< ay x)
               (assoc ay v)))
           {} a)
   b))

(defworkflowfn prepare-send-inputs-1-0-0
  "Outputs the population for the last year of historic data, with one
   row for each individual/year/simulation. Also includes age & state columns"
  {:witan/name :send/prepare-send-inputs
   :witan/version "1.0.0"
   :witan/input-schema {:population sc/PopulationDataset
                        :initial-send-population sc/SENDPopulation
                        :transition-matrix sc/TransitionCounts
                        :setting-cost sc/NeedSettingCost
                        :valid-setting-academic-years sc/ValidSettingAcademicYears}
   :witan/param-schema {}
   :witan/output-schema {:population-by-age-state sc/ModelState
                         :projected-population sc/PopulationByCalendarAndAcademicYear
                         :joiner-beta-params sc/JoinerBetaParams
                         :leaver-beta-params sc/YearStateBetaParams
                         :joiner-state-alphas sc/AcademicYearStateAlphas
                         :mover-beta-params sc/YearStateBetaParams
                         :mover-state-alphas sc/TransitionAlphas
                         :setting-cost-lookup sc/SettingCostLookup
                         :valid-setting-academic-years sc/ValidSettingAcademicYears
                         :transition-matrix sc/TransitionCounts}}
  [{:keys [initial-send-population transition-matrix population
           setting-cost valid-setting-academic-years]} _]
  (let [original-transitions transition-matrix
        transition-matrix (ds/row-maps transition-matrix)
        transition-matrix-filtered (filter #(= (:calendar-year %) 2016) transition-matrix) ;;hard-coded year remove
        transitions (u/transitions-map transition-matrix)
        initial-state (initialise-model (ds/row-maps initial-send-population))

        valid-settings (->> (ds/row-maps valid-setting-academic-years)
                            (states/calculate-valid-settings-from-setting-academic-years))

        valid-needs (->> (ds/row-maps valid-setting-academic-years)
                         (states/calculate-valid-needs-from-setting-academic-years))

        valid-states (->> (ds/row-maps valid-setting-academic-years)
                          (states/calculate-valid-states-from-setting-academic-years))

        valid-year-settings (->> (ds/row-maps valid-setting-academic-years)
                                 (states/calculate-valid-year-settings-from-setting-academic-years))
        splice-ncy 11]
    (s/validate (sc/SENDPopulation+ valid-settings) initial-send-population)
    (s/validate (sc/TransitionsMap+ valid-needs valid-settings) transitions)
    (s/validate (sc/NeedSettingCost+ valid-needs valid-settings) setting-cost)
    {:population-by-age-state initial-state
     :joiner-beta-params (stitch-ay-params splice-ncy
                                           (p/beta-params-joiners valid-states
                                                                  transition-matrix
                                                                  (ds/row-maps population))
                                           (p/beta-params-joiners valid-states
                                                                  transition-matrix-filtered
                                                                  (ds/row-maps population)))
     :leaver-beta-params (stitch-ay-state-params splice-ncy
                                                 (p/beta-params-leavers valid-states transition-matrix)
                                                 (p/beta-params-leavers valid-states transition-matrix-filtered))
     :joiner-state-alphas (stitch-ay-params splice-ncy
                                            (p/alpha-params-joiner-states valid-states (u/transitions-map transition-matrix))
                                            (p/alpha-params-joiner-states valid-states (u/transitions-map transition-matrix-filtered)))
     :projected-population (->> (ds/row-maps population)
                                (group-by :calendar-year)
                                (map (fn [[k v]] [k (u/total-by-academic-year v)]))
                                (into {}))
     :mover-beta-params (stitch-ay-state-params splice-ncy
                                                (p/beta-params-movers valid-states transition-matrix)
                                                (p/beta-params-movers valid-states transition-matrix-filtered))
     :mover-state-alphas (stitch-ay-state-params splice-ncy
                                                 (p/alpha-params-movers valid-states #_valid-year-settings transition-matrix)
                                                 (p/alpha-params-movers valid-states transition-matrix-filtered))
     :setting-cost-lookup (->> (ds/row-maps setting-cost)
                               (map (juxt (juxt :need :setting) :cost))
                               (into {}))
     :valid-setting-academic-years valid-setting-academic-years
     :transition-matrix original-transitions}))

(defn projection->transitions
  [file projections]
  (let [transitions (apply merge-with + (mapcat #(map :transitions %) projections))]
    (spit file (pr-str transitions))))

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
                           :total-cost (r/pre-step (u/histogram-rf number-of-significant-digits) (comp (partial u/total-need-setting-cost setting-cost-lookup)
                                                                            u/model-population-by-need-setting))
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
                        :projected-population sc/PopulationByCalendarAndAcademicYear
                        :joiner-beta-params sc/JoinerBetaParams
                        :leaver-beta-params sc/YearStateBetaParams
                        :joiner-state-alphas sc/AcademicYearStateAlphas
                        :mover-beta-params sc/YearStateBetaParams
                        :mover-state-alphas sc/TransitionAlphas
                        :setting-cost-lookup sc/SettingCostLookup
                        :valid-setting-academic-years sc/ValidSettingAcademicYears
                        :transition-matrix sc/TransitionCounts}
   :witan/param-schema {:seed-year sc/YearSchema
                        :simulations s/Int
                        :random-seed s/Int}
   :witan/output-schema {:send-output sc/Results
                         :transition-matrix sc/TransitionCounts}}
  [{:keys [population-by-age-state projected-population joiner-beta-params joiner-state-alphas leaver-beta-params mover-beta-params mover-state-alphas setting-cost-lookup valid-setting-academic-years transition-matrix] :as inputs}
   {:keys [seed-year random-seed simulations]}]
  (u/set-seed! random-seed)
  (let [projected-future-pop-by-year (->> projected-population
                                          (filter (fn [[k _]] (> k seed-year)))
                                          (sort-by key))
        iterations (inc (count projected-future-pop-by-year)) ;; include current year
        valid-states (->> (ds/row-maps valid-setting-academic-years)
                          (states/calculate-valid-states-from-setting-academic-years))
        inputs (assoc inputs :valid-year-settings (->> (ds/row-maps valid-setting-academic-years)
                                                       (states/calculate-valid-year-settings-from-setting-academic-years)))
        projections (->> (range simulations)
                         (partition-all (int (/ simulations 8)))
                         (map (fn [simulations]
                                (->> (for [simulation simulations]
                                       (let [projection (reductions (partial run-model-iteration simulation inputs)
                                                                    {:model population-by-age-state
                                                                     :transitions {}}
                                                                    projected-future-pop-by-year)]
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
    (projection->transitions "target/transitions.edn" (apply concat projections))
    ;;    (println mover-beta-params)
    (println "Combining...")
    {:send-output (transduce identity (combine-rf iterations) reduced)
     :transition-matrix transition-matrix}))

(defworkflowoutput output-send-results-1-0-0
  "Groups the individual data from the loop to get a demand projection, and applies the cost profile
   to get the total cost."
  {:witan/name :send/output-send-results
   :witan/version "1.0.0"
   :witan/input-schema {:send-output sc/Results
                        :transition-matrix sc/TransitionCounts}}
  [{:keys [send-output transition-matrix]} _]
  (with-open [writer (io/writer (io/file "target/output-ay-state.csv"))]
    (let [columns [:calendar-year :academic-year :state :mean :std-dev :iqr :min :low-ci :q1 :median :q3 :high-ci :max]]
      (->> (mapcat (fn [output year]
                     (map (fn [[[academic-year state] stats]]
                            (-> (medley/map-vals round stats)
                                (assoc :academic-year academic-year :state state :calendar-year year))) (:by-state output))) send-output (range 2017 3000)) ;;; hard-coded year remove
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
                          (:total-in-send-by-ay-group output))) send-output (range 2017 3000)) ;;TODO hard coded years
           (map (apply juxt columns))
           (concat [(map name columns)])
           (csv/write-csv writer))))
  (ch/sankey-transitions (ds/row-maps transition-matrix) 2014 ch/Camden-setting->group) ;;hard coded year
  send-output)
