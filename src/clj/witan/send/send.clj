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
            [clojure.string :as str]
            [clojure.java.shell :as sh]
            [witan.send.report :as report])
  (:import [org.apache.commons.math3.distribution BetaDistribution]))

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
           leaver-beta-params mover-beta-params
           mover-state-alphas
           valid-year-settings] :as params}
   calendar-year]
  (if-let [probs (get mover-state-alphas [(dec year) state])]
    (let [leaver-params (get leaver-beta-params [(dec year) state])
          l (u/sample-beta-binomial population leaver-params)
          next-states-sample (if (states/can-move? valid-year-settings year state)
                               (let [mover-params (get mover-beta-params [(dec year) state])]
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
  [modify-transitions-from
   simulation {:keys [joiner-beta-params joiner-state-alphas
                      leaver-beta-params
                      mover-beta-params mover-state-alphas
                      valid-year-settings] :as standard-projection}
   {:keys [modified-joiner-beta-params modified-joiner-state-alphas
           modified-leaver-beta-params modified-mover-beta-params
           modified-mover-state-alphas] :as scenario-projection}
   {:keys [model transitions]} [calendar-year projected-population]]
  (let [params (if (nil? modify-transitions-from)
                 (if ((complement nil?) scenario-projection)
                   scenario-projection
                   standard-projection)
                 (if (>= calendar-year modify-transitions-from)
                   scenario-projection
                   standard-projection))
        cohorts (step/age-population projected-population model)
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

(definput settings-to-change-1-0-0
  {:witan/name :send/settings-to-change
   :witan/version "1.0.0"
   :witan/key :settings-to-change
   :witan/schema sc/SettingsToChange})

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

(defn generate-transition-key [cy ay need setting]
  (vector cy ay :NONSEND (states/state need setting)))

(defn update-ifelse-assoc [m k arithmetic-fn v]
  (if (contains? m k)
    (update m k #(arithmetic-fn % v))
    (assoc m k v)))

(defn modify-transitions [transitions [first-state second-state] arithmetic-fn v]
  (if (contains? transitions first-state)
    (let [pop (get transitions first-state)
          mod-pop (u/int-ceil (arithmetic-fn pop v))
          diff (- pop mod-pop)
          assoc-first-state (assoc transitions first-state mod-pop)]
      (if (nil? second-state)
        assoc-first-state
        (update-ifelse-assoc assoc-first-state second-state + diff)))
    transitions))

(defn prep-inputs [initial-state splice-ncy valid-states transition-matrix transition-matrix-filtered
                   population valid-setting-academic-years original-transitions setting-cost
                   filter-transitions-from]
  (let [start-map {:population-by-age-state initial-state
                   :valid-setting-academic-years valid-setting-academic-years
                   :transition-matrix original-transitions
                   :population population
                   :setting-cost-lookup (->> (ds/row-maps setting-cost)
                                             (map (juxt (juxt :need :setting) :cost))
                                             (into {}))
                   :projected-population (->> (ds/row-maps population)
                                              (group-by :calendar-year)
                                              (medley/map-vals #(u/total-by-academic-year %)))}]
    (if transition-matrix-filtered
      (merge start-map
             {:joiner-beta-params (stitch-ay-params splice-ncy
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

              :mover-beta-params (stitch-ay-state-params splice-ncy
                                                         (p/beta-params-movers valid-states transition-matrix)
                                                         (p/beta-params-movers valid-states transition-matrix-filtered))
              :mover-state-alphas (stitch-ay-state-params splice-ncy
                                                          (p/alpha-params-movers valid-states transition-matrix)
                                                          (p/alpha-params-movers valid-states transition-matrix-filtered))})
      (merge start-map
             {:joiner-beta-params (p/beta-params-joiners valid-states
                                                         transition-matrix
                                                         (ds/row-maps population))
              :leaver-beta-params (p/beta-params-leavers valid-states transition-matrix)
              :joiner-state-alphas (p/alpha-params-joiner-states valid-states (u/transitions-map transition-matrix))
              :mover-beta-params (p/beta-params-movers valid-states transition-matrix)
              :mover-state-alphas  (p/alpha-params-movers valid-states transition-matrix)}))))

(defn build-states-to-change [settings-to-change valid-needs ages years]
  (if (= :nil (-> settings-to-change
                  ds/row-maps
                  first
                  :setting-2))
    (let [states (->> settings-to-change
                      ds/row-maps
                      (map #(vector (:setting-1 %))))]
      (vec (mapcat (fn [year]
                     (mapcat (fn [age]
                               (mapcat (fn [need]
                                         (map (fn [setting] (vector (generate-transition-key year age need (first setting))))
                                              states)) valid-needs)) ages)) years)))
    (let [state-pairs (->> settings-to-change
                           ds/row-maps
                           (map #(vector (:setting-1 %) (:setting-2 %))))]
      (vec (mapcat (fn [year]
                     (mapcat (fn [age]
                               (mapcat (fn [need]
                                         (map (fn [setting] (vector (generate-transition-key year age need (first setting))
                                                                    (generate-transition-key year age need (second setting))))
                                              state-pairs)) valid-needs)) ages)) years)))))

(defworkflowfn prepare-send-inputs-1-0-0
  "Outputs the population for the last year of historic data, with one
   row for each individual/year/simulation. Also includes age & state columns"
  {:witan/name :send/prepare-send-inputs
   :witan/version "1.0.0"
   :witan/input-schema {:settings-to-change sc/SettingsToChange
                        :population sc/PopulationDataset
                        :initial-send-population sc/SENDPopulation
                        :transition-matrix sc/TransitionCounts
                        :setting-cost sc/NeedSettingCost
                        :valid-setting-academic-years sc/ValidSettingAcademicYears}
   :witan/param-schema {:modify-transition-by s/Num
                        :splice-ncy sc/AcademicYear
                        :filter-transitions-from (s/maybe [sc/CalendarYear])}
   :witan/output-schema {:standard-projection sc/projection-map
                         :scenario-projection (s/maybe sc/projection-map)
                         :modify-transition-by s/Num
                         :settings-to-change sc/SettingsToChange}}
  [{:keys [settings-to-change initial-send-population transition-matrix population
           setting-cost valid-setting-academic-years]}
   {:keys [modify-transition-by splice-ncy filter-transitions-from]}]
  (let [original-transitions transition-matrix
        ages (distinct (map :academic-year (ds/row-maps population)))
        years (distinct (map :calendar-year (ds/row-maps population)))
        valid-needs (->> (ds/row-maps valid-setting-academic-years)
                         (states/calculate-valid-needs-from-setting-academic-years))
        _ (when (not= 1 modify-transition-by)
            (prn (str "Modifying transitions by " modify-transition-by)))
        states-to-change (when (not= 1 modify-transition-by)
                           (build-states-to-change settings-to-change valid-needs ages years))
        transition-matrix (ds/row-maps transition-matrix)
        modified-transition-matrix (when (not= 1 modify-transition-by)
                                     (let [convert (-> transition-matrix
                                                       u/full-transitions-map)
                                           result (reduce (fn [m k] (modify-transitions m k * modify-transition-by)) convert states-to-change)]
                                       (mapcat (fn [[k v]] (u/back-to-transitions-matrix k v)) result)))
        _ (if (nil? modified-transition-matrix)
            (prn "Using input transitions matrix")
            (prn "Using modified transitions matrix"))
        transitions (if (nil? modified-transition-matrix)
                      (u/transitions-map transition-matrix)
                      (u/transitions-map modified-transition-matrix))
        transition-matrix-filtered (when filter-transitions-from
                                     (mapcat (fn [year] (filter #(= (:calendar-year %) year) (or modified-transition-matrix transition-matrix))) filter-transitions-from))

        initial-state (initialise-model (ds/row-maps initial-send-population))

        valid-settings (->> (ds/row-maps valid-setting-academic-years)
                            (states/calculate-valid-settings-from-setting-academic-years))

        valid-states (->> (ds/row-maps valid-setting-academic-years)
                          (states/calculate-valid-states-from-setting-academic-years))

        valid-year-settings (->> (ds/row-maps valid-setting-academic-years)
                                 (states/calculate-valid-year-settings-from-setting-academic-years))]

    (when (not= 1 modify-transition-by)
      (report/info "Modified transitions by " modify-transition-by))
    (if (nil? modified-transition-matrix)
      (report/info "Used input transitions matrix\n")
      (report/info "Used modified transitions matrix\n"))

    (s/validate (sc/SENDPopulation+ valid-settings) initial-send-population)
    (s/validate (sc/TransitionsMap+ valid-needs valid-settings) transitions)
    (s/validate (sc/NeedSettingCost+ valid-needs valid-settings) setting-cost)
    {:standard-projection (prep-inputs initial-state splice-ncy valid-states transition-matrix transition-matrix-filtered
                                       population valid-setting-academic-years original-transitions setting-cost
                                       filter-transitions-from)
     :scenario-projection (when modified-transition-matrix
                            (prep-inputs initial-state splice-ncy valid-states modified-transition-matrix
                                         transition-matrix-filtered population valid-setting-academic-years
                                         original-transitions setting-cost filter-transitions-from))
     :modify-transition-by modify-transition-by
     :settings-to-change settings-to-change}))

(defn projection->transitions
  [projections]
  (apply merge-with + (mapcat #(map :transitions %) projections)))

(defn output-transitions [file projections]
  (spit file (pr-str projections)))

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
   :witan/input-schema {:standard-projection sc/projection-map
                        :scenario-projection (s/maybe sc/projection-map)
                        :modify-transition-by s/Num
                        :settings-to-change sc/SettingsToChange}
   :witan/param-schema {:seed-year sc/YearSchema
                        :simulations s/Int
                        :random-seed s/Int
                        :modify-transitions-from (s/maybe sc/YearSchema)}
   :witan/output-schema {:projection sc/Projection
                         :send-output sc/Results
                         :transition-matrix sc/TransitionCounts
                         :valid-setting-academic-years sc/ValidSettingAcademicYears
                         :population sc/PopulationDataset
                         :modify-transition-by s/Num
                         :settings-to-change sc/SettingsToChange}}
  [{:keys [standard-projection scenario-projection modify-transition-by settings-to-change]}
   {:keys [seed-year random-seed simulations modify-transitions-from]}]
  (u/set-seed! random-seed)
  (let [{:keys [population population-by-age-state projected-population joiner-beta-params
                joiner-state-alphas leaver-beta-params mover-beta-params mover-state-alphas
                setting-cost-lookup valid-setting-academic-years
                transition-matrix] :as inputs} standard-projection
        modified-inputs (when ((complement nil?) scenario-projection)
                          (assoc scenario-projection :valid-year-settings
                                 (->> (ds/row-maps valid-setting-academic-years)
                                      (states/calculate-valid-year-settings-from-setting-academic-years))))
        projected-future-pop-by-year (->> projected-population
                                          (filter (fn [[k _]] (> k seed-year)))
                                          (sort-by key))
        iterations (inc (count projected-future-pop-by-year)) ;; include current year
        valid-states (->> (ds/row-maps valid-setting-academic-years)
                          (states/calculate-valid-states-from-setting-academic-years))
        inputs (assoc inputs :valid-year-settings (->> (ds/row-maps valid-setting-academic-years)
                                                       (states/calculate-valid-year-settings-from-setting-academic-years)))
        projections (->> (range simulations)
                         (partition-all (int (/ simulations 8)))
                         (pmap (fn [simulations]
                                 (->> (for [simulation simulations]
                                        (let [projection (reductions (partial run-model-iteration modify-transitions-from simulation inputs modified-inputs)
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
                       (transduce (map #(map :model %)) (reduce-rf iterations valid-states setting-cost-lookup) projection))))
        projection (apply concat projections)]
    ;;    (println mover-beta-params)
    (println "Combining...")
    {:projection (projection->transitions projection)
     :send-output (transduce identity (combine-rf iterations) reduced)
     :transition-matrix transition-matrix
     :valid-setting-academic-years valid-setting-academic-years
     :population population
     :modify-transition-by modify-transition-by
     :settings-to-change settings-to-change}))

(defn joiner-rate [joiners population ages years]
  (reduce (fn [coll academic-year]
            (reduce (fn [collection calendar-year]
                      (let [j (get-in joiners [calendar-year academic-year] 0)]
                        (assoc-in collection [academic-year calendar-year]
                                  {:alpha j
                                   :beta (- (get-in population [calendar-year academic-year]) j)})))
                    coll years)) {} ages))

(defn leaver-rate [transitions-filtered]
  (reduce (fn [coll {:keys [calendar-year academic-year-1 setting-1 setting-2]}]
            (let [leaver? (= setting-2 sc/non-send)]
              (-> coll
                  (update-in [academic-year-1 calendar-year :alpha] u/some+ (if leaver? 1 0))
                  (update-in [academic-year-1 calendar-year :beta] u/some+ (if leaver? 0 1)))))
          {} transitions-filtered))


(defn mover-rate [transitions-filtered]
  (reduce (fn [coll {:keys [calendar-year academic-year-1 setting-1 setting-2]}]
            (let [mover? (not= setting-1 setting-2)]
              (-> coll
                  (update-in [academic-year-1 calendar-year :alpha] u/some+ (if mover? 1 0))
                  (update-in [academic-year-1 calendar-year :beta] u/some+ (if mover? 0 1)))))
          {}
          transitions-filtered))

(defn confidence-interval
  [results calendar-year]
  (let [academic-years (keys results)]
    (->> (for [academic-year (sort academic-years)]
           (let [alpha (get-in results [academic-year calendar-year :alpha] 0)
                 beta (get-in results [academic-year calendar-year :beta])]
             (apply vector academic-year
                    (if (and (pos? alpha) (pos? beta))
                      [(.inverseCumulativeProbability (BetaDistribution. alpha beta) 0.025)
                       (.inverseCumulativeProbability (BetaDistribution. alpha beta) 0.975)]
                      [0 0])))))))

(defn transition-present? [transition projection]
  (some #(= % transition) projection))

(defworkflowoutput output-send-results-1-0-0
  "Groups the individual data from the loop to get a demand projection, and applies the cost profile
   to get the total cost."
  {:witan/name :send/output-send-results
   :witan/version "1.0.0"
   :witan/input-schema {:projection sc/Projection
                        :send-output sc/Results
                        :transition-matrix sc/TransitionCounts
                        :valid-setting-academic-years sc/ValidSettingAcademicYears
                        :population sc/PopulationDataset
                        :modify-transition-by s/Num
                        :settings-to-change sc/SettingsToChange}
   :witan/param-schema {:output s/Bool}}
  [{:keys [projection send-output transition-matrix valid-setting-academic-years
           population modify-transition-by settings-to-change]} {:keys [output]}]
  (let [transitions-data (ds/row-maps transition-matrix)
        transform-transitions (->> transitions-data
                                   (map #(vector
                                          (:academic-year-2 %)
                                          (states/state (:need-1 %) (:setting-1 %))
                                          (states/state (:need-2 %) (:setting-2 %))))
                                   distinct)
        transform-projection (->> projection
                                  keys
                                  (map #(vec (drop 1 %)))
                                  distinct)]
    (when (every? (fn [transition] (transition-present? transition transform-projection)) transform-transitions)
      (report/info "Not every historic transition present in projection! Consider checking valid state input.\n"))
    (when output
      (let [valid-settings (assoc (->> (ds/row-maps valid-setting-academic-years)
                                       (reduce #(assoc %1 (:setting %2) (:setting->group %2)) {}))
                                  :NON-SEND "Other" )
            years (sort (distinct (map :calendar-year transitions-data)))
            initial-projection-year (+ 1 (last years))
            joiners-count (p/calculate-joiners-per-calendar-year transitions-data)
            population-count (-> population
                                 ds/row-maps
                                 p/calculate-population-per-calendar-year)
            ages (-> population-count first val keys)
            joiner-rates (joiner-rate joiners-count population-count ages years)
            joiner-rates-CI (map #(confidence-interval joiner-rates %) years)
            filter-leavers (remove (fn [{:keys [setting-1]}] (= setting-1 sc/non-send)) transitions-data)
            leaver-rates (leaver-rate filter-leavers)
            leaver-rates-CI (map #(confidence-interval leaver-rates %) years)
            filter-movers (remove (fn [{:keys [setting-1 setting-2]}]
                                    (or (= setting-1 sc/non-send)
                                        (= setting-2 sc/non-send))) transitions-data)
            mover-rates (mover-rate filter-movers)
            mover-rates-CI (map #(confidence-interval mover-rates %) years)
            ;;n-colours (vec (repeatedly (count years) ch/random-colour)) ;; alternative random colour selection
            n-colours (take (count years) ch/palette)]
        (report/info "First year of input data: " (first years))
        (report/info "Final year of input data: " (inc (last years)))
        (report/info "Final year of projection: " (+ (last years) (count (map :total-in-send send-output))))
        (output-transitions "target/transitions.edn" projection)
        (with-open [writer (io/writer (io/file "target/output-ay-state.csv"))]
          (let [columns [:calendar-year :academic-year :state :mean :std-dev :iqr :min :low-ci :q1 :median :q3 :high-ci :max]]
            (->> (mapcat (fn [output year]
                           (map (fn [[[academic-year state] stats]]
                                  (-> (medley/map-vals round stats)
                                      (assoc :academic-year academic-year :state state :calendar-year year))) (:by-state output))) send-output (range initial-projection-year 3000))
                 (map (apply juxt columns))
                 (concat [(map name columns)])
                 (csv/write-csv writer))))
        (with-open [writer (io/writer (io/file "target/output-ay.csv"))]
          (let [columns [:calendar-year :academic-year :mean :std-dev :iqr :min :low-ci :q1 :median :q3 :high-ci :max]]
            (->> (mapcat (fn [output year]
                           (map (fn [[academic-year stats]]
                                  (-> (medley/map-vals round stats)
                                      (assoc :academic-year academic-year :calendar-year year)))
                                (:total-in-send-by-ay output))) send-output (range initial-projection-year 3000))
                 (map (apply juxt columns))
                 (concat [(map name columns)])
                 (csv/write-csv writer))))
        (with-open [writer (io/writer (io/file "target/output-need.csv"))]
          (let [columns [:calendar-year :need :mean :std-dev :iqr :min :low-ci :q1 :median :q3 :high-ci :max]]
            (->> (mapcat (fn [output year]
                           (map (fn [[need stats]]
                                  (-> (medley/map-vals round stats)
                                      (assoc :need (name need) :calendar-year year)))
                                (:total-in-send-by-need output))) send-output (range initial-projection-year 3000))
                 (map (apply juxt columns))
                 (concat [(map name columns)])
                 (csv/write-csv writer))))
        (with-open [writer (io/writer (io/file "target/output-setting.csv"))]
          (let [columns [:calendar-year :setting :mean :std-dev :iqr :min :low-ci :q1 :median :q3 :high-ci :max]]
            (->> (mapcat (fn [output year]
                           (map (fn [[setting stats]]
                                  (-> (medley/map-vals round stats)
                                      (assoc :setting (name setting) :calendar-year year)))
                                (:total-in-send-by-setting output))) send-output (range initial-projection-year 3000))
                 (map (apply juxt columns))
                 (concat [(map name columns)])
                 (csv/write-csv writer))))
        (with-open [writer (io/writer (io/file "target/output-count.csv"))]
          (let [columns [:calendar-year :mean :std-dev :iqr :min :low-ci :q1 :median :q3 :high-ci :max]]
            (->> (map (fn [stats year]
                        (-> (medley/map-vals round stats)
                            (assoc :calendar-year year)))
                      (map :total-in-send send-output) (range initial-projection-year 3000))
                 (map (apply juxt columns))
                 (concat [(map name columns)])
                 (csv/write-csv writer))))
        (with-open [writer (io/writer (io/file "target/output-cost.csv"))]
          (let [columns [:calendar-year :mean :std-dev :iqr :min :low-ci :q1 :median :q3 :high-ci :max]]
            (->> (map (fn [stats year]
                        (-> (medley/map-vals round stats)
                            (assoc :calendar-year year)))
                      (map :total-cost send-output) (range initial-projection-year 3000))
                 (map (apply juxt columns))
                 (concat [(map name columns)])
                 (csv/write-csv writer))))
        (with-open [writer (io/writer (io/file "target/output-ay-group.csv"))]
          (let [columns [:calendar-year :ay-group :mean :std-dev :iqr :min :low-ci :q1 :median :q3 :high-ci :max]]
            (->> (mapcat (fn [output year]
                           (map (fn [[ay-group stats]]
                                  (-> (medley/map-vals round stats)
                                      (assoc :ay-group ay-group :calendar-year year)))
                                (:total-in-send-by-ay-group output))) send-output (range initial-projection-year 3000))
                 (map (apply juxt columns))
                 (concat [(map name columns)])
                 (csv/write-csv writer))))
        (with-open [writer (io/writer (io/file "target/historic-data.csv"))]
          (let [send-only (filter #(not= (:setting-1 %) :NONSEND) transitions-data)
                columns [:calendar-year :setting-1 :need-1 :academic-year-1]
                headers (mapv name columns)
                rows (mapv #(mapv % columns) transitions-data)]
            (csv/write-csv writer (into [headers] rows))))
        (with-open [writer (io/writer (io/file "target/valid-settings.csv"))]
          (csv/write-csv writer valid-settings))
        (println "Producing charts...")
        (sh/sh "Rscript" "--vanilla" "send-charts.R"  :dir "src/R")
        (run! #(ch/sankey-transitions transitions-data % valid-settings) years)
        (ch/ribbon-plot joiner-rates-CI "Joiner" years n-colours)
        (ch/ribbon-plot leaver-rates-CI "Leaver" years n-colours)
        (ch/ribbon-plot mover-rates-CI "Mover" years n-colours)
        (ch/population-line-plot transitions-data (map :total-in-send send-output))
        (ch/send-cost-plot (map :total-cost send-output) years)
        (io/delete-file "target/historic-data.csv" :quiet)
        (io/delete-file "target/valid-settings.csv" :quiet)))
    (report/write-send-report))
  send-output)
