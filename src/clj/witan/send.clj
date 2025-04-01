(ns witan.send
  (:require [medley.core :as medley]
            [witan.send.config :as con]
            [witan.send.distributions :as d]
            [witan.send.maths :as m]
            [witan.send.model.prepare :as p]
            [witan.send.model.input.population :as wip]
            [witan.send.model.input.settings-to-change :as wistc]
            [witan.send.model.input.transitions :as wit]
            [witan.send.model.input.valid-states :as wivs]
            [witan.send.model.run :as r]
            [witan.send.params :as params]
            [witan.send.states :as states]))


;; FIXME: this should go into witan.send.input
(defn build-input-datasets
  "Build a map of the datasets to use for input"
  [project-dir {:keys [_costs population settings-to-change transitions valid-states]}]
  (let [input-datasets { ;; :costs (wic/csv->costs (str project-dir "/" costs))
                        :population (wip/csv->population (str project-dir "/" population))
                        :transitions (wit/csv->transitions (str project-dir "/" transitions))
                        :valid-states (wivs/csv->valid-states (str project-dir "/" valid-states))}]
    (if settings-to-change
      (assoc input-datasets :settings-to-change (wistc/csv->settings-to-change (str project-dir "/" settings-to-change))) ;; do we need this?
      input-datasets)))

(def total-by-academic-year
  "Given a sequence of {:academic-year year :population population}
  sums the total population for each year"
  (partial reduce (fn [coll {:keys [academic-year population]}]
                    (update coll academic-year m/some+ population))
           {}))

(defn transitions-map
  [dataset]
  (reduce (fn [coll {:keys [setting-1 need-1 setting-2 need-2 academic-year-2]}]
            (let [need-setting-1 (states/join-need-setting need-1 setting-1)
                  need-setting-2 (states/join-need-setting need-2 setting-2)]
              (update coll [academic-year-2 need-setting-1 need-setting-2] m/some+ 1)))
          {}
          dataset))

;; FIXME: this should go into witan.send.train
(defn train-model
  "train-model returns a map that can populate `:standard-projection`
  or `:scenario-projection` when running the model.

   - :population-by-state which is a map of [ academic-year need-setting ] population-count of the SEND population
   - :transitions a seq of maps containing
     - :calendar-year the January census point year for the *-1 side
     - :academic-year-1 :setting-1 :need-1
     - :academic-year-2 :setting-2 :need-2
   - :population a seq of
    - :calendar-year
    - :academic-year
    - :population - representing the background population for that NCY not the SEND population
   - :projected-population a map of calendar-year to maps of academic-year to background population.
     A simple transformation of :population

  Distributions for the projection
   - :joiner-beta-params The odds (alpha beta) of joining SEND my NCY
   - :joiner-state-alphas A map of academic-year to a map of need-setting odds of arriving in that need-setting
   - :mover-beta-params - The odds of a NCY/need/setting moving to another SEND setting
   - :mover-state-alphas - a map of [ NCY need-setting ] to a map of {need-setting odds of being in that need-setting}
   - :leaver-beta-params - The odds of [ NCY need-setting ] leaving SEND

  Valid transition movement data
   - :valid-transitions
   - valid-states"
  [input-datasets]
  (let [{:keys [transitions population valid-states]} input-datasets
        valid-transitions (states/calculate-valid-mover-transitions valid-states)
        validate-valid-states (states/calculate-valid-states-from-setting-academic-years valid-states)
        max-transition-year (reduce max (map :calendar-year transitions))
        initial-send-pop (p/create-initial-send-pop max-transition-year transitions)]
    {:population-by-state initial-send-pop
     :valid-transitions valid-transitions
     :valid-states valid-states
     :transitions transitions
     :population population
     :projected-population (->> population
                                (group-by :calendar-year)
                                (medley/map-vals #(total-by-academic-year %))) ;; FIXME lines 84 to 96 this is not model training, consider moving to prep inputs
     :joiner-beta-params (params/beta-params-joiners validate-valid-states
                                                     transitions
                                                     population)
     :leaver-beta-params (params/beta-params-leavers validate-valid-states transitions)
     :joiner-state-alphas (params/alpha-params-joiners validate-valid-states (transitions-map transitions))
     :mover-beta-params (params/beta-params-movers validate-valid-states valid-transitions transitions)
     :mover-state-alphas  (params/alpha-params-movers validate-valid-states valid-transitions transitions)}))

(defn seed-year [transitions]
  (inc (reduce max (map :calendar-year transitions))))

(defn create-projections-xf
  "Outputs a seq of datasets. One for each simulation.

  Each simulation is made up of
  :simulation
  :calendar-year
  :academic-year
  :need-1 :setting-1
  :need-2 :setting-2
  :transition-count"
  [{:keys [standard-projection scenario-projection modify-transition-by
           modify-transitions-date-range seed-year make-setting-invalid]
    :as _trained-model}
   {:keys [random-seed simulations]
    :as _projection-parameters}]
  (d/set-seed! random-seed)
  (let [{:keys [population population-by-state
                projected-population cost-lookup
                valid-states transitions]} standard-projection
        scenario-projection' (when scenario-projection
                               (assoc scenario-projection :valid-year-settings
                                      (states/calculate-valid-year-settings-from-setting-academic-years valid-states)))
        standard-projection' (assoc standard-projection :valid-year-settings
                                    (states/calculate-valid-year-settings-from-setting-academic-years valid-states))]
    (map (fn [simulation-run]
           [simulation-run (reductions (partial r/run-model-iteration
                                                modify-transitions-date-range
                                                make-setting-invalid
                                                standard-projection'
                                                scenario-projection')
                                       {:model population-by-state}
                                       (r/projected-future-pop-by-year projected-population seed-year))]))))
