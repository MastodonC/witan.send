(ns witan.send.model.run
  (:require [redux.core :as r]
            [witan.send.constants :as c]
            [witan.send.distributions :as d]
            [witan.send.maths :as m]
            [witan.send.schemas :as sc]
            [witan.send.states :as states]
            [witan.send.step :as step]
            [witan.send.model.data-products :as dp]))

(defn incorporate-new-ay-need-setting-populations
  "Take a model + transitions tuple as its first argument.
  Returns a model + transitions tuple with `predicted-populations`
  incorporated."
  [{:keys [model transitions academic-year need-setting
           predicted-populations calendar-year]}]
  (vector
   (reduce (fn [coll [next-need-setting n]]
             (cond-> coll
               (pos? n)
               (update [academic-year next-need-setting] m/some+ n)))
           model predicted-populations)
   (reduce (fn [coll [next-need-setting n]]
             (cond-> coll
               (pos? n)
               (update [calendar-year academic-year need-setting next-need-setting] m/some+ n)))
           transitions predicted-populations)))

(defn predict-movers
  "Returns a map of predicted need-setting counts for movers for a given
  `need-setting` and a population `n` with the provided probability
  distribution parameters.

  Also work out how much of the population remains (non-movers) and
  add to the results.

  Note: the actual results are the combination of chaining the beta
  and binomial distributions or the Dirichlet and multinomial
  distribution.  Beta and Dirichlet are used to sample a distribution
  and the outcome is selected by the respective binomial or
  multinomial."
  [{:keys [need-setting n dirichlet-params beta-params]}]
  (if (pos? n)
    (let [mover-n (d/sample-beta-binomial n beta-params)
          non-mover-n (- n mover-n)]
      (-> (d/sample-dirichlet-multinomial mover-n dirichlet-params)
          (assoc need-setting non-mover-n)))
    {}))

(defn predict-leavers
  [{:keys [need-setting n beta-params]}]
  (d/sample-beta-binomial n beta-params))

(defn apply-leavers-movers-for-cohort-unsafe
  "We're calling this function 'unsafe' because it doesn't check whether the need-setting or
  or academic year range is valid."
  [[model transitions] [[year need-setting] population]
   {:keys [mover-state-alphas mover-beta-params leaver-beta-params
           valid-year-settings] :as params}
   calendar-year valid-transitions make-setting-invalid]
  (if-let [mover-dirichlet-params (get mover-state-alphas [(dec year) need-setting])]
    (let [leavers (if (= (second (states/split-need-setting need-setting)) make-setting-invalid)
                    population
                    (predict-leavers {:need-setting need-setting
                                      :n population
                                      :beta-params (get leaver-beta-params [(dec year) need-setting])}))
          movers (if (states/can-move? valid-year-settings year need-setting valid-transitions)
                   (if (= (second (states/split-need-setting need-setting)) make-setting-invalid)
                     0
                     (predict-movers {:need-setting need-setting
                                      :n (- population leavers)
                                      :beta-params (get mover-beta-params [(dec year) need-setting])
                                      :dirichlet-params mover-dirichlet-params}))
                   {need-setting (- population leavers)})
          [model transitions] (incorporate-new-ay-need-setting-populations {:model model :transitions transitions
                                                                            :academic-year year :need-setting need-setting
                                                                            :predicted-populations movers
                                                                            :calendar-year calendar-year})]
      [model
       (update transitions [calendar-year year need-setting c/non-send] m/some+ leavers)])
    [model transitions]))

(defn apply-leavers-movers-for-cohort
  "Take single cohort of users and process them.
  Calls 'unsafe' equivalent once we've removed non-send and children outside
  valid academic year range."
  [[model transitions :as population-by-state]
   [[year state] population :as cohort]
   params calendar-year valid-transitions
   make-setting-invalid]
  (cond
    (= state c/non-send)
    population-by-state
    (or (<= year sc/min-academic-year)
        (> year sc/max-academic-year))
    [model
     (cond-> transitions
       (pos? population)
       (update [calendar-year year state c/non-send] m/some+ population))]
    :else
    (apply-leavers-movers-for-cohort-unsafe population-by-state cohort params calendar-year
                                            valid-transitions make-setting-invalid)))

(defn predict-joiners
  "Returns a map of predicted need-setting counts for joiners for a
  given population n` with the provided probability distribution
  parameters."
  [{:keys [n dirichlet-params beta-params]}]
  (when (and n dirichlet-params beta-params (pos? n))
    (let [joiners (d/sample-beta-binomial n beta-params)]
      (when (pos? joiners)
        (d/sample-dirichlet-multinomial joiners dirichlet-params)))))

(defn apply-joiners-for-academic-year
  [[model transitions] academic-year population {:keys [joiner-beta-params joiner-state-alphas]} calendar-year]
  (let [betas (get joiner-beta-params academic-year)
        alphas (get joiner-state-alphas academic-year)
        pop (get population academic-year)]
    (if-let [joiners (predict-joiners {:n pop
                                       :beta-params betas
                                       :dirichlet-params alphas})]
      (incorporate-new-ay-need-setting-populations {:model model :transitions transitions
                                                    :academic-year academic-year :need-setting c/non-send
                                                    :predicted-populations joiners
                                                    :calendar-year calendar-year})
      [model transitions])))

(defn run-model-iteration
  "Takes the model & transitions, transition params, and the projected population and produce the next state of the model & transitions"
  [modify-transitions-from
   make-setting-invalid
   standard-projection
   scenario-projection
   {population-by-state :model}
   [calendar-year projected-population]]
  (let [valid-transitions (:valid-transitions standard-projection)
        params (if (nil? modify-transitions-from)
                 (if ((complement nil?) scenario-projection)
                   scenario-projection
                   standard-projection)
                 (if (>= calendar-year modify-transitions-from)
                   scenario-projection
                   standard-projection))
        cohorts (step/age-population projected-population population-by-state)
        [population-by-state transitions] (reduce (fn [pop cohort]
                                                    (apply-leavers-movers-for-cohort pop cohort params calendar-year valid-transitions make-setting-invalid))
                                                  [{} {}]
                                                  cohorts)
        [population-by-state transitions] (reduce (fn [pop academic-year]
                                                    (apply-joiners-for-academic-year pop academic-year projected-population params calendar-year))
                                                  [population-by-state transitions]
                                                  sc/academic-years)]
    {:model population-by-state :transitions transitions}))

(defn projection->transitions
  [projections]
  (apply merge-with + (mapcat #(map :transitions %) projections)))

(defn projected-future-pop-by-year [projected-population seed-year]
  (->> projected-population
       (filter (fn [[k _]] (> k seed-year)))
       (sort-by key)))

(defn create-projections [simulations modify-transitions-from make-setting-invalid inputs modified-inputs population-by-state projected-population seed-year]
  (sequence
   (map (fn [simulation-run]
          (reductions (partial run-model-iteration
                               modify-transitions-from
                               make-setting-invalid
                               inputs
                               modified-inputs)
                      {:model population-by-state}
                      (projected-future-pop-by-year projected-population seed-year))))
   (range simulations)))

(defn run-send-model
  "Outputs the population for the last year of historic data, with one
   row for each individual/year/simulation. Also includes age & state columns"
  [{:keys [standard-projection scenario-projection modify-transition-by
           modify-transitions-from seed-year make-setting-invalid]}
   {:keys [random-seed simulations]}]
  (d/set-seed! random-seed)
  (println "Preparing" simulations "simulations...")
  (let [{:keys [population population-by-state
                projected-population cost-lookup
                valid-states transitions] :as inputs} standard-projection
        modified-inputs (when ((complement nil?) scenario-projection)
                          (assoc scenario-projection :valid-year-settings
                                 (states/calculate-valid-year-settings-from-setting-academic-years valid-states)))
        inputs (assoc inputs :valid-year-settings (states/calculate-valid-year-settings-from-setting-academic-years valid-states))
        projection (create-projections simulations
                                       modify-transitions-from
                                       make-setting-invalid
                                       inputs
                                       modified-inputs
                                       population-by-state
                                       projected-population
                                       seed-year)]
    {:projection (projection->transitions projection)
     :simulations simulations
     :send-output (dp/->send-output-style (dp/data-products valid-states cost-lookup projection))
     :transitions transitions
     :valid-states valid-states
     :population population
     :modify-transition-by modify-transition-by
     :standard-projection standard-projection
     :scenario-projection scenario-projection}))
