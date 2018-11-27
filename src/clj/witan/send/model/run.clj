(ns witan.send.model.run
  (:require [clojure.core.matrix.dataset :as ds]
            [redux.core :as r]
            [witan.send.constants :as c]
            [witan.send.distributions :as d]
            [witan.send.maths :as m]
            [witan.send.schemas :as sc]
            [witan.send.states :as states]
            [witan.send.step :as step]
            [witan.send.utils :as u]))

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
   calendar-year]
  (if-let [mover-dirichlet-params (get mover-state-alphas [(dec year) need-setting])]
    (let [leavers (predict-leavers {:need-setting need-setting
                                    :n population
                                    :beta-params (get leaver-beta-params [(dec year) need-setting])})
          movers (if (states/can-move? valid-year-settings year need-setting)
                               (predict-movers {:need-setting need-setting
                                                :n (- population leavers)
                                                :beta-params (get mover-beta-params [(dec year) need-setting])
                                                :dirichlet-params mover-dirichlet-params})
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
   params calendar-year]
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
    (apply-leavers-movers-for-cohort-unsafe population-by-state cohort params calendar-year)))


;; (defn apply-joiners-for-academic-year
;;   [[model transitions] academic-year population {:keys [joiner-beta-params joiner-state-alphas]} calendar-year]
;;   (let [betas (get joiner-beta-params academic-year)
;;         alphas (get joiner-state-alphas academic-year)
;;         pop (get population academic-year)]
;;     (if (and alphas betas pop (every? pos? (vals betas)))
;;       (let [joiners (d/sample-beta-binomial pop betas)]
;;         (if (zero? joiners)
;;           [model transitions]
;;           (let [joiner-states (d/sample-dirichlet-multinomial joiners alphas)]
;;             (incorporate-new-ay-need-setting-populations {:model model :transitions transitions
;;                                                           :academic-year academic-year :need-setting c/non-send
;;                                                           :predicted-populations joiner-states
;;                                                           :calendar-year calendar-year}))))
;;       [model transitions])))

;; Rework doesn't pass tests
(defn predict-joiners
  "Returns a map of predicted need-setting counts for joiners for a
  given population n` with the provided probability distribution
  parameters."
  [{:keys [n dirichlet-params beta-params]}]
  (when (pos? n)
    (let [joiners (d/sample-beta-binomial n beta-params)]
      (when (pos? joiners)
        (d/sample-dirichlet-multinomial joiners dirichlet-params)))))

(defn apply-joiners-for-academic-year
  [[model transitions] academic-year population {:keys [joiner-beta-params joiner-state-alphas]} calendar-year]
  (let [betas (get joiner-beta-params academic-year)
        alphas (get joiner-state-alphas academic-year)
        pop (get population academic-year)]
    (if (and alphas betas pop (every? pos? (vals betas)))
      (let [joiners (predict-joiners {:n pop
                                      :beta-params betas
                                      :dirichlet-params alphas})]
        (if joiners
          (incorporate-new-ay-need-setting-populations {:model model :transitions transitions
                                                        :academic-year academic-year :need-setting c/non-send
                                                        :predicted-populations joiners
                                                        :calendar-year calendar-year})
          [model transitions]))
      [model transitions])))

(defn run-model-iteration
  "Takes the model & transitions, transition params, and the projected population and produce the next state of the model & transitions"
  [modify-transitions-from
   standard-projection
   scenario-projection
   {population-by-state :model}
   [calendar-year projected-population]]
  (let [params (if (nil? modify-transitions-from)
                 (if ((complement nil?) scenario-projection)
                   scenario-projection
                   standard-projection)
                 (if (>= calendar-year modify-transitions-from)
                   scenario-projection
                   standard-projection))
        cohorts (step/age-population projected-population population-by-state)
        [population-by-state transitions] (reduce (fn [pop cohort]
                                                    (apply-leavers-movers-for-cohort pop cohort params calendar-year))
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

(defn values-rf
  "Associate a reducing function to be used for each value of map indexed by key"
  [kvs]
  (->> (for [[k v] kvs]
         [k (r/pre-step v k)])
       (into {})
       (r/fuse)))

(def number-of-significant-digits 3)

(defn reduce-rf [iterations validate-valid-states cost-lookup]
  (u/partition-rf iterations
                  (r/fuse {:by-state (u/model-states-rf validate-valid-states (u/histogram-rf number-of-significant-digits))
                           :total-in-send-by-ay (r/pre-step (u/with-keys-rf (u/histogram-rf number-of-significant-digits) sc/academic-years) u/model-population-by-ay)
                           :total-in-send (r/pre-step (u/histogram-rf number-of-significant-digits) u/model-send-population)
                           :total-in-send-by-need (r/pre-step (u/merge-with-rf (u/histogram-rf number-of-significant-digits)) u/model-population-by-need)
                           :total-in-send-by-setting (r/pre-step (u/merge-with-rf (u/histogram-rf number-of-significant-digits)) u/model-population-by-setting)
                           :total-cost (r/pre-step (u/histogram-rf number-of-significant-digits) (comp (partial u/total-need-setting-cost cost-lookup)
                                                                                                       u/model-population-by-need-setting))
                           :total-in-send-by-ay-group (r/pre-step (u/merge-with-rf (u/histogram-rf number-of-significant-digits))
                                                                  u/model-population-by-ay-group)})))

(defn combine-rf [simulations iterations]
  (u/partition-rf iterations
                  (values-rf {:by-state (u/merge-with-rf (u/histogram-combiner-rf simulations number-of-significant-digits))
                              :total-in-send-by-ay (u/merge-with-rf (u/histogram-combiner-rf simulations number-of-significant-digits))
                              :total-in-send (u/histogram-combiner-rf simulations number-of-significant-digits)
                              :total-in-send-by-need (u/merge-with-rf (u/histogram-combiner-rf simulations  number-of-significant-digits))
                              :total-in-send-by-setting (u/merge-with-rf (u/histogram-combiner-rf simulations number-of-significant-digits))
                              :total-cost (u/histogram-combiner-rf simulations number-of-significant-digits)
                              :total-in-send-by-ay-group (u/merge-with-rf (u/histogram-combiner-rf simulations number-of-significant-digits))})))

(defn run-send-model
  "Outputs the population for the last year of historic data, with one
   row for each individual/year/simulation. Also includes age & state columns"
  [{:keys [standard-projection scenario-projection modify-transition-by
           modify-transitions-from seed-year]}
   {:keys [random-seed simulations]}]
  (d/set-seed! random-seed)
  (println "Preparing" simulations "simulations...")
  (let [{:keys [population population-by-state
                projected-population cost-lookup
                valid-states transitions] :as inputs} standard-projection
        modified-inputs (when ((complement nil?) scenario-projection)
                          (assoc scenario-projection :valid-year-settings
                                 (->> (ds/row-maps valid-states)
                                      (states/calculate-valid-year-settings-from-setting-academic-years))))
        projected-future-pop-by-year (->> projected-population
                                          (filter (fn [[k _]] (> k seed-year)))
                                          (sort-by key))
        iterations (inc (count projected-future-pop-by-year)) ;; include current year
        validate-valid-states (->> (ds/row-maps valid-states)
                                   (states/calculate-valid-states-from-setting-academic-years))
        inputs (assoc inputs :valid-year-settings (->> (ds/row-maps valid-states)
                                                       (states/calculate-valid-year-settings-from-setting-academic-years)))
        projections (->> (range simulations)
                         (partition-all (int (/ simulations 8)))
                         (pmap (fn [simulations]
                                 (->> (for [_ simulations]
                                        (let [projection (reductions (partial run-model-iteration modify-transitions-from inputs modified-inputs)
                                                                     {:model population-by-state}
                                                                     projected-future-pop-by-year)]
                                          projection))
                                      (doall))))
                         (doall))
        reduced (doall
                 (for [projection projections]
                   (do (println "Reducing...")
                       (transduce (map #(map :model %)) (reduce-rf iterations validate-valid-states cost-lookup) projection))))
        projection (apply concat projections)]
    (println "Combining...")
    {:projection (projection->transitions projection)
     :send-output (transduce identity (combine-rf simulations iterations) reduced)
     :transitions transitions
     :valid-states valid-states
     :population population
     :modify-transition-by modify-transition-by}))
