(ns witan.send.model.run
  (:require [witan.send.schemas :as sc]
            [clojure.core.matrix.dataset :as ds]
            [witan.send.step :as step]
            [witan.send.states :as states]
            [witan.send.utils :as u :refer [round]]
            [redux.core :as r]
            [witan.send.report :refer [reset-send-report]]
            [witan.send.distributions :as d]))

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

(defn sample-send-transitions
  "Takes a total count and map of categories to probabilities and
  returns the count in each category at the next step."
  [state n probs mover-beta]
  (try
    (if (pos? n)
      (let [movers (d/sample-beta-binomial n mover-beta)
            non-movers (- n movers)]
        (-> (d/sample-dirichlet-multinomial movers probs)
            (assoc state non-movers)))
      {})
    (catch Exception e
      (do (println state n probs mover-beta)
          nil))))

(defn apply-leavers-movers-for-cohort-unsafe
  "We're calling this function 'unsafe' because it doesn't check whether the state or
  or academic year range is valid."
  [[model transitions] [[year state] population]
   {:keys [leaver-beta-params mover-beta-params
           mover-state-alphas valid-year-settings]}
   calendar-year]
  (if-let [probs (get mover-state-alphas [(dec year) state])]
    (let [leaver-params (get leaver-beta-params [(dec year) state])
          l (d/sample-beta-binomial population leaver-params)
          next-states-sample (if (states/can-move? valid-year-settings year state)
                               (let [mover-params (get mover-beta-params [(dec year) state])]
                                 (sample-send-transitions state (- population l) probs mover-params))
                               {state (- population l)})
          [model transitions] (incorporate-new-states-for-academic-year-state [model transitions] year state next-states-sample calendar-year)]
      [model
       (update transitions [calendar-year year state sc/non-send] u/some+ l)])
    [model transitions]))

(defn apply-leavers-movers-for-cohort
  "Take single cohort of users and process them into the model state.
  Calls 'unsafe' equivalent once we've removed non-send and children outside
  valid academic year range."
  [[model transitions :as model-state]
   [[year state] population :as cohort]
   params calendar-year]
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
      (let [joiners (d/sample-beta-binomial pop betas)]
        (if (zero? joiners)
          [model transitions]
          (let [joiner-states (d/sample-dirichlet-multinomial joiners alphas)]
            (incorporate-new-states-for-academic-year-state [model transitions] academic-year sc/non-send joiner-states calendar-year))))
      [model transitions])))

(defn run-model-iteration
  "Takes the model & transitions, transition params, and the projected population and produce the next state of the model & transitions"
  [modify-transitions-from
   standard-projection
   scenario-projection
   {model-state :model}
   [calendar-year projected-population]]
  (let [params (if (nil? modify-transitions-from)
                 (if ((complement nil?) scenario-projection)
                   scenario-projection
                   standard-projection)
                 (if (>= calendar-year modify-transitions-from)
                   scenario-projection
                   standard-projection))
        cohorts (step/age-population projected-population model-state)
        [model transitions] (reduce (fn [model-state cohort]
                                      (apply-leavers-movers-for-cohort model-state cohort params calendar-year))
                                    [{} {}]
                                    cohorts)
        [model transitions] (reduce (fn [model-state academic-year]
                                      (apply-joiners-for-academic-year model-state academic-year projected-population params calendar-year))
                                    [model transitions]
                                    sc/academic-years)]
    {:model model :transitions transitions}))

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
  [{:keys [standard-projection scenario-projection modify-transition-by settings-to-change]}
   {:keys [seed-year random-seed simulations modify-transitions-from]}]
  (d/set-seed! random-seed)
  (println "Preparing" simulations "simulations...")
  (let [{:keys [population population-by-age-state
                projected-population setting-cost-lookup
                valid-setting-academic-years transition-matrix] :as inputs} standard-projection
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
                                 (->> (for [_ simulations]
                                        (let [projection (reductions (partial run-model-iteration modify-transitions-from inputs modified-inputs)
                                                                     {:model population-by-age-state}
                                                                     projected-future-pop-by-year)]
                                          projection))
                                      (doall))))
                         (doall))
        reduced (doall
                 (for [projection projections]
                   (do (println "Reducing...")
                       (transduce (map #(map :model %)) (reduce-rf iterations valid-states setting-cost-lookup) projection))))
        projection (apply concat projections)]
    (println "Combining...")
    {:projection (projection->transitions projection)
     :send-output (transduce identity (combine-rf simulations iterations) reduced)
     :transition-matrix transition-matrix
     :valid-setting-academic-years valid-setting-academic-years
     :population population
     :modify-transition-by modify-transition-by
     :settings-to-change settings-to-change}))
