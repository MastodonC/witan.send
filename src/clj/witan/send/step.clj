(ns witan.send.step
  (:require [witan.send.constants :as const]
            [witan.send.states :as s]))

(defn age-population
  [projection model-state]
  (-> (reduce (fn [coll [[year state :as k] population]]
                (cond-> coll
                  (< year const/max-academic-year)
                  (assoc [(inc year) state] population)))
              {}
              model-state)
      (assoc [const/min-academic-year s/non-send] (get projection const/min-academic-year))))

(defn binomial-mean
  [n {:keys [alpha beta]}]
  (let [d (+ alpha beta)]
    (if (zero? d)
      0
      (* n (/ alpha d)))))

(defn dirichlet-mean
  [n alphas]
  (let [d (->> alphas vals (apply +))
        f (if (zero? d)
            0 (/ n d))]
    (reduce (fn [coll [k v]]
              (let [v (* f v)]
                (cond-> coll
                  (pos? v)
                  (assoc k v))))
            {}
            alphas)))

(defn apply-leavers
  [model-state params f]
  (reduce (fn [coll [[year state :as k] population]]
            (let [p (get params k)
                  leavers (f population p)]
              (assoc coll k (- population leavers))))
          {}
          model-state))

(defn step-leavers
  [model-state transitions params f]
  (reduce (fn [[model-state transitions] [[year state :as k] population]]
            (let [p (get params k)
                  leavers (f population p)]
              (if (not= state s/non-send)
                [(-> (update model-state [year state] - leavers)
                     (update [year s/non-send] + leavers))
                 (-> (assoc transitions [year state state] (- population leavers))
                     (assoc [year state s/non-send] leavers))]
                [model-state transitions])))
          [model-state transitions]
          model-state))

(defn step-movers
  [model-state transitions betas binomial alphas dirichlet]
  (reduce (fn [[model-state transitions] [[year state :as k] population]]
            (let [bs (get betas k)
                  as (get alphas k)
                  movers-count (binomial population bs)
                  movers (dirichlet movers-count as)]
              (if (not= state s/non-send)
                [(reduce (fn [model-state [k v]]
                           (update model-state [year k] (fnil + 0) v))
                         (update model-state [year state] - movers-count)
                         movers)
                 (reduce (fn [transitions [k v]]
                           (update transitions [year state k] + v))
                         (update transitions [year state state] - movers-count)
                         movers)]
                [model-state transitions])))
          [model-state transitions]
          model-state))

(defn step-joiners
  [model-state transitions population betas binomial
   age-alphas dirichlet state-alphas state-dirichlet]
  (reduce (fn [[model-state transitions] [ay population]]
            (let [bs (get betas ay)
                  joiners-count (binomial population bs)
                  joiner-ages (dirichlet joiners-count age-alphas)
                  joiner-states (reduce (fn [coll [ay joiners]]
                                          (assoc coll ay (state-dirichlet joiners (get state-alphas ay))))
                                        {} joiner-ages)]
              [(reduce (fn [coll [year states]]
                         (reduce (fn [coll [state joiners]]
                                   (-> (update coll [year s/non-send] (fnil - 0) joiners)
                                       (update [year states] (fnil + 0) joiners)))
                                 coll
                                 states))
                       model-state
                       joiner-states)
               (reduce (fn [coll [year states]]
                         (reduce (fn [coll [state joiners]]
                                   (update coll [year s/non-send state] (fnil + 0) joiners))
                                 coll
                                 states))
                       transitions
                       joiner-states)]))
          [model-state transitions]
          population))
