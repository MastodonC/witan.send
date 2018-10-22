(ns witan.send.step
  (:require [witan.send.constants :as const]
            [witan.send.states :as s]))

(defn age-population
  [projection model-state]
  (-> (reduce (fn [coll [[year state] population]]
                (cond-> coll
                  (< year const/max-academic-year)
                  (assoc [(inc year) state] population)))
              {}
              model-state)
      (assoc [const/min-academic-year s/non-send] (get projection const/min-academic-year))))
