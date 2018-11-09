(ns witan.send.step
  (:require [witan.send.constants :as c]))

(defn age-population
  [projection model-state]
  (-> (reduce (fn [coll [[year state] population]]
                (cond-> coll
                  (< year c/max-academic-year)
                  (assoc [(inc year) state] population)))
              {}
              model-state)
      (assoc [c/min-academic-year c/non-send] (get projection c/min-academic-year))))
