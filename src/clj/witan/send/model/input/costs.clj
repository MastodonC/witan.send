(ns witan.send.model.input.costs
  (:require [witan.send.model.input :as i]))

(defn csv->costs
  [file-name]
  (i/csv->x
   (map #(-> %
             (update :cost i/->double)
             (update :need keyword)
             (update :setting keyword)))
   file-name))

(comment

  (def costs (csv->costs "data/demo/data/costs.csv"))

  )
