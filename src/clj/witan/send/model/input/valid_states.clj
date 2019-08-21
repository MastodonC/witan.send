(ns witan.send.model.input.valid-states
  (:require [witan.send.model.input :as i]))


(defn csv->valid-states
  [file-name]
  (i/csv->x (map #(-> %
                      (update :setting keyword)
                      (update :min-academic-year i/->int)
                      (update :max-academic-year i/->int)))
            file-name))

(comment

  (def valid-states (csv->valid-states "data/demo/data/valid-states.csv"))

  )
