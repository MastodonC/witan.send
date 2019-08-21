(ns witan.send.model.input.transitions
  (:require [witan.send.model.input :as i]))

(defn csv->transitions
  [file-name]
  (i/csv->x (map #(-> %
                      (update :calendar-year i/->int)
                      (update :academic-year-1 i/->int)
                      (update :academic-year-2 i/->int)))
            file-name))

(comment

  (def transitions (csv->transitions "data/demo/data/transitions.csv"))

  )
