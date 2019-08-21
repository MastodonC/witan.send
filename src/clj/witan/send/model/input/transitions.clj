(ns witan.send.model.input.transitions
  (:require [witan.send.model.input :as i]))

(defn csv->transitions
  [file-name]
  (i/csv->x (map #(-> %
                      (update :calendar-year i/->int)
                      (update :academic-year-1 i/->int)
                      (update :academic-year-2 i/->int)
                      (update :setting-1 keyword)
                      (update :setting-2 keyword)
                      (update :need-1 keyword)
                      (update :need-2 keyword)))
            file-name))

(comment

  (def transitions (csv->transitions "data/demo/data/transitions.csv"))

  )
