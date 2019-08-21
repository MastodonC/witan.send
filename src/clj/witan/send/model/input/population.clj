(ns witan.send.model.input.population
  (:require [witan.send.model.input :as i]))

(defn csv->population
  [file-name]
  (i/csv->x (map #(-> %
                      (update :calendar-year i/->int)
                      (update :academic-year i/->int)
                      (update :population i/->int)))
            file-name))

(comment

  (def population (csv->population "data/demo/data/population.csv"))

  )
