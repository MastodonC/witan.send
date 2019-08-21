(ns witan.send.model.input.population
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [witan.send.model.input :as i]))

(defn csv->population
  [file-name]
  (with-open [reader (io/reader file-name)]
    (let [[header & data] (csv/read-csv reader)
          header (map keyword header)]
      (into []
            (comp
             (map #(zipmap header %))
             (map #(-> %
                       (update :calendar-year i/->int)
                       (update :academic-year i/->int)
                       (update :population i/->int))))
            data))))

(comment

  (def population (csv->population "data/demo/data/population.csv"))

  )
