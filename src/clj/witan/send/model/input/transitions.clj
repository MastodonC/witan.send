(ns witan.send.model.input.transitions
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [witan.send.model.input :as i]))

(defn csv->transitions
  [file-name]
  (with-open [reader (io/reader file-name)]
    (let [[header & data] (csv/read-csv reader)
          header (map keyword header)]
      (into []
            (comp
             (map #(zipmap header %))
             (map #(-> %
                       (update :calendar-year i/->int)
                       (update :academic-year-1 i/->int)
                       (update :academic-year-2 i/->int))))
            data))))

(comment

  (def transitions (csv->transitions "data/demo/data/transitions.csv"))

  )
