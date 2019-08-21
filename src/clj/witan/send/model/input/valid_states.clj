(ns witan.send.model.input.valid-states
  (:require [clojure.string :as s]
            [witan.send.model.input :as i]))


(defn csv->valid-states
  [file-name]
  (i/csv->x (map #(-> %
                      (update :min-academic-year i/->int)
                      (update :max-academic-year i/->int)
                      (update :needs s/split #",")
                      (update :setting->setting s/split #",")))
            file-name))

(comment

  (def valid-states (csv->valid-states "data/demo/data/valid-states.csv"))

  )
