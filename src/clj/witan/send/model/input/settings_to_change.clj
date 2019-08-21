(ns witan.send.model.input.settings-to-change
  (:require [witan.send.model.input :as i]))

(defn csv->settings-to-change [file-name]
  (i/csv->x (map #(-> %
                      (update :setting-1 keyword)
                      (update :setting-2 keyword)))
            file-name))
