(ns witan.send.goal-seek
  (:require [witan.send.multi-config :as mc]))

(defn update-transition-modifier [m n]
  "Takes a map of keys partially matching a transition and a new modifier"
  [(assoc m :modify-transition-by n)])

(defn create-transition-modifier-seq [m start end step]
  "Take a map of keys partially matching a transition and creates a sequence of configs for
   :transitions-to-change"
  [[[:transition-parameters :transitions-to-change]
    (mapv #(update-transition-modifier m %) (range start end step))]])
