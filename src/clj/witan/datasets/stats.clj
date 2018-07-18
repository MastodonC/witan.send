(ns witan.datasets.stats
  (:require [incanter.stats :as st]))

;; Wrappers for Incanter functions defined in  incanter.stats

(defn standard-deviation
  [xs]
  (st/sd xs))
