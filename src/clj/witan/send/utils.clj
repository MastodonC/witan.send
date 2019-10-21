(ns witan.send.utils
  (:require [kixi.stats.math :as math]))

(defn ay-groups [ay]
  (condp >= ay
    0 "NCY < 1"
    6 "NCY 1-6"
    11 "NCY 7-11"
    13 "NCY 12-13"
    "NCY 14+"))

;;;; Reducing functions for use with transduce

(defn confidence-intervals [m sims]
  (let [z-value 1.96 ;; this is for a 95% confidence value assuming normal distribution
        std-err (/ (:std-dev m) (math/sqrt sims))
        margin-of-error (* z-value std-err)]
    (merge m {:low-ci (- (:mean m) margin-of-error)
              :high-ci (+ (:mean m) margin-of-error)})))
