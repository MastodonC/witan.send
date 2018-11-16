(ns witan.send.utils
  (:require [kixi.stats.math :as math]
            [medley.core :as medley]
            [witan.send.constants :as c]
            [witan.send.maths :as m]
            [witan.send.states :as states])
  (:import org.HdrHistogram.IntCountsHistogram))

(defn model-population-by-ay
  [model]
  (reduce (fn [coll [[ay state] population]]
            (cond-> coll
              (not= state c/non-send)
              (update ay m/some+ population)))
          {} model))

(defn model-population-by-need
  [model]
  (reduce (fn [coll [[ay state] population]]
            (let [[need setting] (states/split-need-setting state)]
              (cond-> coll
                (not= state c/non-send)
                (update need m/some+ population))))
          {} model))

(defn model-population-by-setting
  [model]
  (reduce (fn [coll [[ay state] population]]
            (let [[need setting] (states/split-need-setting state)]
              (cond-> coll
                (not= state c/non-send)
                (update setting m/some+ population))))
          {} model))

(defn model-population-by-need-setting
  [model]
  (reduce (fn [coll [[ay state] population]]
            (let [[need setting] (states/split-need-setting state)]
              (cond-> coll
                (not= state c/non-send)
                (update [need setting] m/some+ population))))
          {} model))

(defn ay-groups [ay]
  (condp >= ay
    0 "NCY < 1"
    6 "NCY 1-6"
    11 "NCY 7-11"
    13 "NCY 12-13"
    "NCY 14+"))

(defn model-population-by-ay-group
  [model]
  (reduce (fn [coll [[ay state] population]]
            (let [ay-group (ay-groups ay)]
              (cond-> coll
                (not= state c/non-send)
                (update ay-group m/some+ population))))
          {} model))

(defn total-need-setting-cost
  [need-setting-lookup population-by-need-setting]
  (-> (reduce (fn [cost [split-need-setting population]]
                (+ cost (* population (get need-setting-lookup split-need-setting 0))))
              0 population-by-need-setting)
      m/round))

(defn model-send-population
  [model]
  (reduce (fn [n [[ay state] population]]
            (cond-> n
              (not= state c/non-send)
              (+ population)))
          0 model))

;;;; Reducing functions for use with transduce

(defn histogram-rf
  [number-of-significant-digits]
  (fn
    ([] (IntCountsHistogram. number-of-significant-digits))
    ([hist x]
     (doto hist (.recordValue (inc x))))
    ([hist] hist)))

(defn confidence-intervals [m sims]
  (let [z-value 1.96 ;; this is for a 95% confidence value assuming normal distribution
        std-err (/ (:std-dev m) (math/sqrt sims))
        margin-of-error (* z-value std-err)]
    (merge m {:low-ci (- (:mean m) margin-of-error)
              :high-ci (+ (:mean m) margin-of-error)})))

(defn histogram-combiner-rf
  [simulations number-of-significant-digits]
  (fn
    ([] (IntCountsHistogram. number-of-significant-digits))
    ([acc hist]
     (doto acc (.add hist)))
    ([hist]
     (let [result {:median (dec (.getValueAtPercentile hist 50.0))
                   :mean (dec (.getMean hist))
                   :std-dev (.getStdDeviation hist)
                   :iqr (- (.getValueAtPercentile hist 75.0) (.getValueAtPercentile hist 25.0))
                   :min (dec (.getValueAtPercentile hist 0.0))
                   :max (dec (.getValueAtPercentile hist 100.0))
                   :q1 (dec (.getValueAtPercentile hist 25.0))
                   :q3 (dec (.getValueAtPercentile hist 75.0))
                   :low-95pc-bound (dec (.getValueAtPercentile hist 2.5))
                   :high-95pc-bound (dec (.getValueAtPercentile hist 97.5))}]
       (confidence-intervals result simulations)))))

(defn merge-with-rf
  "Like (apply merge-with f) but for reducing functions"
  [rf]
  (fn
    ([] {})
    ([acc x]
     (reduce (fn [acc [k v]]
               (-> acc
                   (cond-> (not (contains? acc k))
                     (assoc k (rf)))
                   (update k rf v)))
             acc x))
    ([acc]
     (medley/map-vals rf acc))))

(defn model-states-rf
  [valid-states rf]
  (fn
    ([]
     (reduce (fn [coll k]
               (assoc coll k (rf)))
             {} valid-states))
    ([acc x]
     (medley/map-kv
      (fn [k v]
        [k (rf v (get x k 0))])
      acc))
    ([acc]
     (medley/map-vals rf acc))))

(defn with-keys-rf
  [rf keys]
  (fn
    ([]
     (reduce (fn [coll k]
               (assoc coll k (rf)))
             {} keys))
    ([acc x]
     (medley/map-kv
      (fn [k v]
        [k (rf v (get x k 0))])
      acc))
    ([acc]
     (medley/map-vals rf acc))))

(defn partition-rf
  "Executes the rf on partitions of length n, returning n results"
  [n rf]
  (fn
    ([]
     (println "Initialising...")
     (mapv #(%1) (repeat n rf)))
    ([acc xs]
     (mapv #(rf %1 %2) acc xs))
    ([acc]
     (println "Complete rf...")
     (mapv #(rf %1) acc))))
