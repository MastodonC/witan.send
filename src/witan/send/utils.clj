(ns witan.send.utils
  (:require [clojure.core.matrix.dataset :as ds]
            [clojure.data.avl :as avl]
            [witan.workspace-api.utils :as utils]
            [witan.send.schemas :as sc]
            [kixi.stats.core :as kixi]
            [kixi.stats.random :refer [dirichlet-multinomial draw]]
            [redux.core :as r]
            [medley.core :as medley]
            [schema.core :as s]
            [clojure.string :as str]
            [witan.send.utils :as u])
  (:import [org.HdrHistogram IntCountsHistogram DoubleHistogram]))

(defn round [x]
  (Double/parseDouble (format "%.02f" (double x))))

(def some+
  "x + y. Returns y if x is nil and x if y is nil."
  (fnil + 0))

(defn order-ds
  [dataset col-key]
  (utils/property-holds? dataset ds/dataset? "Not a dataset")
  (cond (keyword? col-key) (->> dataset
                                ds/row-maps
                                vec
                                (sort-by col-key)
                                ds/dataset)
        (vector? col-key) (->> dataset
                               ds/row-maps
                               vec
                               (sort-by (apply juxt col-key))
                               ds/dataset)))

(defn filter-indexed
  [pred coll]
  (keep identity
        (map-indexed pred coll)))

(defn state [need setting]
  (keyword (str (name need) "-" (name setting))))

(defn need-setting [state]
  (mapv keyword (str/split (name state) #"-")))

(defn valid-year-setting? [year setting]
  (or
   (and (= setting :CC) (<= year 0))
   (and (= setting :EO) (<= -1 year 13))
   (and (= setting :FEC) (<= 12 year 22))
   (and (= setting :IMS) (<= -1 year 14))
   (and (= setting :IN) (<= year 0))
   (and (= setting :ISC) (<= 14 year 19))
   (and (= setting :ISS) (<= 0 year 15))
   (and (= setting :IT) (<= 2 year 15))
   (and (= setting :MMS) (<= year 15))
   (and (= setting :MSS) (<= year 14))
   (and (= setting :OOE) (<= 6 year 19))
   (and (= setting :PRU) (<= 2 year 11))
   (and (= setting :MU) (<= -1 year 14))))

(defn valid-state? [academic-year state]
  (or (= state sc/non-send)
      (let [[need setting] (need-setting state)]
        (valid-year-setting? academic-year setting))))

(def valid-states
  (->> (concat (for [academic-year sc/academic-years
                     setting sc/settings
                     need sc/needs]
                 [academic-year (state need setting)])
               (for [academic-year sc/academic-years]
                 [academic-year sc/non-send]))
       (filter #(apply valid-state? %))))

(defn valid-transition? [academic-year state-1 state-2]
  (and (valid-state? academic-year state-1)
       (valid-state? academic-year state-1)))

(def valid-transitions
  (->> (concat (for [academic-year sc/academic-years
                     from-setting sc/settings
                     to-setting sc/settings
                     need sc/needs]
                 [academic-year (state need from-setting) (state need to-setting)])
               (for [academic-year sc/academic-years
                     to-setting sc/settings
                     need sc/needs]
                 [academic-year sc/non-send (state need to-setting)])
               (for [academic-year sc/academic-years
                     from-setting sc/settings
                     need sc/needs]
                 [academic-year (state need from-setting) sc/non-send])
               (for [academic-year sc/academic-years]
                 [academic-year sc/non-send sc/non-send]))
       (filter #(apply valid-transition? %))))

(defn transition-alphas
  "Creates transition alphas based on prior belief and observations"
  [ds y1 y2]
  (let [observations (->> (ds/row-maps ds)
                          (reduce (fn [coll {:keys [academic-year-1 need-1 setting-1 need-2 setting-2]}]
                                    (cond (= setting-1 sc/non-send)
                                          (update coll [academic-year-1 sc/non-send (state need-2 setting-2)] some+ 1)
                                          (= setting-2 sc/non-send)
                                          (update coll [academic-year-1 (state need-1 setting-1) sc/non-send] some+ 1)
                                          :else
                                          (update coll [academic-year-1 (state need-1 setting-1) (state need-1 setting-2)] some+ 1))) {}))
        observations-by-ay (reduce (fn [coll [[academic-year]]]
                                     (update coll academic-year some+ 1))
                                   {} observations)
        non-send (reduce (fn [coll [ay n]]
                           (update coll ay - n))
                         y1 observations-by-ay)
        observations (reduce (fn [coll [ay n]]
                               (assoc coll [ay sc/non-send sc/non-send] n))
                             observations
                             non-send)
        status-quo-multiplier (->> (ds/row-maps ds)
                                   (reduce (fn [[a b] {:keys [setting-1 setting-2]}]
                                             (cond
                                               (or (= setting-1 sc/non-send) (= setting-2 sc/non-send))
                                               [a b]
                                               (= setting-1 setting-2)
                                               [(+ a 1) b]
                                               :else [a (+ b 1)]))
                                           [1 1])
                                   (apply /))
        _ (println "Status Quo multiplier" status-quo-multiplier)
        valid-transition-counts (reduce (fn [coll [academic-year from-state _]]
                                          (update coll [academic-year from-state] some+ 1))
                                        {} valid-transitions)]
    (doto (reduce (fn [coll [academic-year from-state to-state :as k]]
                    (if-let [c (get observations [academic-year from-state to-state])]
                      (assoc-in coll [[academic-year from-state] to-state] (inc (* c (get valid-transition-counts [academic-year from-state]))))
                      (if (= from-state to-state)
                        (assoc-in coll [[academic-year from-state] to-state] status-quo-multiplier)
                        (assoc-in coll [[academic-year from-state] to-state] 1))))
                  {} valid-transitions)
      clojure.pprint/pprint)))

(defn sample-transitions
  "Takes a total count and map of categories to probabilities and
  returns the count in each category at the next step."
  [seed n alphas]
  (let [as (vals alphas)
        xs (draw (dirichlet-multinomial n as) {:seed seed})]
    (zipmap (keys alphas) xs)))

(def total-by-age
  "Given a sequence of {:age age :population population}
  sums the total population for each age"
  (partial reduce (fn [coll {:keys [age population]}]
                    (update coll age some+ population))
           {}))

(def total-by-academic-year
  "Given a sequence of {:academic-year year :population population}
  sums the total population for each year"
  (partial reduce (fn [coll {:keys [academic-year population]}]
                    (update coll academic-year some+ population))
           {}))

(defn model-population-by-ay
  [model]
  (reduce (fn [coll [[ay state] population]]
            (cond-> coll
              (not= state sc/non-send)
              (update ay some+ population)))
          {} model))

(defn model-send-population
  [model]
  (reduce (fn [n [[ay state] population]]
            (cond-> n
              (not= state sc/non-send)
              (+ population)))
          0 model))


;;;; Reducing functions for use with transduce

(defn int-ci-rf
  [number-of-signigicant-digits]
  (fn
    ([] (IntCountsHistogram. number-of-signigicant-digits))
    ([hist x]
     (doto hist (.recordValue x)))
    ([hist]
     {:median (.getValueAtPercentile hist 50.0)
      :mean (.getMean hist)
      :std-dev (.getStdDeviation hist)
      :iqr (- (.getValueAtPercentile hist 75.0) (.getValueAtPercentile hist 25.0))
      :min (.getValueAtPercentile hist 0.0)
      :max (.getValueAtPercentile hist 100.0)
      :q1 (.getValueAtPercentile hist 25.0)
      :q3 (.getValueAtPercentile hist 75.0)
      :low-ci (.getValueAtPercentile hist 2.5)
      :high-ci (.getValueAtPercentile hist 97.5)})))

(def mean-or-zero-rf
  (r/post-complete
   kixi/mean
   (fn [mean]
     (or mean 0.0))))

(def int-summary-rf
  "Returns a summary of a sequence of integers"
  (int-ci-rf 3))

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
  [rf]
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

(defn partition-rf
  "Executes the rf on partitions of length n, returning n results"
  [n rf]
  (fn
    ([] (mapv #(%1) (repeat n rf)))
    ([acc xs]
     (mapv #(rf %1 %2) acc xs))
    ([acc]
     (mapv #(rf %1) acc))))
