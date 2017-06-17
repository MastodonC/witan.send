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
            [clojure.string :as str])
  (:import [org.HdrHistogram IntCountsHistogram DoubleHistogram]))

(defn round [x]
  (Math/round (double x)))

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

(defn valid-transition? [academic-year state-1 state-2]
  (let [[need-1 setting-1] (need-setting state-1)
        [need-2 setting-2] (need-setting state-1)]
    (and (valid-year-setting? academic-year setting-1)
         (valid-year-setting? (inc academic-year) setting-2))))

(defn transition-alphas
  "Creates a full transition matrix where all alphas are 1.0."
  [ds]
  (let [observations (->> (ds/row-maps ds)
                          (reduce (fn [coll {:keys [year-1 need-1 setting-1 setting-2]}]
                                    (update coll [year-1 (state need-1 setting-1) (state need-1 setting-2)] some+ 1))) {})]
    (doto (->> (concat (for [academic-year sc/academic-years
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
                         [academic-year (state need from-setting) sc/non-send]))
               (reduce (fn [coll [academic-year from-state to-state :as k]]
                         (if (valid-transition? academic-year from-state to-state)
                           (if-let [c (get observations k)]
                             (assoc-in coll [[academic-year from-state] to-state] (inc c))
                             (assoc-in coll [[academic-year from-state] to-state] 1))
                           coll))
                       {}))
      #_clojure.pprint/pprint)))

(defn sample-transitions
  "Takes a total count and map of categories to probabilities and
  returns the count in each category at the next step."
  [n alphas]
  (let [as (vals alphas)
        xs (draw (dirichlet-multinomial n as))]
    (zipmap (keys alphas) xs)))

(defn minus-or-zero [a b]
  (max (- a b) 0))

(defn subtract-map
  "Unlike (merge-with - &args), only returns keys from the first map"
  [a b]
  (reduce (fn [coll k]
            (update coll k minus-or-zero (get b k)))
          a (keys a)))

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


;;;; Reducing functions for use with transduce

(defn int-ci-rf
  [number-of-signigicant-digits]
  (fn
    ([] (IntCountsHistogram. number-of-signigicant-digits))
    ([hist x]
     (doto hist (.recordValue x)))
    ([hist]
     {:median (.getValueAtPercentile hist 50.0)
      :low-ci (.getValueAtPercentile hist 2.5)
      :high-ci (.getValueAtPercentile hist 97.5)})))

(def mean-or-zero-rf
  (r/post-complete
   kixi/mean
   (fn [mean]
     (or mean 0.0))))

(def int-summary-rf
  "Returns a summary of a sequence of integers"
  (r/post-complete
   (r/fuse {:mean mean-or-zero-rf
            :ci (int-ci-rf 3)})
   (fn [{:keys [mean ci]}]
     (assoc ci :mean mean))))

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

(defn partition-rf
  "Executes the rf on partitions of length n, returning n results"
  [n rf]
  (fn
    ([] (mapv #(%1) (repeat n rf)))
    ([acc xs]
     (mapv #(rf %1 %2) acc xs))
    ([acc]
     (mapv #(rf %1) acc))))
