(ns witan.send.utils
  (:require [clojure.core.matrix.dataset :as ds]
            [clojure.data.csv :as data-csv]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [kixi.stats.math :as math]
            [medley.core :as medley]
            [schema.coerce :as coerce]
            [witan.send.maths :as m]
            [witan.send.schemas :as sc]
            [witan.send.states :as states])
  (:import org.HdrHistogram.IntCountsHistogram))

(defn blank-row? [row]
  (every? #(= "" %) row))

(defn load-csv
  "Loads csv file with each row as a vector.
   Stored in map separating column-names from data"
  ([filename]
   (let [file (io/file filename)]
     (when (.exists (io/as-file file))
       (let [parsed-csv (with-open [in-file (io/reader file)]
                          (doall (->> in-file
                                      data-csv/read-csv
                                      (remove (fn [row] (blank-row? row))))))
             parsed-data (rest parsed-csv)
             headers (first parsed-csv)]
         {:column-names headers
          :columns (vec parsed-data)})))))

(defn apply-row-schema
  [col-schema csv-data]
  (let [row-schema (sc/make-row-schema col-schema)]
    (map (coerce/coercer row-schema coerce/string-coercion-matcher)
         (:columns csv-data))))

(defn apply-col-names-schema
  [col-schema csv-data]
  (let [col-names-schema (sc/make-col-names-schema col-schema)]
    ((coerce/coercer col-names-schema coerce/string-coercion-matcher)
     (:column-names csv-data))))

(defn apply-schema-coercion [data schema]
  {:column-names (apply-col-names-schema schema data)
   :columns (vec (apply-row-schema schema data))})

(defn csv-to-dataset
  "Takes in a file path and a schema. Creates a dataset with the file
   data after coercing it using the schema."
  [filepath schema]
  (-> (load-csv filepath)
      (apply-schema-coercion schema)
      (as-> {:keys [column-names columns]} (ds/dataset column-names columns))))

(defn transitions-map
  [dataset]
  (->> dataset
       (reduce (fn [coll {:keys [setting-1 need-1 setting-2 need-2 academic-year-2]}]
                 (let [state-1 (states/state need-1 setting-1)
                       state-2 (states/state need-2 setting-2)]
                   (update coll [academic-year-2 state-1 state-2] m/some+ 1)))
               {})))

(defn full-transitions-map
  [dataset]
  (->> dataset
       (reduce (fn [coll {:keys [calendar-year setting-1 need-1 setting-2 need-2 academic-year-2]}]
                 (let [state-1 (states/state need-1 setting-1)
                       state-2 (states/state need-2 setting-2)]
                   (update coll [calendar-year academic-year-2 state-1 state-2] m/some+ 1)))
               {})))

(defn split-need-state [state pos]
  (keyword (pos (str/split (name state) #"-"))))

(defn back-to-transitions-matrix [k v]
  (let [[calendar-year academic-year-2 state-1 state-2] k
        total v]
    (repeat total (assoc {}
                         :calendar-year calendar-year
                         :academic-year-1 (- academic-year-2 1)
                         :academic-year-2 academic-year-2
                         :need-1 (split-need-state state-1 first)
                         :setting-1 (if (nil? (split-need-state state-1 second))
                                      (split-need-state state-1 first)
                                      (split-need-state state-1 second))
                         :need-2 (split-need-state state-2 first)
                         :setting-2 (if (nil? (split-need-state state-2 second))
                                      (split-need-state state-2 first)
                                      (split-need-state state-2 second))))))

(def total-by-academic-year
  "Given a sequence of {:academic-year year :population population}
  sums the total population for each year"
  (partial reduce (fn [coll {:keys [academic-year population]}]
                    (update coll academic-year m/some+ population))
           {}))

(defn model-population-by-ay
  [model]
  (reduce (fn [coll [[ay state] population]]
            (cond-> coll
              (not= state sc/non-send)
              (update ay m/some+ population)))
          {} model))

(defn model-population-by-need
  [model]
  (reduce (fn [coll [[ay state] population]]
            (let [[need setting] (states/need-setting state)]
              (cond-> coll
                (not= state sc/non-send)
                (update need m/some+ population))))
          {} model))

(defn model-population-by-setting
  [model]
  (reduce (fn [coll [[ay state] population]]
            (let [[need setting] (states/need-setting state)]
              (cond-> coll
                (not= state sc/non-send)
                (update setting m/some+ population))))
          {} model))

(defn model-population-by-need-setting
  [model]
  (reduce (fn [coll [[ay state] population]]
            (let [[need setting] (states/need-setting state)]
              (cond-> coll
                (not= state sc/non-send)
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
                (not= state sc/non-send)
                (update ay-group m/some+ population))))
          {} model))

(defn total-need-setting-cost
  [need-setting-lookup population-by-need-setting]
  (-> (reduce (fn [cost [need-setting population]]
                (+ cost (* population (get need-setting-lookup need-setting 0))))
              0 population-by-need-setting)
      m/round))

(defn model-send-population
  [model]
  (reduce (fn [n [[ay state] population]]
            (cond-> n
              (not= state sc/non-send)
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

(defn keep-duplicates [seq]
  (for [[id freq] (frequencies seq)
        :when (> freq 1)]
    id))
