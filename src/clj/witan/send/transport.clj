(ns witan.send.transport
  (:require [witan.send.schemas :as sc]
            [witan.send.test-utils :as tu]
            [clojure.core.matrix.dataset :as ds]
            [kixi.stats.random :as r]
            [schema.core :as schema]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [gg4clj.core :as gg4clj]
            )
  (:import [org.HdrHistogram DoubleHistogram]))

;; Calculate better confidence intervals around transport costs

(def TransportCounts
  (sc/make-ordered-ds-schema [[:state schema/Keyword]
                              [:none sc/N]
                              [:internal sc/N]
                              [:external sc/N]
                              [:none-proportion sc/R]
                              [:internal-proportion sc/R]
                              [:external-proportion sc/R]]))

(def OutputAYState
  (sc/make-ordered-ds-schema [[:calendar-year sc/N]
                              [:academic-year schema/Num]
                              [:state schema/Keyword]
                              [:mean sc/R]
                              [:std-dev sc/R]
                              [:iqr sc/N]
                              [:min sc/N]
                              [:low-ci sc/N]
                              [:q1 sc/N]
                              [:median sc/N]
                              [:q3 sc/N]
                              [:high-ci sc/N]
                              [:max sc/N]]))

(def proportion-lookup
  (->> (tu/csv-to-dataset "data/transport/A-new-transport-counts.csv" TransportCounts)
       (ds/row-maps)
       (map (juxt :state identity))
       (into {})))

(defn sample-truncated-normal
  "We don't have enough information to re-create a beta-binomial distribution, so we'll sample from a normal distribution truncated at zero instead. This should be good enough for our purposes"
  [{:keys [mean std-dev]}]
  (max 0 (r/draw (r/normal {:mu mean :sd std-dev}))))

(defn sq [x]
  (* x x))

(defn sample-binomial
  [{:keys [mean std-dev]}]
  (if (> mean 0)
    (let [p (- 1 (/ (sq std-dev) mean))]
      (if (> p 0)
        (let [n (/ mean p)]
          (r/draw (r/binomial {:n n :p p})))
        0.0))
    0.0))

(def internal-avg-cost
  (/ 2478918 419)) ;; check number of individuals

(def external-avg-cost
  (/ 1575773 229))

(defn sample-cost
  [{:keys [state median std-dev]}]
  (let [n (sample-truncated-normal {:mean median :std-dev std-dev})
        {:keys [internal-proportion external-proportion] :or {internal-proportion 0 external-proportion 0}} (get proportion-lookup state)]
    (+ (* internal-proportion n internal-avg-cost)
       (* external-proportion n external-avg-cost))))


(def histogram-reducer
  (fn
    ([] (DoubleHistogram. 4))
    ([acc x]
     (doto acc (.recordValue (inc x))))
    ([hist]
     {:median (dec (.getValueAtPercentile hist 50.0))
      :mean (dec (.getMean hist))
      :std-dev (.getStdDeviation hist)
      :iqr (- (.getValueAtPercentile hist 75.0) (.getValueAtPercentile hist 25.0))
      :min (dec (.getValueAtPercentile hist 0.0))
      :max (dec (.getValueAtPercentile hist 100.0))
      :q1 (dec (.getValueAtPercentile hist 25.0))
      :q3 (dec (.getValueAtPercentile hist 75.0))
      :low-ci (dec (.getValueAtPercentile hist 2.5))
      :high-ci (dec (.getValueAtPercentile hist 97.5))})))


(defn calculate-ci
  "Given a stochastic function f, calculates the median, and 95% CI"
  [f]
  (transduce identity histogram-reducer (repeatedly 100 f)))


(def output (for [calendar-year [2017 2018 2019 2020 2021 2022 2023 2024 2025 2026 2027]]
              (let [data (->> (tu/csv-to-dataset "/Users/mike/witan.send/data/transport/TH-VersionA-baseline-29-03-18-AY-State.csv" OutputAYState)
                              (ds/row-maps)
                              (filter #(= (:calendar-year %) calendar-year)))
                    f #(->> (map sample-cost data)
                            (reduce +))]
                {:calendar-year calendar-year :hist (calculate-ci f)})))


(defn collapse-map [m]
  (assoc (:hist m) :calendar-year (:calendar-year m)))

(with-open [writer (io/writer (io/file "data/transport/Transport-Output.csv"))]
  (let [columns [:calendar-year :mean :std-dev :iqr :min :low-ci :q1 :median :q3 :high-ci :max]
        headers (mapv name columns)
        rows (mapv #(mapv % columns) (map collapse-map output))]
    (csv/write-csv writer (into [headers] rows))))

