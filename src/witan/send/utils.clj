(ns witan.send.utils
  (:require [clojure.core.matrix.dataset :as ds]
            [witan.workspace-api.utils :as utils]))

(defn year? [n] (and (>= n 1900) (<= n 2100)))

(defn SENDage? [n] (and (>= n 0) (<= n 26)))

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

(defn get-matrix-prob-for-age
  [age fields dataset]
  (let [age-rows (->> (ds/column dataset :age)
                      (filter-indexed #(when (= %2 age) %1))
                      (ds/select-rows dataset)
                      (ds/row-maps))]
    (reduce (fn [acc field]
              (let [field-rows (filter #(= (:from-state %) field) age-rows)]
                (assoc acc [field]
                       (reduce (fn [acc row]
                                 (assoc acc (:to-state row) (:probability row)))
                               {} field-rows)))) {} fields)))

(defn full-trans-mat [fields [min-age max-age] dataset]
  (reduce (fn [acc age]
            (assoc acc (-> age str keyword) (get-matrix-prob-for-age age fields dataset)))
          {} (range min-age (inc max-age))))
