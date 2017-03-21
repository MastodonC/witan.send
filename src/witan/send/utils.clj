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

(defn lag
  "Data must be kept chronologically ordered in order to work correctly"
  ([dataset col-key]
   (-> dataset
       (ds/to-map)
       (col-key)
       (#(cons 0 %))
       (butlast)))
  ([dataset col-key lag-amount]
   (-> dataset
       (ds/to-map)
       (col-key)
       (#(concat (repeat lag-amount 0) %))
       (as-> d (drop-last lag-amount d)))))

(defn groups-to-individuals
  [dataset freq-col]
  (let [number-rows (first (:shape dataset))
        row-seq (range 0 number-rows)
        freqs (ds/column dataset freq-col)
        rows-to-select (into [] (reduce concat
                                        (map (fn [freq val] (repeat freq val))
                                             freqs row-seq)))]
    (-> dataset
        (ds/remove-columns [freq-col])
        (ds/select-rows rows-to-select))))

(defn add-simulation-numbers
  [dataset num-simulations]
  (let [number-rows (first (:shape dataset))
        row-seq (range 0 number-rows)
        rows-to-select (into [] (reduce concat
                                        (map (fn [val] (repeat num-simulations val))
                                             row-seq)))
        sim-numbers (into [] (reduce concat (repeat num-simulations (range 1 (inc num-simulations)))))]
    (-> dataset
        (ds/select-rows rows-to-select)
        (ds/add-column :sim-num sim-numbers))))
