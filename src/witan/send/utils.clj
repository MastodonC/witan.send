(ns witan.send.utils
  (:require [clojure.core.matrix.dataset :as ds]
            [clojure.data.avl :as avl]
            [witan.workspace-api.utils :as utils]
            [witan.send.schemas :as sc]
            [kixi.stats.random :refer [categorical sample-summary]]))

(defn round [x]
  (Math/round (double x)))

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

(def empty-transition-probabilities
  "Creates a full transition matrix where all transition probabilities are 0.0."
  (let [empty-transitions (->> (interleave sc/States (repeat 0.0))
                               (apply avl/sorted-map))]
    (->> (for [age sc/Ages
               from-state sc/States]
           [age from-state])
         (reduce (fn [coll [age from-state]]
                   (assoc coll [age from-state] empty-transitions))
                 {}))))

(defn transition-probabilities
  "Takes a transition matrix dataset and returns a map of maps in the format
  expected when applying transitions.
  NOTE: Any state transition not supplied in the dataset will assumed to have zero probability"
  [ds]
  (->> (ds/row-maps ds)
       (reduce (fn [coll {:keys [age from-state to-state probability]}]
                 (update coll [age from-state] assoc
                         to-state (max 0.0 probability)))
               empty-transition-probabilities)))

(defn full-trans-mat
  "This returns a transition matrix in the format needed to apply the state changes.
  NOTE: Any state transition not supplied in the dataset will assumed to have zero probability"
  [_ _  dataset]
  (transition-probabilities dataset))

(defn sample-transitions
  "Takes a total count and map of categories to probabilities and
  returns the count in each category at the next step."
  [n [ks ps]]
  (->> (categorical ks ps)
       (sample-summary n)))
