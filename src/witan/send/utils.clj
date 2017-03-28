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

(defn probs-1 [dataset]
  (reduce merge (map (fn [{:keys [state probability]}] {state probability})
                     (-> dataset
                         (ds/select-columns [:state :probability])
                         ds/row-maps))))

(defn trans-mat [probabilities]
  {[:Non-SEND] probabilities
   [:ASD-Mainstream] probabilities
   [:ASD-Special] probabilities
   [:ASD-Other] probabilities
   [:BESM-Mainstream] probabilities
   [:BESM-Special] probabilities
   [:BESM-Other] probabilities
   [:LD-Mainstream] probabilities
   [:LD-Special] probabilities
   [:LD-Other] probabilities
   [:PSI-Mainstream] probabilities
   [:PSI-Special] probabilities
   [:PSI-Other] probabilities
   [:SLCN-Mainstream] probabilities
   [:SLCN-Special] probabilities
   [:SLCN-Other] probabilities
   [:SPLD-Mainstream] probabilities
   [:SPLD-Special] probabilities
   [:SPLCD-Other] probabilities
   [:UO-Mainstream] probabilities
   [:UO-Special] probabilities
   [:UO-Other] probabilities
   [:Too-old] probabilities})


(defn full-trans-mat [dataset]
  (let [map-probas (probs-1 dataset)
        matrix-proba (trans-mat map-probas)]
    {:1 matrix-proba :2 matrix-proba :3 matrix-proba :4 matrix-proba :5 matrix-proba
     :6 matrix-proba :7 matrix-proba :8 matrix-proba :9 matrix-proba :10 matrix-proba
     :11 matrix-proba :12 matrix-proba :13 matrix-proba :14 matrix-proba :15 matrix-proba
     :16 matrix-proba :17 matrix-proba :18 matrix-proba :19 matrix-proba :20 matrix-proba
     :21 matrix-proba :22 matrix-proba :23 matrix-proba :24 matrix-proba
     :25 matrix-proba :26 matrix-proba}))
