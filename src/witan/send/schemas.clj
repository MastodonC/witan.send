(ns witan.send.schemas
  (:require [schema.core :as s]
            [schema-contrib.core :as sc]))

(defn year? [n] (and (>= n 1900) (<= n 2100)))

(defn SENDage? [n] (and (>= n 0) (<= n 26)))

(defn make-ordered-ds-schema [col-vec]
  {:column-names (mapv #(s/one (s/eq (first %)) (str (first %))) col-vec)
   :columns (mapv #(s/one [(second %)] (format "col %s" (name (first %)))) col-vec)
   s/Keyword s/Any})

(defn make-row-schema
  [col-schema]
  (mapv (fn [s] (let [datatype (-> s :schema first)
                      fieldname (:name s)]
                  (s/one datatype fieldname)))
        (:columns col-schema)))

(defn make-col-names-schema
  [col-schema]
  (mapv (fn [s] (let [datatype (:schema s)
                      fieldname (:name s)]
                  (s/one datatype fieldname)))
        (:column-names col-schema)))

(def YearSchema
  (s/constrained s/Int year?))

(def PopulationSYA
  (make-ordered-ds-schema [[:year (s/constrained s/Int year?)]
                           [:age (s/constrained s/Int SENDage?)]
                           [:population s/Int]]))

(def States [:Non-SEND
             :ASD-Mainstream
             :ASD-Special
             :ASD-Other
             :BESM-Mainstream
             :BESM-Special
             :BESM-Other
             :LD-Mainstream
             :LD-Special
             :LD-Other
             :PSI-Mainstream
             :PSI-Special
             :PSI-Other
             :SLCN-Mainstream
             :SLCN-Special
             :SLCN-Other
             :SPLD-Mainstream
             :SPLD-Special
             :SPLD-Other
             :UO-Mainstream
             :UO-Special
             :UO-Other
             :Too-old])

(def Ages
  (range 0 (inc 26)))

(def SendStatesSchema
  (apply s/enum States))

(def AgeSchema (s/constrained s/Int SENDage?))

(def TransitionMatrixSchema
  {[(s/one AgeSchema :age)
    (s/one SendStatesSchema :state)]
   {s/Num SendStatesSchema}})

(def DataForMatrix
  (make-ordered-ds-schema [[:age AgeSchema]
                           [:from-state (s/constrained s/Keyword (fn [s] (some #(= s %) States)))]
                           [:to-state (s/constrained s/Keyword (fn [s] (some #(= s %) States)))]
                           [:probability double]]))
(def SENDSchemaGrouped
  (make-ordered-ds-schema [[:year (s/constrained s/Int year?)]
                           [:age (s/constrained s/Int SENDage?)]
                           [:need s/Str]
                           [:placement s/Str]
                           [:population s/Int]]))

(def PopulationDeltas
  [{AgeSchema s/Num}])

(def SENDSchema
  {[(s/one AgeSchema :age)
    (s/one SendStatesSchema :state)]
   (s/constrained s/Int (complement neg?))})

(def StatisticsSchema
  {:mean s/Num
   :median s/Num
   :quantiles [s/Num]})

(def SENDOutputSchema1
  [{[(s/one AgeSchema :age)
     (s/one SendStatesSchema :state)]
    StatisticsSchema}])
