(ns witan.send.schemas
  (:require [schema.core :as s]
            [schema-contrib.core :as sc]
            [witan.send.utils :as u]))

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
  (s/constrained s/Int u/year?))

(def PopulationSYA
  (make-ordered-ds-schema [[:year (s/constrained s/Int u/year?)]
                           [:age (s/constrained s/Int u/SENDage?)]
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
             :SPLCD-Other
             :UO-Mainstream
             :UO-Special
             :UO-Other
             :Too-old])

(def SendStatesSchema
  [(apply s/enum States)])

(def ProbabilitiesSchema
  (reduce merge
          (map #(hash-map % double) States)))

(def TransitionMatrixSchema
  {SendStatesSchema ProbabilitiesSchema})

(def AgeGroupsSchema
  (apply s/enum (map #((comp keyword str) %) (range 1 27))))

(def TransitionMatrix
  {AgeGroupsSchema TransitionMatrixSchema})

(def DataForMatrix
  (make-ordered-ds-schema [[:age (apply s/enum (conj (range 1 27) "all"))]
                           [:state (s/constrained s/Keyword (fn [s] (some #(= s %) States)))]
                           [:probability double]]))

(def SENDSchemaGrouped
  (make-ordered-ds-schema [[:year (s/constrained s/Int u/year?)]
                           [:age (s/constrained s/Int u/SENDage?)]
                           [:need s/Str]
                           [:placement s/Str]
                           [:population s/Int]]))

(def SENDSchemaIndividual
  (make-ordered-ds-schema [[:year (s/constrained s/Int u/year?)]
                           [:age (s/constrained s/Int u/SENDage?)]
                           [:state s/Keyword]
                           [:sim-num s/Int]
                           [:id s/Int]]))

(def SENDSchemaGroupedWithCI
  (make-ordered-ds-schema [[:year (s/constrained s/Int u/year?)]
                           [:age (s/constrained s/Int u/SENDage?)]
                           [:need s/Str]
                           [:placement s/Str]
                           [:population double]
                           [:lower-bound-95-CI double]
                           [:upper-bound-95-CI double]]))

(def CostProfile
  (make-ordered-ds-schema [[:need s/Str]
                           [:placement s/Str]
                           [:cost-per-pupil double]]))

(def YearlyCost
  (make-ordered-ds-schema [[:year (s/constrained s/Int u/year?)]
                           [:need s/Str]
                           [:placement s/Str]
                           [:cost double]]))
