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
  {})
;;dataset with Year, Age, Population

(def TransitionMatrix
  {})
;;map with age keys and corresponding transition matrices- see markov demo. Actual placement types are: Mainstream, Special, Other. Actual need types are: ASD, SLCN, LD, SPLD, BESM, PSI, OTHER. 

(def SENDSchemaGrouped
  {})
;;dataset with year, age, need type, placement type, population

(def SENDSchemaIndividual
  {})
;;dataset with at minimum simulation number, id,  year, age, state. Possibly need type and placement type as well, otherwise these can be worked out from state at end (or converted to state at beginning)

(def SENDSchemaGroupedWithCI
  {})
;;dataset with year, age, state, mean population, 95% CI lower bound, 95% CI upper bound (gets rid of simulation number from SENDSchemaGrouped)

(def CostProfile
  {})
;;depends on what data the LAs can provide. simplest is average cost per pupil. most complex is cost for each need type + placement type combo.

(def YearlyCost
  {})
;;dataset with year, mean total cost = mean population * cost profile, 95% CI lower bound for cost = 95% lower CI for population * cost profile, 95% CI upper bound for cost = 95% upper CI for population * cost profile
