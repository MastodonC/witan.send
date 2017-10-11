(ns witan.send.schemas
  (:require [schema.core :as s]
            [schema-contrib.core :as sc]))

(defn year? [n] (and (>= n 1900) (<= n 2100)))

(defn SENDage? [n] (and (>= n 0) (<= n 26)))

(defn SEND-year? [y]
  (<= -3 y 22))

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

(def CalendarYear
  (s/constrained s/Int #(<= 1900 % 2100)))

(def settings
  [:EO :EYS :FEC :IMS :ISS :ISSR :MAP :MMS :MMSIB :MMSOB :MSS :MSSIB :MSSOB :MSSOP :MSSR :MU :MUOB :NMSS :NMSSR :OOE])
 
(def needs
  ;; Detailed needs
  #_[:ASD :CI :CL :HI :MLD :MSI :OTH :PD :PMLD :SEMH :SLCN :SLD :SP :SPLD :VI :UKN]
  ;; Broad need categories:
  [:CI :CL :OTH :SEMH :SP :UKN])

(def non-send :NON-SEND)

(def states
  (-> (for [need needs setting settings]
        (keyword (str (name need) "-" (name setting))))
      (conj non-send)))

(def academic-years
  (range -5 (inc 22)))

(def Ages
  (range 0 (inc 26)))

(def AgeSchema (s/constrained s/Int SENDage?))

(def AcademicYear
  (s/constrained s/Int #(<= -5 % 25)))

(def Need
  (apply s/enum (conj needs non-send)))

(def Setting
  (apply s/enum (conj settings non-send)))

(def State
  (apply s/enum states))

(def N
  (s/constrained s/Int (complement neg?)))

(def R s/Num)

(def PopulationDataset
  (make-ordered-ds-schema [[:calendar-year (s/constrained s/Int year?)]
                           [:academic-year AcademicYear]
                           [:population s/Int]]))

(def TransitionCounts
  (make-ordered-ds-schema [[:setting-1 s/Keyword]
                           [:need-1 s/Keyword]
                           [:academic-year-1 AcademicYear]
                           [:setting-2 s/Keyword]
                           [:need-2 s/Keyword]
                           [:academic-year-2 AcademicYear]]))

(def TransitionAlphas
  {[(s/one AcademicYear :academic-year)
    (s/one s/Keyword :state)]
   {s/Keyword R}})

(def SENDPopulation
  (make-ordered-ds-schema [[:calendar-year CalendarYear]
                           [:academic-year AcademicYear]
                           [:need s/Keyword]
                           [:setting s/Keyword]
                           [:population N]]))

(def ValidSettingAcademicYears
  (make-ordered-ds-schema [[:setting s/Keyword]
                           [:min-academic-year AcademicYear]
                           [:max-academic-year AcademicYear]]))

(def PopulationByAcademicYear
  [{AcademicYear s/Int}])

(def ModelState
  {[(s/one AcademicYear :academic-year)
    (s/one s/Keyword :state)]
   N})

(def StatisticsSchema
  {:mean s/Num
   :median s/Num
   :low-ci s/Num
   :high-ci s/Num
   :min s/Num
   :max s/Num
   :std-dev s/Num
   :iqr s/Num
   :q1 s/Num
   :q3 s/Num})

(def Results
  [{:by-state {[(s/one AcademicYear :academic-year)
                (s/one s/Keyword :state)]
               StatisticsSchema}
    :total-in-send-by-ay {AcademicYear StatisticsSchema}
    :total-in-send StatisticsSchema
    :total-in-send-by-need {s/Keyword StatisticsSchema}
    :total-in-send-by-setting {s/Keyword StatisticsSchema}
    :total-cost StatisticsSchema
    :total-in-send-by-ay-group {s/Str StatisticsSchema}}])

(def StateAlphas
  {s/Keyword s/Num})

(def AcademicYearStateAlphas
  {AcademicYear StateAlphas})

(def BetaParams
  {:alpha s/Num :beta s/Num})

(def YearStateBetaParams
  {[(s/one AcademicYear :academic-year)
    (s/one s/Keyword :state)]
   BetaParams})

(def AgeAlphas
  {AcademicYear s/Num})

(def SettingCost
  (make-ordered-ds-schema [[:setting s/Keyword]
                           [:cost s/Num]]))

(def SettingCostLookup
  {s/Keyword s/Num})

(def min-academic-year (apply min academic-years))
(def max-academic-year (apply max academic-years))
