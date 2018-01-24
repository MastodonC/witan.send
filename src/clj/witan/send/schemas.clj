(ns witan.send.schemas
  (:require [schema.core :as s]
            [schema-contrib.core :as sc]))

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

(def CalendarYear
  (s/constrained s/Int #(<= 1900 % 2100)))

(def non-send :NONSEND)

(defn State
  [needs settings]
  (->> (-> (for [need needs setting settings]
             (keyword (str (name need) "-" (name setting))))
           (conj non-send))
       (apply s/enum)))

(def academic-years
  (range -5 (inc 25)))

(def min-academic-year (apply min academic-years))
(def max-academic-year (apply max academic-years))

(def AcademicYear
  (s/constrained s/Int #(<= min-academic-year % max-academic-year)))

(def YearSchema
  (s/constrained s/Int #(<= 2000 % 2100)))

(defn Need [needs]
  (apply s/enum (conj needs non-send)))

(defn Setting [settings]
  (apply s/enum (conj settings non-send)))

(def N
  (s/constrained s/Int (complement neg?)))

(def R s/Num)

(def PopulationDataset
  (make-ordered-ds-schema [[:calendar-year CalendarYear]
                           [:academic-year AcademicYear]
                           [:population s/Int]]))

(def TransitionCounts
  (make-ordered-ds-schema [[:calendar-year s/Int]
                           [:setting-1 s/Keyword]
                           [:need-1 s/Keyword]
                           [:academic-year-1 AcademicYear]
                           [:setting-2 s/Keyword]
                           [:need-2 s/Keyword]
                           [:academic-year-2 AcademicYear]]))

(defn TransitionsMap+
  [needs settings]
  (let [State (State needs settings)]
    {[(s/one AcademicYear :academic-year)
      (s/one State :state-1)
      (s/one State :state-2)]
     N}))

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

(defn SENDPopulation+ [settings]
  (make-ordered-ds-schema [[:calendar-year CalendarYear]
                           [:academic-year AcademicYear]
                           [:need s/Keyword]
                           [:setting (Setting settings)]
                           [:population N]]))

(def ValidSettingAcademicYears
  (make-ordered-ds-schema [[:setting s/Keyword]
                           [:setting->group s/Str]
                           [:min-academic-year AcademicYear]
                           [:max-academic-year AcademicYear]
                           [:needs s/Str]]))

(def PopulationByCalendarAndAcademicYear
  {CalendarYear {AcademicYear N}})

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

(def JoinerBetaParams
  {AcademicYear BetaParams})

(def YearStateBetaParams
  {[(s/one AcademicYear :academic-year)
    (s/one s/Keyword :state)]
   BetaParams})

(def SettingCost
  (make-ordered-ds-schema [[:setting s/Keyword]
                           [:cost s/Num]]))

(def NeedSettingCost
  (make-ordered-ds-schema [[:need s/Keyword]
                           [:setting s/Keyword]
                           [:cost s/Num]]))

(defn SettingCost+
  [settings]
  (prn settings)
  (make-ordered-ds-schema [[:setting (apply s/enum settings)]
                           [:cost s/Num]]))

(defn NeedSettingCost+
  [needs settings]
  (make-ordered-ds-schema [[:need (apply s/enum needs)]
                           [:setting (apply s/enum settings)]
                           [:cost s/Num]]))

(def SettingCostLookup
  {[(s/one s/Keyword :need)
    (s/one s/Keyword :setting)]
   s/Num})
