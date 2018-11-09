(ns witan.send.schemas
  (:require [schema.core :as s]
            [witan.send.constants :as c]))

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

(defn State
  [needs settings]
  (->> (-> (for [need needs setting settings]
             (keyword (str (name need) "-" (name setting))))
           (conj c/non-send))
       (apply s/enum)))

(def academic-years
  (range -5 (inc 25)))

(def min-academic-year (apply min academic-years))
(def max-academic-year (apply max academic-years))

(def AcademicYear
  (s/constrained s/Int #(<= min-academic-year % max-academic-year)))

(def N
  (s/constrained s/Int (complement neg?)))

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

(def SettingsToChange
  (make-ordered-ds-schema [[:setting-1 s/Keyword]
                           [:setting-2 s/Keyword]]))

(def SENDPopulation
  (make-ordered-ds-schema [[:calendar-year CalendarYear]
                           [:academic-year AcademicYear]
                           [:need s/Keyword]
                           [:setting s/Keyword]
                           [:population N]]))

(def ValidSettingAcademicYears
  (make-ordered-ds-schema [[:setting s/Keyword]
                           [:setting-group s/Str]
                           [:min-academic-year AcademicYear]
                           [:max-academic-year AcademicYear]
                           [:needs s/Str]
                           [:setting->setting s/Str]]))

(def NeedSettingCost
  (make-ordered-ds-schema [[:need s/Keyword]
                           [:setting s/Keyword]
                           [:cost s/Num]]))

(defn NeedSettingCost+
  [needs settings]
  (make-ordered-ds-schema [[:need (apply s/enum needs)]
                           [:setting (apply s/enum settings)]
                           [:cost s/Num]]))
