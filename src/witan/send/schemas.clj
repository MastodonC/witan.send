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

(def settings
  [:CC :EO :FEC :IMS :IN :ISC :ISS :ISSR :IT :MMS :MSS :OOE :PRU :MU])

(def needs
  [:SLD :ASD :MLD :PD :HI :M :SLCN :PMLD :SEMH :VI :OTH :SPLD :MSI])

(def non-send
  :NON-SEND)

(def states
  (-> (for [need needs setting settings]
        (keyword (str (name need) "-" (name setting))))
      (conj non-send)))

(def academic-years
  (range -5 (inc 22)))

(def Ages
  (range 0 (inc 26)))

(def SendStatesSchema
  (apply s/enum States))

(def AgeSchema (s/constrained s/Int SENDage?))

;; (def YearSchema (s/constrained s/Int SEND-year?))

(def DataForMatrix
  (make-ordered-ds-schema [[:age AgeSchema]
                           [:from-state (s/constrained s/Keyword (fn [s] (some #(= s %) States)))]
                           [:to-state (s/constrained s/Keyword (fn [s] (some #(= s %) States)))]
                           [:probability double]]))
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

(def R-pos
  (s/constrained R pos?))

(def Rgt1
  (s/constrained s/Num pos?))

(def PopulationSYA
  (make-ordered-ds-schema [[:calendar-year (s/constrained s/Int year?)]
                           [:academic-year AcademicYear]
                           [:population s/Int]]))

#_(def TransitionCounts
  (make-ordered-ds-schema [[:academic-year AcademicYear]
                           [:need Need]
                           [:setting Setting]
                           [:CC N]
                           [:EO N]
                           [:FEC N]
                           [:IMS N]
                           [:IN N]
                           [:ISC N]
                           [:ISS N]
                           [:ISSR N]
                           [:IT N]
                           [:MMS N]
                           [:MSS N]
                           [:MU N]
                           [:NON-SEND N]
                           [:OOE N]
                           [:PRU N]]))

(def JoinerCounts
  (make-ordered-ds-schema [[:calendar-year CalendarYear]
                           [:academic-year AcademicYear]
                           [:need Need]
                           [:setting Setting]
                           [:population N]]))

(def TransitionMatrix
  {[(s/one AcademicYear :academic-year)
    (s/one Need :need)
    (s/one Setting :setting)]
   [N]})

(def TransitionCounts
  (make-ordered-ds-schema [[:setting-1 Setting]
                           [:need-1 Need]
                           [:academic-year-1 AcademicYear]
                           [:setting-2 Setting]
                           [:need-2 Need]
                           [:academic-year-2 AcademicYear]]))

(def TransitionAlphas
  {[(s/one AcademicYear :academic-year)
    (s/one State :state)]
   {State R}})

(def SENDPopulation
  (make-ordered-ds-schema [[:calendar-year CalendarYear]
                           [:academic-year AcademicYear]
                           [:need Need]
                           [:setting Setting]
                           [:population N]]))

(def PopulationByAcademicYear
  [{AcademicYear s/Int}])

(def SENDSchema
  {[(s/one AcademicYear :year)
    (s/one Need :need)
    (s/one Setting :setting)]
   N})

(def ModelState
  {[(s/one AcademicYear :academic-year)
    (s/one State :state)]
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

(def SENDOutputSchema1
  [{[(s/one AgeSchema :age)
     (s/one SendStatesSchema :state)]
    StatisticsSchema}])

(def Results
  [{:by-state {[(s/one AcademicYear :academic-year)
                (s/one State :state)]
               StatisticsSchema}
    :total-in-send-by-ay {AcademicYear StatisticsSchema}
    :total-in-send StatisticsSchema
    :total-in-send-by-need {Need StatisticsSchema}
    :total-in-send-by-setting {Setting StatisticsSchema}}])

(def LeaverProbabilities
  {AcademicYear {:alpha s/Num :beta s/Num}})

(def AcademicYearBetaParams
  {AcademicYear {:alpha s/Num :beta s/Num}})

(def BetaParams
  {:alpha s/Num :beta s/Num})

(def StateAlphas
  {State s/Num})

(def AgeAlphas
  {AcademicYear s/Num})
