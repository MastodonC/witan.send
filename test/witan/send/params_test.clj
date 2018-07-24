(ns witan.send.params-test
  (:require [clojure.core.matrix.dataset :as ds]
            [clojure.test :refer :all]
            [witan.send.constants :as c]
            [witan.send.params :as sut]
            [witan.send.schemas :as sc]
            [witan.send.states :as s]
            [witan.send.test-utils :as tu]))

(def valid-setting-academic-years
  [{:setting :CC, :setting->setting "CC,EO,FEC,IMS,IN,ISC,ISCR,ISS,ISSR,IT,MMS,MSS,MU,OOE,PRU" :min-academic-year -4, :max-academic-year 0, :needs "ASD,HI,M,MLD,MSI,OTH,PD,PMLD,SEMH,SLCN,SLD,SPLD,VI"}
   {:setting :EO, :setting->setting "CC,EO,FEC,IMS,IN,ISC,ISCR,ISS,ISSR,IT,MMS,MSS,MU,OOE,PRU" :min-academic-year -1, :max-academic-year 14, :needs "ASD,HI,M,MLD,MSI,OTH,PD,PMLD,SEMH,SLCN,SLD,SPLD,VI"}
   {:setting :FEC, :setting->setting "CC,EO,FEC,IMS,IN,ISC,ISCR,ISS,ISSR,IT,MMS,MSS,MU,OOE,PRU" :min-academic-year 12, :max-academic-year 20, :needs "ASD,HI,M,MLD,MSI,OTH,PD,PMLD,SEMH,SLCN,SLD,SPLD,VI"}
   {:setting :IMS, :setting->setting "CC,EO,FEC,IMS,IN,ISC,ISCR,ISS,ISSR,IT,MMS,MSS,MU,OOE,PRU" :min-academic-year -3, :max-academic-year 14, :needs "ASD,HI,M,MLD,MSI,OTH,PD,PMLD,SEMH,SLCN,SLD,SPLD,VI"}
   {:setting :IN, :setting->setting "CC,EO,FEC,IMS,IN,ISC,ISCR,ISS,ISSR,IT,MMS,MSS,MU,OOE,PRU" :min-academic-year -4, :max-academic-year 0, :needs "ASD,HI,M,MLD,MSI,OTH,PD,PMLD,SEMH,SLCN,SLD,SPLD,VI"}
   {:setting :ISC, :setting->setting "CC,EO,FEC,IMS,IN,ISC,ISCR,ISS,ISSR,IT,MMS,MSS,MU,OOE,PRU" :min-academic-year 15, :max-academic-year 20, :needs "ASD,HI,M,MLD,MSI,OTH,PD,PMLD,SEMH,SLCN,SLD,SPLD,VI"}
   {:setting :ISCR, :setting->setting "CC,EO,FEC,IMS,IN,ISC,ISCR,ISS,ISSR,IT,MMS,MSS,MU,OOE,PRU" :min-academic-year 15, :max-academic-year 20, :needs "ASD,HI,M,MLD,MSI,OTH,PD,PMLD,SEMH,SLCN,SLD,SPLD,VI"}
   {:setting :ISS, :setting->setting "CC,EO,FEC,IMS,IN,ISC,ISCR,ISS,ISSR,IT,MMS,MSS,MU,OOE,PRU" :min-academic-year 0, :max-academic-year 14, :needs "ASD,HI,M,MLD,MSI,OTH,PD,PMLD,SEMH,SLCN,SLD,SPLD,VI"}
   {:setting :ISSR, :setting->setting "CC,EO,FEC,IMS,IN,ISC,ISCR,ISS,ISSR,IT,MMS,MSS,MU,OOE,PRU" :min-academic-year 0, :max-academic-year 14, :needs "ASD,HI,M,MLD,MSI,OTH,PD,PMLD,SEMH,SLCN,SLD,SPLD,VI"}
   {:setting :IT, :setting->setting "CC,EO,FEC,IMS,IN,ISC,ISCR,ISS,ISSR,IT,MMS,MSS,MU,OOE,PRU" :min-academic-year 2, :max-academic-year 15, :needs "ASD,HI,M,MLD,MSI,OTH,PD,PMLD,SEMH,SLCN,SLD,SPLD,VI"}
   {:setting :MMS, :setting->setting "CC,EO,FEC,IMS,IN,ISC,ISCR,ISS,ISSR,IT,MMS,MSS,MU,OOE,PRU" :min-academic-year -3, :max-academic-year 14, :needs "ASD,HI,M,MLD,MSI,OTH,PD,PMLD,SEMH,SLCN,SLD,SPLD,VI"}
   {:setting :MSS, :setting->setting "CC,EO,FEC,IMS,IN,ISC,ISCR,ISS,ISSR,IT,MMS,MSS,MU,OOE,PRU" :min-academic-year -3, :max-academic-year 14, :needs "ASD,HI,M,MLD,MSI,OTH,PD,PMLD,SEMH,SLCN,SLD,SPLD,VI"}
   {:setting :MU, :setting->setting "CC,EO,FEC,IMS,IN,ISC,ISCR,ISS,ISSR,IT,MMS,MSS,MU,OOE,PRU" :min-academic-year -1, :max-academic-year 14, :needs "ASD,HI,M,MLD,MSI,OTH,PD,PMLD,SEMH,SLCN,SLD,SPLD,VI"}
   {:setting :OOE, :setting->setting "CC,EO,FEC,IMS,IN,ISC,ISCR,ISS,ISSR,IT,MMS,MSS,MU,OOE,PRU" :min-academic-year 6, :max-academic-year 20, :needs "ASD,HI,M,MLD,MSI,OTH,PD,PMLD,SEMH,SLCN,SLD,SPLD,VI"}
   {:setting :PRU, :setting->setting "CC,EO,FEC,IMS,IN,ISC,ISCR,ISS,ISSR,IT,MMS,MSS,MU,OOE,PRU" :min-academic-year 2, :max-academic-year 14, :needs "ASD,HI,M,MLD,MSI,OTH,PD,PMLD,SEMH,SLCN,SLD,SPLD,VI"}])

(def population-dataset
  (tu/csv-to-dataset "data/demo/data/population.csv" sc/PopulationDataset))

(def transitions-matrix
  (tu/csv-to-dataset "data/demo/data/transitions.csv" sc/TransitionCounts))

(def valid-states
  (-> valid-setting-academic-years
      (s/calculate-valid-states-from-setting-academic-years)))

(def valid-transitions
  (s/calculate-valid-mover-transitions valid-setting-academic-years))

(def valid-year-settings
  (s/calculate-valid-year-settings-from-setting-academic-years valid-setting-academic-years))

(def academic-years
  (s/calculate-academic-year-range valid-setting-academic-years))

(deftest validate-params
  (testing "Positive joiner state alphas for every valid academic year"
    (let [transitions {}
          params (sut/alpha-params-joiner-states valid-states transitions)]
      (is (every? (fn [[academic-year alphas]]
                    (and (pos? (count alphas))
                         (->> alphas vals (every? pos?))))
                  params))))

  (testing "Positive mover state alphas for every year with >1 setting"
    (let [transitions {}
          params (sut/alpha-params-movers valid-states valid-transitions transitions)]
      (is (empty? (remove (fn [[academic-year state]]
                            (let [alphas (get params [academic-year state])]
                              (and (pos? (count alphas))
                                   (->> alphas vals (every? pos?)))))
                          (for [academic-year c/academic-years
                                state (s/valid-states-for-ay valid-states academic-year)
                                :when (s/can-move? valid-year-settings academic-year state)]
                            [academic-year state]))))))

  (testing "Positive leaver beta params for every valid state"
    (let [transitions {}
          params (sut/beta-params-leavers valid-states transitions)]
      (is (every? (fn [[_ betas]]
                    (and (pos? (:alpha betas))
                         (pos? (:beta betas))))
                  params))))

  (testing "Positive mover beta params for every valid state"
    (let [transitions {}
          params (sut/beta-params-movers valid-states valid-transitions transitions)]
      (is (every? (fn [[_ betas]]
                    (and (pos? (:alpha betas))
                         (pos? (:beta betas))))
                  params)))))

(deftest calculate-population-per-calendar-year-test
  (let [population (ds/row-maps population-dataset)
        result (sut/calculate-population-per-calendar-year population)]
    (testing "all input calendar years are present"
      (let [expected-calendar-years (->> population-dataset
                                         ds/row-maps
                                         (map :calendar-year)
                                         (into #{}))]
        (is (= (-> result keys set)
               expected-calendar-years))))
    (testing "each calendar year contains expected academic years"
      (let [expected-academic-years (->> population-dataset
                                         ds/row-maps
                                         (map :academic-year)
                                         (into #{}))]
        (is (every? #(= (set (keys %))
                        expected-academic-years)
                    (vals result)))))
    (testing "population count matches expectation"
      (is (= (reduce + (map :population population))
             (reduce + (mapcat vals (vals result))))))))

(deftest beta-params-joiners-test
  []
  (let [population-row-maps (->> population-dataset
                                 ds/row-maps)
        transitions-matrix (->> transitions-matrix
                                ds/row-maps)
        expected-academic-years (->> population-row-maps
                                     (map :academic-year)
                                     (into #{}))
        result (sut/beta-params-joiners valid-states
                                        transitions-matrix
                                        population-row-maps)]
    (testing "each val is a valid beta param"
      (is (every? (every-pred :alpha :beta) (vals result)))
      (is (every? (comp (partial every? pos?) vals) (vals result))))))
