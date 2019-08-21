(ns witan.send.params-test
  (:require [clojure.test :refer [deftest testing is]]
            [witan.send.constants :as c]
            [witan.send.model.input.population :as ip]
            [witan.send.model.input.transitions :as it]
            [witan.send.params :as sut]
            [witan.send.states :as s]))

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

(def population-dataset (ip/csv->population "data/demo/data/population.csv"))

(def transitions (it/csv->transitions "data/demo/data/transitions.csv"))

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
          params (sut/alpha-params-joiners valid-states transitions)]
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
                                state (s/validate-states-for-ay valid-states academic-year)
                                :when (s/can-move? valid-year-settings academic-year
                                                   state valid-transitions)]
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
  (let [result (sut/calculate-population-per-calendar-year population-dataset)]
    (testing "all input calendar years are present"
      (let [expected-calendar-years (->> population-dataset
                                         (map :calendar-year)
                                         (into #{}))]
        (is (= (-> result keys set)
               expected-calendar-years))))
    (testing "each calendar year contains expected academic years"
      (let [expected-academic-years (->> population-dataset
                                         (map :academic-year)
                                         (into #{}))]
        (is (every? #(= (set (keys %))
                        expected-academic-years)
                    (vals result)))))
    (testing "population count matches expectation"
      (is (= (reduce + (map :population population-dataset))
             (reduce + (mapcat vals (vals result))))))))

(deftest beta-params-joiners-test
  []
  (let [result (sut/beta-params-joiners valid-states
                                        transitions
                                        population-dataset)]
    (testing "each val is a valid beta param"
      (is (every? (every-pred :alpha :beta) (vals result)))
      (is (every? (comp (partial every? pos?) vals) (vals result))))))
