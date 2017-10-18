(ns witan.send.params-test
  (:require [clojure.test :refer :all]
            [witan.send.params :as sut]
            [witan.send.constants :as c]
            [witan.send.states :as s]))

(def valid-setting-academic-years
  [{:setting :CC, :min-academic-year -4, :max-academic-year 0, :needs "ASD,HI,M,MLD,MSI,OTH,PD,PMLD,SEMH,SLCN,SLD,SPLD,VI"}
   {:setting :EO, :min-academic-year -1, :max-academic-year 14, :needs "ASD,HI,M,MLD,MSI,OTH,PD,PMLD,SEMH,SLCN,SLD,SPLD,VI"}
   {:setting :FEC, :min-academic-year 12, :max-academic-year 20, :needs "ASD,HI,M,MLD,MSI,OTH,PD,PMLD,SEMH,SLCN,SLD,SPLD,VI"}
   {:setting :IMS, :min-academic-year -3, :max-academic-year 14, :needs "ASD,HI,M,MLD,MSI,OTH,PD,PMLD,SEMH,SLCN,SLD,SPLD,VI"}
   {:setting :IN, :min-academic-year -4, :max-academic-year 0, :needs "ASD,HI,M,MLD,MSI,OTH,PD,PMLD,SEMH,SLCN,SLD,SPLD,VI"}
   {:setting :ISC, :min-academic-year 15, :max-academic-year 20, :needs "ASD,HI,M,MLD,MSI,OTH,PD,PMLD,SEMH,SLCN,SLD,SPLD,VI"}
   {:setting :ISCR, :min-academic-year 15, :max-academic-year 20, :needs "ASD,HI,M,MLD,MSI,OTH,PD,PMLD,SEMH,SLCN,SLD,SPLD,VI"}
   {:setting :ISS, :min-academic-year 0, :max-academic-year 14, :needs "ASD,HI,M,MLD,MSI,OTH,PD,PMLD,SEMH,SLCN,SLD,SPLD,VI"}
   {:setting :ISSR, :min-academic-year 0, :max-academic-year 14, :needs "ASD,HI,M,MLD,MSI,OTH,PD,PMLD,SEMH,SLCN,SLD,SPLD,VI"}
   {:setting :IT, :min-academic-year 2, :max-academic-year 15, :needs "ASD,HI,M,MLD,MSI,OTH,PD,PMLD,SEMH,SLCN,SLD,SPLD,VI"}
   {:setting :MMS, :min-academic-year -3, :max-academic-year 14, :needs "ASD,HI,M,MLD,MSI,OTH,PD,PMLD,SEMH,SLCN,SLD,SPLD,VI"}
   {:setting :MSS, :min-academic-year -3, :max-academic-year 14, :needs "ASD,HI,M,MLD,MSI,OTH,PD,PMLD,SEMH,SLCN,SLD,SPLD,VI"}
   {:setting :MU, :min-academic-year -1, :max-academic-year 14, :needs "ASD,HI,M,MLD,MSI,OTH,PD,PMLD,SEMH,SLCN,SLD,SPLD,VI"}
   {:setting :OOE, :min-academic-year 6, :max-academic-year 20, :needs "ASD,HI,M,MLD,MSI,OTH,PD,PMLD,SEMH,SLCN,SLD,SPLD,VI"}
   {:setting :PRU, :min-academic-year 2, :max-academic-year 14, :needs "ASD,HI,M,MLD,MSI,OTH,PD,PMLD,SEMH,SLCN,SLD,SPLD,VI"}])

(def valid-states
  (-> valid-setting-academic-years
      (s/calculate-valid-states-from-setting-academic-years)))

(def valid-year-settings
  (s/calculate-valid-year-settings-from-setting-academic-years valid-setting-academic-years))

(def academic-years
  (s/calculate-academic-year-range valid-setting-academic-years))

(deftest validate-params
  (testing "Positive joiner age alpha for every valid academic year"
    (let [transitions {}
          params (sut/alpha-params-joiner-ages transitions)]
      (is (every? (fn [academic-year]
                    (pos? (get params academic-year))) c/academic-years))))

  (testing "Positive joiner state alphas for every valid academic year"
    (let [transitions {}
          params (sut/alpha-params-joiner-states valid-states transitions 0.5 0.5 0.5)]
      (is (every? (fn [academic-year]
                    (let [alphas (get params academic-year)]
                      (and (pos? (count alphas))
                           (->> alphas vals (every? pos?)))))
                  academic-years))))

  (testing "Positive mover state alphas for every year with >1 setting"
    (let [transitions {}
          params (sut/alpha-params-movers valid-states valid-year-settings transitions 0.5 0.5 0.5)]
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
          params (sut/beta-params-leavers valid-states transitions 0.5 0.5 0.5)]
      (is (every? (fn [[academic-year state]]
                    (let [betas (get params [academic-year state])]
                      (and (pos? (:alpha betas))
                           (pos? (:beta betas)))))
                  valid-states))))

  (testing "Positive mover beta params for every valid state"
    (let [transitions {}
          params (sut/beta-params-movers valid-states transitions 0.5 0.5 0.5)]
      (is (every? (fn [[academic-year state]]
                    (let [betas (get params [academic-year state])]
                      (and (pos? (:alpha betas))
                           (pos? (:beta betas)))))
                  valid-states))))

  (testing "Positive joiner beta params for every valid state"
    (let [transitions {}
          initial-population {}
          params (sut/beta-params-joiners transitions initial-population 0.5)]
      (is (and (pos? (:alpha params))
               (pos? (:beta params)))))))
