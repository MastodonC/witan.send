(ns witan.send.params-test
  (:require [clojure.core.matrix.dataset :as ds]
            [clojure.test :refer [deftest testing is]]
            [witan.send.constants :as c]
            [witan.send.model.input :as i]
            [witan.send.params :as sut]
            [witan.send.schemas :as sc]
            [witan.send.states :as s]))

(def valid-setting-academic-years
  [{:setting :A, :min-academic-year -3, :max-academic-year 15, :needs "T,U,V,W,X,Y" :setting->setting "A,B,C,D,E,F,G,H,I,J,K,L,N,O,P,Q,R,S"}
   {:setting :B, :min-academic-year -1, :max-academic-year 16, :needs "T,U,V,W,X,Y", :setting->setting "A,B,C,D,E,F,G,H,I,J,K,L,N,O,P,Q,R,S"}
   {:setting :C, :min-academic-year 6, :max-academic-year 15, :needs "T,U,V,W,X,Y", :setting->setting "A,B,C,D,E,F,G,H,I,J,K,L,N,O,P,Q,R,S"}
   {:setting :D, :min-academic-year 6, :max-academic-year 15, :needs "T,U,V,W,X,Y", :setting->setting "A,B,C,D,E,F,G,H,I,J,K,L,N,O,P,Q,R,S"}
   {:setting :E, :min-academic-year 2, :max-academic-year 15, :needs "T,U,V,W,X,Y", :setting->setting "A,B,C,D,E,F,G,H,I,J,K,L,N,O,P,Q,R,S"}
   {:setting :F, :min-academic-year -1, :max-academic-year 14, :needs "T,U,V,W,X,Y", :setting->setting "A,B,C,D,E,F,G,H,I,J,K,L,N,O,P,Q,R,S"}
   {:setting :G, :min-academic-year -2, :max-academic-year 15, :needs "T,U,V,W,X,Y", :setting->setting "A,B,C,D,E,F,G,H,I,J,K,L,N,O,P,Q,R,S"}
   {:setting :H, :min-academic-year 0, :max-academic-year 14, :needs "T,U,V,W,X,Y", :setting->setting "A,B,C,D,E,F,G,H,I,J,K,L,N,O,P,Q,R,S"}
   {:setting :I, :min-academic-year 0, :max-academic-year 15, :needs "T,U,V,W,X,Y", :setting->setting "A,B,C,D,E,F,G,H,I,J,K,L,N,O,P,Q,R,S"}
   {:setting :J, :min-academic-year -2, :max-academic-year 16, :needs "T,U,V,W,X,Y", :setting->setting "A,B,C,D,E,F,G,H,I,J,K,L,N,O,P,Q,R,S"}
   {:setting :K, :min-academic-year 0, :max-academic-year 14, :needs "T,U,V,W,X,Y", :setting->setting "A,B,C,D,E,F,G,H,I,J,K,L,N,O,P,Q,R,S"}
   {:setting :L, :min-academic-year 0, :max-academic-year 15, :needs "T,U,V,W,X,Y", :setting->setting "A,B,C,D,E,F,G,H,I,J,K,L,N,O,P,Q,R,S"}
   {:setting :M, :min-academic-year 6, :max-academic-year 15, :needs "T,U,V,W,X,Y", :setting->setting "A,B,C,D,E,F,G,H,I,J,K,L,N,O,P,Q,R,S"}
   {:setting :N, :min-academic-year 8, :max-academic-year 17, :needs "T,U,V,W,X,Y", :setting->setting "A,B,C,D,E,F,G,H,I,J,K,L,N,O,P,Q,R,S"}
   {:setting :O, :min-academic-year 0, :max-academic-year 6, :needs "T,U,V,W,X,Y", :setting->setting "A,B,C,D,E,F,G,H,I,J,K,L,N,O,P,Q,R,S"}
   {:setting :P, :min-academic-year -3, :max-academic-year -1, :needs "T,U,V,W,X,Y", :setting->setting "A,B,C,D,E,F,G,H,I,J,K,L,N,O,P,Q,R,S"}
   {:setting :Q, :min-academic-year -2, :max-academic-year 0, :needs "T,U,V,W,X,Y", :setting->setting "A,B,C,D,E,F,G,H,I,J,K,L,N,O,P,Q,R,S"}
   {:setting :R, :min-academic-year -1, :max-academic-year 14, :needs "T,U,V,W,X,Y", :setting->setting "A,B,C,D,E,F,G,H,I,J,K,L,N,O,P,Q,R,S"}
   {:setting :S, :min-academic-year 11, :max-academic-year 19, :needs "T,U,V,W,X,Y", :setting->setting "A,B,C,D,E,F,G,H,I,J,K,L,N,O,P,Q,R,S"}])


(def population-dataset
  (i/csv-to-dataset "data/demo/data/population.csv" sc/PopulationDataset))

(def transitions
  (i/csv-to-dataset "data/demo/data/transitions.csv" sc/TransitionCounts))

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
          params (sut/alpha-params-movers valid-states valid-year-settings valid-transitions transitions)]
      (is (empty? (remove (fn [[academic-year state]]
                            (let [alphas (get params [academic-year state])]
                              (and (pos? (count alphas))
                                   (->> alphas vals (every? pos?)))))
                          (for [academic-year c/academic-years
                                state (s/validate-states-for-ay valid-states academic-year)
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
          params (sut/beta-params-movers valid-states valid-year-settings transitions)]
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
        transitions (->> transitions
                         ds/row-maps)
        expected-academic-years (->> population-row-maps
                                     (map :academic-year)
                                     (into #{}))
        result (sut/beta-params-joiners valid-states
                                        transitions
                                        population-row-maps)]
    (testing "each val is a valid beta param"
      (is (every? (every-pred :alpha :beta) (vals result)))
      (is (every? (comp (partial every? pos?) vals) (vals result))))))
