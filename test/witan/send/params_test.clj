(ns witan.send.params-test
  (:require [clojure.test :refer :all]
            [witan.send.params :as sut]
            [witan.send.constants :as c]
            [witan.send.states :as s]))

(deftest validate-params
  (testing "Positive joiner age alpha for every valid academic year"
    (let [transitions {}
          params (sut/alpha-params-joiner-ages transitions)]
      (is (every? (fn [academic-year]
                    (pos? (get params academic-year))) c/academic-years))))

  (testing "Positive joiner state alphas for every valid academic year"
    (let [transitions {}
          params (sut/alpha-params-joiner-states transitions 0.5 0.5 0.5)]
      (is (every? (fn [academic-year]
                    (let [alphas (get params academic-year)]
                      (and (pos? (count alphas))
                           (->> alphas vals (every? pos?)))))
                  c/academic-years))))

  (testing "Positive mover state alphas for every year with >1 setting"
    (let [transitions {}
          params (sut/alpha-params-movers transitions 0.5 0.5 0.5)]
      (is (empty? (remove (fn [[academic-year state]]
                            (let [alphas (get params [academic-year state])]
                              (and (pos? (count alphas))
                                   (->> alphas vals (every? pos?)))))
                          (for [academic-year c/academic-years
                                state (s/valid-states-for-ay academic-year)
                                :when (s/can-move? academic-year state)]
                            [academic-year state]))))))

  (testing "Positive leaver beta params for every valid state"
    (let [transitions {}
          params (sut/beta-params-leavers transitions 0.5 0.5 0.5)]
      (is (every? (fn [[academic-year state]]
                    (let [betas (get params [academic-year state])]
                      (and (pos? (:alpha betas))
                           (pos? (:beta betas)))))
                  s/valid-states))))

  (testing "Positive mover beta params for every valid state"
    (let [transitions {}
          params (sut/beta-params-movers transitions 0.5 0.5 0.5)]
      (is (every? (fn [[academic-year state]]
                    (let [betas (get params [academic-year state])]
                      (and (pos? (:alpha betas))
                           (pos? (:beta betas)))))
                  s/valid-states))))

  (testing "Positive joiner beta params for every valid state"
    (let [transitions {}
          initial-population {}
          params (sut/beta-params-joiners transitions initial-population 0.5)]
      (is (and (pos? (:alpha params))
               (pos? (:beta params)))))))
