(ns witan.send.model.run-test
  (:require [clojure.test :refer [deftest is testing]]
            [witan.send.main :as wsm]
            [witan.send.send :as wss]
            [witan.send.states :as states]
            [witan.send.distributions :as d]
            [witan.send.model.prepare :as wsmp]
            [witan.send.model.run :as sut]))


(deftest apply-leavers-movers-for-cohort-unsafe-test
  (testing "Transition counts are stable on re-run"
    (is (= [[{[11 :Y-F] 84, [11 :Y-N] 1} {[2018 11 :Y-F :Y-F] 84, [2018 11 :Y-F :Y-N] 1, [2018 11 :Y-F :NONSEND] 15}]
            [{[11 :Y-A] 1, [11 :Y-F] 85} {[2018 11 :Y-F :Y-A] 1, [2018 11 :Y-F :Y-F] 85, [2018 11 :Y-F :NONSEND] 14}]
            [{[11 :Y-F] 25} {[2018 11 :Y-F :Y-F] 25, [2018 11 :Y-F :NONSEND] 75}]
            [{[11 :Y-F] 46, [11 :Y-N] 1} {[2018 11 :Y-F :Y-F] 46, [2018 11 :Y-F :Y-N] 1, [2018 11 :Y-F :NONSEND] 53}]
            [{[11 :Y-F] 36} {[2018 11 :Y-F :Y-F] 36, [2018 11 :Y-F :NONSEND] 64}]
            [{[11 :Y-K] 3, [11 :Y-E] 1, [11 :Y-F] 90, [11 :Y-N] 6} {[2018 11 :Y-F :Y-K] 3, [2018 11 :Y-F :Y-E] 1, [2018 11 :Y-F :Y-F] 90, [2018 11 :Y-F :Y-N] 6, [2018 11 :Y-F :NONSEND] 0}]
            [{[11 :Y-K] 3, [11 :Y-F] 66, [11 :Y-N] 4} {[2018 11 :Y-F :Y-K] 3, [2018 11 :Y-F :Y-F] 66, [2018 11 :Y-F :Y-N] 4, [2018 11 :Y-F :NONSEND] 27}]
            [{[11 :Y-K] 1, [11 :Y-F] 47, [11 :Y-N] 2} {[2018 11 :Y-F :Y-K] 1, [2018 11 :Y-F :Y-F] 47, [2018 11 :Y-F :Y-N] 2, [2018 11 :Y-F :NONSEND] 50}]
            [{[11 :Y-K] 4, [11 :Y-F] 40, [11 :Y-N] 2} {[2018 11 :Y-F :Y-K] 4, [2018 11 :Y-F :Y-F] 40, [2018 11 :Y-F :Y-N] 2, [2018 11 :Y-F :NONSEND] 54}]
            [{[11 :Y-F] 14} {[2018 11 :Y-F :Y-F] 14, [2018 11 :Y-F :NONSEND] 86}]]
           (let [_ (d/set-seed! 42)
                 mover-state-alphas {[10 :Y-F]
                                     {:Y-H 0.0017035775127768312
                                      :Y-L 0.04940374787052811
                                      :Y-D 0.0017035775127768312
                                      :Y-K 2.02555366269165247 ;; 0.02555366269165247
                                      :Y-A 0.02555366269165247
                                      :Y-I 0.0017035775127768312
                                      :Y-B 0.0017035775127768312
                                      :Y-J 0.2402044293015332
                                      :Y-C 0.07325383304940375
                                      :Y-E 0.12095400340715501
                                      :Y-R 0.0017035775127768312
                                      :Y-G 0.07325383304940375
                                      :Y-N 2.3594548551959114 ;; 0.3594548551959114
                                      :Y-S 0.023850085178875637}}
                 leaver-beta-params {[10 :Y-F] { :beta 1.0 ;; 11.958035714285714,
                                                :alpha 1.0 ;; 5.04196428571428571 ;; :alpha 0.04196428571428571
                                                }}
                 mover-beta-params {[10 :Y-F] { :beta 10.0 ;; 11.920856610800746,
                                               :alpha 1.0 ;; 40.07914338919925512 ;; :alpha 0.07914338919925512
                                               }}
                 valid-year-settings {9 #{:L :M :I :R :A :F :D :B :J :C :E :G :H :N :K}
                                      10 #{:L :M :I :R :A :F :D :B :J :C :E :G :H :N :K}
                                      11 #{:L :M :I :R :A :F :D :B :J :C :E :G :H :N :K}
                                      12 #{:L :M :I :R :A :F :D :B :J :C :E :G :H :N :K}}
                 valid-transitions {:Y [:A :B :C :D :E :F :G :H :I :J :K :L :N :O :P :Q :R :S]
                                    :F [:A :B :C :D :E :F :G :H :I :J :K :L :N :O :P :Q :R :S]}
                 model {}
                 transitions {}
                 year 11
                 population 100
                 make-setting-invalid nil
                 calendar-year 2018
                 need-setting :Y-F]
             (repeatedly 10 #(sut/apply-leavers-movers-for-cohort-unsafe
                              [model transitions]
                              [[year need-setting] population]
                              {:mover-state-alphas mover-state-alphas
                               :mover-beta-params mover-beta-params
                               :leaver-beta-params leaver-beta-params
                               :valid-year-settings valid-year-settings}
                              calendar-year
                              valid-transitions
                              make-setting-invalid)))))))
