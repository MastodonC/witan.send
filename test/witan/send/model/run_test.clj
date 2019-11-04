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

(deftest apply-joiners-for-academic-year-test
  (testing "Transitions counts are stable on re-run"
    (is (= [[{[3 :T-B] 10, [3 :X-B] 2, [3 :X-J] 1} {[2018 3 :T-B :T-B] 1, [2018 3 :NONSEND :T-B] 9, [2018 3 :NONSEND :X-B] 1}]
            [{[3 :T-B] 18, [3 :X-B] 4, [3 :X-J] 1} {[2018 3 :T-B :T-B] 1, [2018 3 :NONSEND :T-B] 17, [2018 3 :NONSEND :X-B] 3}]
            [{[3 :T-B] 8, [3 :X-B] 3, [3 :X-J] 1} {[2018 3 :T-B :T-B] 1, [2018 3 :NONSEND :T-B] 7, [2018 3 :NONSEND :X-B] 2}]
            [{[3 :T-B] 9, [3 :X-B] 1, [3 :X-J] 1} {[2018 3 :T-B :T-B] 1, [2018 3 :NONSEND :T-B] 8}]
            [{[3 :T-B] 17, [3 :X-B] 8, [3 :X-J] 1} {[2018 3 :T-B :T-B] 1, [2018 3 :NONSEND :T-B] 16, [2018 3 :NONSEND :X-B] 7}]
            [{[3 :T-B] 6, [3 :X-B] 6, [3 :X-J] 1} {[2018 3 :T-B :T-B] 1, [2018 3 :NONSEND :T-B] 5, [2018 3 :NONSEND :X-B] 5}]
            [{[3 :T-B] 13, [3 :X-B] 4, [3 :X-J] 1} {[2018 3 :T-B :T-B] 1, [2018 3 :NONSEND :T-B] 12, [2018 3 :NONSEND :X-B] 3}]
            [{[3 :T-B] 14, [3 :X-B] 5, [3 :X-J] 1} {[2018 3 :T-B :T-B] 1, [2018 3 :NONSEND :T-B] 13, [2018 3 :NONSEND :X-B] 4}]
            [{[3 :T-B] 11, [3 :X-B] 4, [3 :X-J] 1} {[2018 3 :T-B :T-B] 1, [2018 3 :NONSEND :T-B] 10, [2018 3 :NONSEND :X-B] 3}]
            [{[3 :T-B] 3, [3 :X-B] 6, [3 :X-J] 1} {[2018 3 :T-B :T-B] 1, [2018 3 :NONSEND :T-B] 2, [2018 3 :NONSEND :X-B] 5}]]
           (let [_ (d/set-seed! 42)
                 joiner-beta-params {3 { :alpha 57/4, :beta 7027N }}
                 joiner-state-alphas {3 {:T-B 25.01 :X-B 11.01 :X-J 0.01388888}}
                 academic-year 3
                 population {3 8102}
                 calendar-year 2018
                 model {[3 :T-B] 1
                        [3 :X-B] 1
                        [3 :X-J] 1}
                 transitions {[2018 3 :T-B :T-B] 1}]
             (repeatedly 10 #(sut/apply-joiners-for-academic-year
                              [model transitions]
                              academic-year
                              population
                              {:joiner-beta-params joiner-beta-params
                               :joiner-state-alphas joiner-state-alphas}
                              calendar-year)))))))

(comment

  (let [modify-transition-from nil
        make-setting-invalid nil
        standard-projection
        {:mover-state-alphas {[10 :Y-F]
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
                               :Y-S 0.023850085178875637}
                              [9 :Y-F]
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
         :transitions {}
         :leaver-beta-params {[10 :Y-F] { :beta 1.0 ;; 11.958035714285714,
                                         :alpha 1.0 ;; 5.04196428571428571 ;; :alpha 0.04196428571428571
                                         }}
         :joiner-state-alphas {10 {:T-B 25.01 :X-B 11.01 :X-J 0.01388888}}
         :projected-population {2018 {0 8949
                                      -4 9142
                                      7 6871
                                      20 16131
                                      1 8882
                                      24 19217
                                      -2 8524
                                      4 7541
                                      -1 8407
                                      15 9489
                                      21 17732
                                      13 6233
                                      22 18998
                                      -3 8773
                                      6 7084
                                      25 19502
                                      17 11040
                                      3 8102
                                      12 6023
                                      2 8331
                                      23 19037
                                      19 14421
                                      11 6143
                                      9 6275
                                      5 7301
                                      14 8232
                                      16 10366
                                      10 6362
                                      18 12557
                                      -5 9639
                                      8 6296}}
         :valid-transitions {:Y [:A :B :C :D :E :F :G :H :I :J :K :L :N :O :P :Q :R :S]
                             :F [:A :B :C :D :E :F :G :H :I :J :K :L :N :O :P :Q :R :S]}
         :population {10 8102}
         :mover-beta-parameters {[10 :Y-F] { :beta 10.0 ;; 11.920856610800746,
                                            :alpha 1.0 ;; 40.07914338919925512 ;; :alpha 0.07914338919925512
                                            }}
         :valid-states {} ;; is this OK?
         :joiner-beta-params {10 { :alpha 57/4, :beta 7027N }}
         :valid-year-settings {9 #{:L :M :I :R :A :F :D :B :J :C :E :G :H :N :K}
                               10 #{:L :M :I :R :A :F :D :B :J :C :E :G :H :N :K}
                               11 #{:L :M :I :R :A :F :D :B :J :C :E :G :H :N :K}
                               12 #{:L :M :I :R :A :F :D :B :J :C :E :G :H :N :K}}
         }
        scenario-projection nil
        population-by-state {[9 :T-B] 10
                             [9 :X-B] 10
                             [9 :X-J] 10}
        calendar-year 2018
        projected-population {2018 {0 8949
                                    -4 9142
                                    7 6871
                                    20 16131
                                    1 8882
                                    24 19217
                                    -2 8524
                                    4 7541
                                    -1 8407
                                    15 9489
                                    21 17732
                                    13 6233
                                    22 18998
                                    -3 8773
                                    6 7084
                                    25 19502
                                    17 11040
                                    3 8102
                                    12 6023
                                    2 8331
                                    23 19037
                                    19 14421
                                    11 6143
                                    9 6275
                                    5 7301
                                    14 8232
                                    16 10366
                                    10 6362
                                    18 12557
                                    -5 9639
                                    8 6296}}
        ]

    (sut/run-model-iteration
     modify-transition-from
     make-setting-invalid
     standard-projection
     scenario-projection
     {:model population-by-state}
     [calendar-year projected-population]))


  )
