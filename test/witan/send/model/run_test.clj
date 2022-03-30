(ns witan.send.model.run-test
  (:require [clojure.test :refer [deftest is testing]]
            [witan.send.distributions :as d]
            [witan.send.main :as m]
            [witan.send.model.prepare :as p]
            [witan.send.model.run :as sut]
            [witan.send.send :as s]))

(deftest test-run-send-model
  (let [config (m/read-config "data/demo/config-single.edn")
        input-datasets (s/build-input-datasets (:project-dir config) (:file-inputs config) (:schema-inputs config))
        inputs (p/prepare-send-inputs input-datasets (:transition-parameters config) true)
        {:keys [send-output simulated-transitions simulations projection]}
        (sut/run-send-model inputs (:projection-parameters config))]
    (testing "Simulation count"
      (is (= 1 simulations)))

    (testing "Apply leavers movers for cohort"
      (let [population-by-state [ {} {} ]
            cohort [ [ 11 :Y-F ] 1 ]
            calendar-year 2018
            valid-transitions {:F [:A :B :C :D :E :F :G :H :I :J :K :L :M :N :O :P :Q :R :S]}
            make-setting-invalid nil
            params {:mover-state-alphas {[10 :Y-F]
                                         {:Y-H 0.0017035775127768312
                                          :Y-L 0.04940374787052811
                                          :Y-D 0.0017035775127768312
                                          :Y-K 0.02555366269165247
                                          :Y-A 0.02555366269165247
                                          :Y-I 0.0017035775127768312
                                          :Y-B 0.0017035775127768312
                                          :Y-J 0.2402044293015332
                                          :Y-C 0.07325383304940375
                                          :Y-E 0.12095400340715501
                                          :Y-R 0.0017035775127768312
                                          :Y-G 0.07325383304940375
                                          :Y-N 0.3594548551959114
                                          :Y-S 0.023850085178875637}}
                    :leaver-beta-params {[10 :Y-F]
                                         {:beta 11.958035714285714
                                          :alpha 0.04196428571428571}}
                    :mover-beta-params {[10 :Y-F]
                                        {:beta 11.920856610800746
                                         :alpha 0.07914338919925512}}
                    :valid-year-settings {12 #{:L :M :I :R :A :F :D :B :J :C :E :G :H :S :N :K}}}]
        (is (= [{[11 :Y-F] 1} {[2018 11 :Y-F :Y-F] 1, [2018 11 :Y-F :NONSEND] 0}]
               #_[{ [ 11 :Y-F ] 1 }
                  { [ 2018 11 :Y-F :Y-F ] 1, [ 2018 11 :Y-F :NONSEND ] 0 }]
               (sut/apply-leavers-movers-for-cohort population-by-state cohort params calendar-year valid-transitions make-setting-invalid)))))

    (testing "Apply joiners for academic year with seed 42"
      (let [_ (d/set-seed! 42)
            academic-year 7
            joiner-beta-params {7 {:alpha 41/4 :beta 12195/2}}
            calendar-year 2018
            joiner-state-alphas
            {7 {:T-A 0.011904761904761904 :T-B 9.011904761904763 :T-C 0.011904761904761904 :T-D 0.011904761904761904 :T-E 0.011904761904761904 :T-F 0.011904761904761904 :T-G 0.011904761904761904 :T-H 0.011904761904761904 :T-I 1.0119047619047619 :T-J 0.011904761904761904 :T-K 0.011904761904761904 :T-L 2.011904761904762 :T-M 0.011904761904761904 :T-R 0.011904761904761904
                :U-A 0.011904761904761904 :U-B 7.011904761904762 :U-C 0.011904761904761904 :U-D 0.011904761904761904 :U-E 0.011904761904761904 :U-F 0.011904761904761904 :U-G 0.011904761904761904 :U-H 0.011904761904761904 :U-I 1.0119047619047619 :U-J 1.0119047619047619 :U-K 0.011904761904761904 :U-L 1.0119047619047619 :U-M 0.011904761904761904 :U-R 1.0119047619047619
                :V-A 0.011904761904761904 :V-B 0.011904761904761904 :V-C 0.011904761904761904 :V-D 0.011904761904761904 :V-E 0.011904761904761904 :V-F 0.011904761904761904 :V-G 0.011904761904761904 :V-H 0.011904761904761904 :V-I 0.011904761904761904 :V-J 2.011904761904762 :V-K 0.011904761904761904 :V-L 0.011904761904761904 :V-M 0.011904761904761904 :V-R 0.011904761904761904
                :W-A 0.011904761904761904 :W-B 0.011904761904761904 :W-C 0.011904761904761904 :W-D 0.011904761904761904 :W-E 0.011904761904761904 :W-F 0.011904761904761904 :W-G 0.011904761904761904 :W-H 0.011904761904761904 :W-I 0.011904761904761904 :W-J 0.011904761904761904 :W-K 0.011904761904761904 :W-L 0.011904761904761904 :W-M 0.011904761904761904 :W-R 1.0119047619047619
                :X-A 1.0119047619047619 :X-B 7.011904761904762 :X-C 0.011904761904761904 :X-D 0.011904761904761904 :X-E 0.011904761904761904 :X-F 0.011904761904761904 :X-G 0.011904761904761904 :X-H 0.011904761904761904 :X-I 0.011904761904761904 :X-J 0.011904761904761904 :X-K 0.011904761904761904 :X-L 0.011904761904761904 :X-M 0.011904761904761904 :X-R 1.0119047619047619
                :Y-A 0.011904761904761904 :Y-B 3.011904761904762 :Y-C 0.011904761904761904 :Y-D 0.011904761904761904 :Y-E 0.011904761904761904 :Y-F 2.011904761904762 :Y-G 0.011904761904761904 :Y-H 0.011904761904761904 :Y-I 0.011904761904761904 :Y-J 0.011904761904761904 :Y-K 0.011904761904761904 :Y-L 1.0119047619047619 :Y-M 0.011904761904761904 :Y-R 0.011904761904761904}}
            population {7 6871}
            model {}
            transitions {}]
        (is (= [{[7 :U-B] 2, [7 :X-A] 4} {[2018 7 :NONSEND :U-B] 2, [2018 7 :NONSEND :X-A] 4}]
               #_[{[7 :U-B] 2, [7 :X-A] 4} {[2018 7 :NONSEND :U-B] 2, [2018 7 :NONSEND :X-A] 4}]
               (sut/apply-joiners-for-academic-year [model transitions] academic-year population {:joiner-beta-params joiner-beta-params :joiner-state-alphas joiner-state-alphas} calendar-year)))))

    (testing "Apply joiners for academic year with seed 50"
      (let [_ (d/set-seed! 50)
            academic-year 7
            joiner-beta-params {7 {:alpha 41/4 :beta 12195/2}}
            calendar-year 2019
            joiner-state-alphas
            {7 {:T-A 0.011904761904761904 :T-B 9.011904761904763 :T-C 0.011904761904761904 :T-D 0.011904761904761904 :T-E 0.011904761904761904 :T-F 0.011904761904761904 :T-G 0.011904761904761904 :T-H 0.011904761904761904 :T-I 1.0119047619047619 :T-J 0.011904761904761904 :T-K 0.011904761904761904 :T-L 2.011904761904762 :T-M 0.011904761904761904 :T-R 0.011904761904761904
                :U-A 0.011904761904761904 :U-B 7.011904761904762 :U-C 0.011904761904761904 :U-D 0.011904761904761904 :U-E 0.011904761904761904 :U-F 0.011904761904761904 :U-G 0.011904761904761904 :U-H 0.011904761904761904 :U-I 1.0119047619047619 :U-J 1.0119047619047619 :U-K 0.011904761904761904 :U-L 1.0119047619047619 :U-M 0.011904761904761904 :U-R 1.0119047619047619
                :V-A 0.011904761904761904 :V-B 0.011904761904761904 :V-C 0.011904761904761904 :V-D 0.011904761904761904 :V-E 0.011904761904761904 :V-F 0.011904761904761904 :V-G 0.011904761904761904 :V-H 0.011904761904761904 :V-I 0.011904761904761904 :V-J 2.011904761904762 :V-K 0.011904761904761904 :V-L 0.011904761904761904 :V-M 0.011904761904761904 :V-R 0.011904761904761904
                :W-A 0.011904761904761904 :W-B 0.011904761904761904 :W-C 0.011904761904761904 :W-D 0.011904761904761904 :W-E 0.011904761904761904 :W-F 0.011904761904761904 :W-G 0.011904761904761904 :W-H 0.011904761904761904 :W-I 0.011904761904761904 :W-J 0.011904761904761904 :W-K 0.011904761904761904 :W-L 0.011904761904761904 :W-M 0.011904761904761904 :W-R 1.0119047619047619
                :X-A 1.0119047619047619 :X-B 7.011904761904762 :X-C 0.011904761904761904 :X-D 0.011904761904761904 :X-E 0.011904761904761904 :X-F 0.011904761904761904 :X-G 0.011904761904761904 :X-H 0.011904761904761904 :X-I 0.011904761904761904 :X-J 0.011904761904761904 :X-K 0.011904761904761904 :X-L 0.011904761904761904 :X-M 0.011904761904761904 :X-R 1.0119047619047619
                :Y-A 0.011904761904761904 :Y-B 3.011904761904762 :Y-C 0.011904761904761904 :Y-D 0.011904761904761904 :Y-E 0.011904761904761904 :Y-F 2.011904761904762 :Y-G 0.011904761904761904 :Y-H 0.011904761904761904 :Y-I 0.011904761904761904 :Y-J 0.011904761904761904 :Y-K 0.011904761904761904 :Y-L 1.0119047619047619 :Y-M 0.011904761904761904 :Y-R 0.011904761904761904}}
            population {7 6969}
            model {}
            transitions {}]
        (is (= [{[7 :U-B] 5, [7 :Y-B] 2, [7 :T-B] 5, [7 :V-J] 1, [7 :Y-F] 3, [7 :X-B] 1, [7 :U-R] 1} {[2019 7 :NONSEND :U-B] 5, [2019 7 :NONSEND :Y-B] 2, [2019 7 :NONSEND :T-B] 5, [2019 7 :NONSEND :V-J] 1, [2019 7 :NONSEND :Y-F] 3, [2019 7 :NONSEND :X-B] 1, [2019 7 :NONSEND :U-R] 1}]
               #_[{[7 :U-B] 5, [7 :Y-B] 2, [7 :T-B] 5, [7 :V-J] 1, [7 :Y-F] 3, [7 :X-B] 1, [7 :U-R] 1} {[2019 7 :NONSEND :U-B] 5, [2019 7 :NONSEND :Y-B] 2, [2019 7 :NONSEND :T-B] 5, [2019 7 :NONSEND :V-J] 1, [2019 7 :NONSEND :Y-F] 3, [2019 7 :NONSEND :X-B] 1, [2019 7 :NONSEND :U-R] 1}]
               (sut/apply-joiners-for-academic-year [model transitions] academic-year population {:joiner-beta-params joiner-beta-params :joiner-state-alphas joiner-state-alphas} calendar-year)))))

    (testing "Simulated transitions sample"
      (is (= {[2019 4 :X-A :X-A] 4,
              [2019 16 :T-L :NONSEND] 0,
              [2019 6 :U-J :NONSEND] 0,
              [2019 5 :T-F :T-F] 8,
              [2019 5 :U-L :NONSEND] 0,
              [2019 7 :NONSEND :U-B] 2}
             (-> simulated-transitions
                 first
                 second
                 (nth 1)
                 (select-keys [[ 2019 9 :NONSEND :U-L ]
                               [ 2019 4 :X-A :X-A ]
                               [ 2019 16 :T-L :NONSEND ]
                               [ 2019 6 :U-J :NONSEND ]
                               [ 2019 5 :T-F :T-F ]
                               [ 2019 5 :U-L :NONSEND ]
                               [ 2019 9 :NONSEND :U-B ]
                               [ 2019 7 :NONSEND :U-B ]])))))

    (testing "Testing projection sample"
      (is (= {[2019 10 :U-B :U-N] 2,
              [2018 14 :V-J :NONSEND] 0,
              [2022 8 :X-B :NONSEND] 0,
              [2019 9 :T-A :T-A] 17,
              [2019 10 :X-B :NONSEND] 0}
             ;; Old test
             #_{[2018 14 :V-J :NONSEND] 0,
                [2022 8 :X-B :NONSEND] 0,
                [2019 9 :T-A :T-A] 17,
                [2021 2 :NONSEND :X-B] 1,
                [2022 8 :NONSEND :T-A] 2,
                [2019 10 :X-B :NONSEND] 0}
             (-> projection
                 (select-keys [[ 2019 10 :U-B :U-N ]
                               [ 2018 14 :V-J :NONSEND ]
                               [ 2022 8 :X-B :NONSEND ]
                               [ 2019 9 :T-A :T-A ]
                               [ 2021 2 :NONSEND :X-B ]
                               [ 2022 5 :X-E :X-E ]
                               [ 2022 8 :NONSEND :T-A ]
                               [ 2019 10 :X-B :NONSEND ]])))))

    ;; We don't do this output any more
    #_(testing "Testing send output samples"
        (is (= {:min 199,
                :q1 199,
                :q3 199,
                :low-ci 199.0,
                :mean 199.0,
                :high-ci 199.0,
                :iqr 0,
                :high-95pc-bound 199,
                :low-95pc-bound 199,
                :median 199,
                :max 199,
                :std-dev 0.0}
               (-> send-output (nth 3) :total-in-send-by-ay (get 7)))))

    ;; We don't do this output any more
    #_(testing "Total cost sample"
        (is (= {:min 2418687,
                :q1 2420734,
                :q3 2420734,
                :low-ci 2419711.0,
                :mean 2419711.0,
                :high-ci 2419711.0,
                :iqr 0,
                :high-95pc-bound 2420734,
                :low-95pc-bound 2420734,
                :median 2420734,
                :max 2420734,
                :std-dev 0.0}
               (-> send-output (nth 3) :total-cost))))

    ;; We don't do this output any more
    #_(testing "Total in send by AY Group Sample"
        (is (= {:min 943,
                :q1 943,
                :q3 943,
                :low-ci 943.0,
                :mean 943.0,
                :high-ci 943.0,
                :iqr 0,
                :high-95pc-bound 943,
                :low-95pc-bound 943,
                :median 943,
                :max 943,
                :std-dev 0.0}
               (-> send-output (nth 4) :total-in-send-by-ay-group (get "NCY 7-11")))))
    ;; We don't do this output any more
    #_(testing "Total in send sample"
        (is (= {:min 2153,
                :q1 2154,
                :q3 2154,
                :low-ci 2154.0,
                :mean 2154.0,
                :high-ci 2154.0,
                :iqr 0,
                :high-95pc-bound 2154,
                :low-95pc-bound 2154,
                :median 2154,
                :max 2154,
                :std-dev 0.0}
               (-> send-output (nth 0) :total-in-send))))
    ;; We don't do this output any more
    #_(testing "Total by need sample"
        (is (= {:T
                {:min 1506,
                 :q1 1506,
                 :q3 1506,
                 :low-ci 1506.0,
                 :mean 1506.0,
                 :high-ci 1506.0,
                 :iqr 0,
                 :high-95pc-bound 1506,
                 :low-95pc-bound 1506,
                 :median 1506,
                 :max 1506,
                 :std-dev 0.0},
                :U
                {:min 545,
                 :q1 545,
                 :q3 545,
                 :low-ci 545.0,
                 :mean 545.0,
                 :high-ci 545.0,
                 :iqr 0,
                 :high-95pc-bound 545,
                 :low-95pc-bound 545,
                 :median 545,
                 :max 545,
                 :std-dev 0.0},
                :X
                {:min 206,
                 :q1 206,
                 :q3 206,
                 :low-ci 206.0,
                 :mean 206.0,
                 :high-ci 206.0,
                 :iqr 0,
                 :high-95pc-bound 206,
                 :low-95pc-bound 206,
                 :median 206,
                 :max 206,
                 :std-dev 0.0}}
               (-> send-output (nth 5) :total-in-send-by-need (select-keys [:T :U :X])))))

    ;; We don't do this output any more
    #_(testing "Total by setting sample"
        (is (= {:L
                {:min 58,
                 :q1 58,
                 :q3 58,
                 :low-ci 58.0,
                 :mean 58.0,
                 :high-ci 58.0,
                 :iqr 0,
                 :high-95pc-bound 58,
                 :low-95pc-bound 58,
                 :median 58,
                 :max 58,
                 :std-dev 0.0},
                :M
                {:min 3,
                 :q1 3,
                 :q3 3,
                 :low-ci 3.0,
                 :mean 3.0,
                 :high-ci 3.0,
                 :iqr 0,
                 :high-95pc-bound 3,
                 :low-95pc-bound 3,
                 :median 3,
                 :max 3,
                 :std-dev 0.0},
                :N
                {:min 30,
                 :q1 30,
                 :q3 30,
                 :low-ci 30.0,
                 :mean 30.0,
                 :high-ci 30.0,
                 :iqr 0,
                 :high-95pc-bound 30,
                 :low-95pc-bound 30,
                 :median 30,
                 :max 30,
                 :std-dev 0.0}}
               (-> send-output (nth 2) :total-in-send-by-setting (select-keys [:L :M :N])))))

    ;; We don't do this output any more
    #_(testing "Setting cost sample"
        (is (= {:L
                {:min 53983,
                 :q1 54014,
                 :q3 54014,
                 :low-ci 53999.0,
                 :mean 53999.0,
                 :high-ci 53999.0,
                 :iqr 0,
                 :high-95pc-bound 54014,
                 :low-95pc-bound 54014,
                 :median 54014,
                 :max 54014,
                 :std-dev 0.0},
                :M
                {:min 1000,
                 :q1 1000,
                 :q3 1000,
                 :low-ci 1000.0,
                 :mean 1000.0,
                 :high-ci 1000.0,
                 :iqr 0,
                 :high-95pc-bound 1000,
                 :low-95pc-bound 1000,
                 :median 1000,
                 :max 1000,
                 :std-dev 0.0},
                :N
                {:min 31999,
                 :q1 32014,
                 :q3 32014,
                 :low-ci 32007.0,
                 :mean 32007.0,
                 :high-ci 32007.0,
                 :iqr 0,
                 :high-95pc-bound 32014,
                 :low-95pc-bound 32014,
                 :median 32014,
                 :max 32014,
                 :std-dev 0.0}}
               (-> send-output (nth 1) :setting-cost (select-keys [:L :M :N])))))))
