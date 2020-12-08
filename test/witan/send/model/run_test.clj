(ns witan.send.model.run-test
  (:require [clojure.test :refer [deftest is testing]]
            [witan.send.main :as m]
            [witan.send.model.prepare :as p]
            [witan.send.model.run :as sut]
            [witan.send.send :as s]))

(deftest test-run-send-model
  (let [config (m/read-config "data/demo/config-single.edn")
        input-datasets (s/build-input-datasets (:project-dir config) (:file-inputs config) (:schema-inputs config))
        inputs (p/prepare-send-inputs input-datasets (:transition-parameters config) true)
        {:keys [send-output simulated-transitions simulations projection]}
        (sut/run-send-model inputs (:projection-parameters config))
        send-output' @send-output
        simulated-transitions' @simulated-transitions
        projection' @projection]
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
        (is (= [{ [ 11 :Y-F ] 1 }
                { [ 2018 11 :Y-F :Y-F ] 1, [ 2018 11 :Y-F :NONSEND ] 0 }]
               (sut/apply-leavers-movers-for-cohort population-by-state cohort params calendar-year valid-transitions make-setting-invalid)))))

    (testing "Simulated transitions sample"
      (is (= #_{[2020 10 :Y-F :Y-F] 7,
                [2020 12 :T-C :T-C] 3,
                [2020 1 :V-B :V-A] 2,
                [2020 11 :X-E :X-E] 1,
                [2020 9 :U-L :NONSEND] 0,
                [2020 5 :V-B :V-B] 2,
                [2020 13 :NONSEND :U-D] 1}
             {[2019 9 :NONSEND :U-L] 2,
              [2019 4 :X-A :X-A] 4,
              [2019 16 :T-L :NONSEND] 0,
              [2019 6 :U-J :NONSEND] 1,
              [2019 5 :T-F :T-F] 8,
              [2019 5 :U-L :NONSEND] 0,
              [2019 9 :NONSEND :U-B] 1,
              [2019 7 :NONSEND :U-B] 1}
             (-> simulated-transitions'
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
                               [ 2019 7 :NONSEND :U-B ]]
                              #_[[2020 10 :Y-F :Y-F]
                                 [2020 12 :T-C :T-C]
                                 [2020 1 :V-B :V-A]
                                 [2020 11 :X-E :X-E]
                                 [2020 9 :U-L :NONSEND]
                                 [2020 5 :V-B :V-B]
                                 [2020 13 :NONSEND :U-D]]))

             )))
    (testing "Testing projection sample"
      (is (= {[ 2019 10 :U-B :U-N ] 1
              [ 2018 14 :V-J :NONSEND ] 0
              [ 2022 8 :X-B :NONSEND ] 1
              [ 2019 9 :T-A :T-A ] 15
              [ 2021 2 :NONSEND :X-B ] 2
              [ 2022 5 :X-E :X-E ] 1
              [ 2022 8 :NONSEND :T-A ] 1
              [ 2019 10 :X-B :NONSEND ] 0}
             (-> projection'
                 (select-keys [[ 2019 10 :U-B :U-N ]
                               [ 2018 14 :V-J :NONSEND ]
                               [ 2022 8 :X-B :NONSEND ]
                               [ 2019 9 :T-A :T-A ]
                               [ 2021 2 :NONSEND :X-B ]
                               [ 2022 5 :X-E :X-E ]
                               [ 2022 8 :NONSEND :T-A ]
                               [ 2019 10 :X-B :NONSEND ]]))))
      #_(is (= {[2020 10 :U-I :NONSEND] 0,
                [2022 12 :T-G :NONSEND] 1,
                [2019 15 :T-L :T-L] 2,
                [2020 5 :T-B :NONSEND] 1,
                [2018 12 :NONSEND :U-B] 1,
                [2022 1 :T-F :NONSEND] 0,
                [2022 14 :U-A :U-A] 9,
                [2020 4 :X-B :X-B] 4}
               (-> projection'
                   (select-keys [[2020 10 :U-I :NONSEND]
                                 [2022 12 :T-G :NONSEND]
                                 [2019 15 :T-L :T-L]
                                 [2020 5 :T-B :NONSEND]
                                 [2018 12 :NONSEND :U-B]
                                 [2022 1 :T-F :NONSEND]
                                 [2022 14 :U-A :U-A]
                                 [2020 4 :X-B :X-B]])))))

    (testing "Testing send output samples"
      (is (= {:min 187, :q1 187, :q3 187, :low-ci 187.0, :mean 187.0, :high-ci 187.0, :iqr 0, :high-95pc-bound 187, :low-95pc-bound 187, :median 187, :max 187, :std-dev 0.0}
             (-> send-output' (nth 3) :total-in-send-by-ay (get 7))
             #_{:min 181, :q1 181, :q3 181, :low-ci 181.0, :mean 181.0, :high-ci 181.0, :iqr 0, :high-95pc-bound 181, :low-95pc-bound 181, :median 181, :max 181, :std-dev 0.0})))
    (testing "Total cost sample"
      (is (= {:min 2414591, :q1 2416638, :q3 2416638, :low-ci 2415615.0, :mean 2415615.0, :high-ci 2415615.0, :iqr 0, :high-95pc-bound 2416638, :low-95pc-bound 2416638, :median 2416638, :max 2416638, :std-dev 0.0}
             (-> send-output' (nth 3) :total-cost)
             #_{:min 2387967, :q1 2390014, :q3 2390014, :low-ci 2388991.0, :mean 2388991.0, :high-ci 2388991.0, :iqr 0, :high-95pc-bound 2390014, :low-95pc-bound 2390014, :median 2390014, :max 2390014, :std-dev 0.0})))
    (testing "Total in send by AY Group Sample"
      (is (= {:min 930, :q1 930, :q3 930, :low-ci 930.0, :mean 930.0, :high-ci 930.0, :iqr 0, :high-95pc-bound 930, :low-95pc-bound 930, :median 930, :max 930, :std-dev 0.0}
             (-> send-output' (nth 4) :total-in-send-by-ay-group (get "NCY 7-11"))
             #_{:min 924, :q1 924, :q3 924, :low-ci 924.0, :mean 924.0, :high-ci 924.0, :iqr 0, :high-95pc-bound 924, :low-95pc-bound 924, :median 924, :max 924, :std-dev 0.0}
             )))
    (testing "Total in send sample"
      (is (= {:min 2153, :q1 2154, :q3 2154, :low-ci 2154.0, :mean 2154.0, :high-ci 2154.0, :iqr 0, :high-95pc-bound 2154, :low-95pc-bound 2154, :median 2154, :max 2154, :std-dev 0.0}
             (-> send-output' (nth 0) :total-in-send)
             #_{:min 2153, :q1 2154, :q3 2154, :low-ci 2154.0, :mean 2154.0, :high-ci 2154.0, :iqr 0, :high-95pc-bound 2154, :low-95pc-bound 2154, :median 2154, :max 2154, :std-dev 0.0})))
    (testing "Total by need sample"
      (is (= {:T {:min 1478, :q1 1478, :q3 1478, :low-ci 1478.0, :mean 1478.0, :high-ci 1478.0, :iqr 0, :high-95pc-bound 1478, :low-95pc-bound 1478, :median 1478, :max 1478, :std-dev 0.0},
              :U {:min 555, :q1 555, :q3 555, :low-ci 555.0, :mean 555.0, :high-ci 555.0, :iqr 0, :high-95pc-bound 555, :low-95pc-bound 555, :median 555, :max 555, :std-dev 0.0},
              :X {:min 227, :q1 227, :q3 227, :low-ci 227.0, :mean 227.0, :high-ci 227.0, :iqr 0, :high-95pc-bound 227, :low-95pc-bound 227, :median 227, :max 227, :std-dev 0.0}}
             (-> send-output' (nth 5) :total-in-send-by-need (select-keys [:T :U :X]))
             #_{:T {:min 1427, :q1 1427, :q3 1427, :low-ci 1427.0, :mean 1427.0, :high-ci 1427.0, :iqr 0, :high-95pc-bound 1427, :low-95pc-bound 1427, :median 1427, :max 1427, :std-dev 0.0},
                :U {:min 543, :q1 543, :q3 543, :low-ci 543.0, :mean 543.0, :high-ci 543.0, :iqr 0, :high-95pc-bound 543, :low-95pc-bound 543, :median 543, :max 543, :std-dev 0.0},
                :X {:min 225, :q1 225, :q3 225, :low-ci 225.0, :mean 225.0, :high-ci 225.0, :iqr 0, :high-95pc-bound 225, :low-95pc-bound 225, :median 225, :max 225, :std-dev 0.0}})))
    (testing "Total by setting sample"
      (is (= {:L {:min 56, :q1 56, :q3 56, :low-ci 56.0, :mean 56.0, :high-ci 56.0, :iqr 0, :high-95pc-bound 56, :low-95pc-bound 56, :median 56, :max 56, :std-dev 0.0},
              :M {:min 4, :q1 4, :q3 4, :low-ci 4.0, :mean 4.0, :high-ci 4.0, :iqr 0, :high-95pc-bound 4, :low-95pc-bound 4, :median 4, :max 4, :std-dev 0.0},
              :N {:min 36, :q1 36, :q3 36, :low-ci 36.0, :mean 36.0, :high-ci 36.0, :iqr 0, :high-95pc-bound 36, :low-95pc-bound 36, :median 36, :max 36, :std-dev 0.0}}
             (-> send-output' (nth 2) :total-in-send-by-setting (select-keys [:L :M :N]))
             #_{:L {:min 50, :q1 50, :q3 50, :low-ci 50.0, :mean 50.0, :high-ci 50.0, :iqr 0, :high-95pc-bound 50, :low-95pc-bound 50, :median 50, :max 50, :std-dev 0.0},
                :M {:min 2, :q1 2, :q3 2, :low-ci 2.0, :mean 2.0, :high-ci 2.0, :iqr 0, :high-95pc-bound 2, :low-95pc-bound 2, :median 2, :max 2, :std-dev 0.0},
                :N {:min 32, :q1 32, :q3 32, :low-ci 32.0, :mean 32.0, :high-ci 32.0, :iqr 0, :high-95pc-bound 32, :low-95pc-bound 32, :median 32, :max 32, :std-dev 0.0}})))
    (testing "Setting cost sample"
      (is (= {:L {:min 53983, :q1 54014, :q3 54014, :low-ci 53999.0, :mean 53999.0, :high-ci 53999.0, :iqr 0, :high-95pc-bound 54014, :low-95pc-bound 54014, :median 54014, :max 54014, :std-dev 0.0},
              :M {:min 1000, :q1 1000, :q3 1000, :low-ci 1000.0, :mean 1000.0, :high-ci 1000.0, :iqr 0, :high-95pc-bound 1000, :low-95pc-bound 1000, :median 1000, :max 1000, :std-dev 0.0},
              :N {:min 31999, :q1 32014, :q3 32014, :low-ci 32007.0, :mean 32007.0, :high-ci 32007.0, :iqr 0, :high-95pc-bound 32014, :low-95pc-bound 32014, :median 32014, :max 32014, :std-dev 0.0}}
             (-> send-output' (nth 1) :setting-cost (select-keys [:L :M :N]))
             #_{:L {:min 53983, :q1 54014, :q3 54014, :low-ci 53999.0, :mean 53999.0, :high-ci 53999.0, :iqr 0, :high-95pc-bound 54014, :low-95pc-bound 54014, :median 54014, :max 54014, :std-dev 0.0},
                :M {:min 1000, :q1 1000, :q3 1000, :low-ci 1000.0, :mean 1000.0, :high-ci 1000.0, :iqr 0, :high-95pc-bound 1000, :low-95pc-bound 1000, :median 1000, :max 1000, :std-dev 0.0},
                :N {:min 31999, :q1 32014, :q3 32014, :low-ci 32007.0, :mean 32007.0, :high-ci 32007.0, :iqr 0, :high-95pc-bound 32014, :low-95pc-bound 32014, :median 32014, :max 32014, :std-dev 0.0}})))))

(comment

  (def test-data
    (let [config (m/read-config "data/demo/config-single.edn")
          input-datasets (s/build-input-datasets (:project-dir config) (:file-inputs config) (:schema-inputs config))
          inputs (p/prepare-send-inputs input-datasets (:transition-parameters config) true)
          {:keys [send-output simulated-transitions simulations projection]}
          (sut/run-send-model inputs (:projection-parameters config))]
      {:send-output @send-output
       :simulated-transitions @simulated-transitions
       :simulations simulations
       :projection @projection}))

  (def send-output' (:send-output test-data))
  (def simulated-transitions' (:simulated-transitions test-data))
  (def projection' (:projection test-data))

  (-> test-data
      :simulated-transitions
      first
      second
      (nth 2)
      (select-keys [[ 2020 10 :Y-F :Y-F ]     ;; = 7
                    [ 2020 12 :T-C :T-C ]     ;; = 3
                    [ 2020 1 :V-B :V-A ]      ;; = 2
                    [ 2020 11 :X-E :X-E ]     ;; = 1
                    [ 2020 9 :U-L :NONSEND ]  ;; = 0
                    [ 2020 5 :V-B :V-B ]      ;; = 2
                    [ 2020 13 :NONSEND :U-D ] ;; = 1
                    ]))



  )

(comment

  (let [population-by-state [ {} {} ]
        cohort [ [ 11 :Y-F ] 1 ]
        calendar-year 2018
        valid-transitions {:F [:A :B :C :D :E :F :G :H :I :J :K :L :M :N :O :P :Q :R :S]}
        make-setting-invalid nil
        result [{ [ 11 :Y-F ] 1 }
                { [ 2018 11 :Y-F :Y-F ] 1, [ 2018 11 :Y-F :NONSEND ] 0 }]
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
    (sut/apply-leavers-movers-for-cohort population-by-state cohort params calendar-year valid-transitions make-setting-invalid))



  )
