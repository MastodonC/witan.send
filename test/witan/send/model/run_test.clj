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
    (testing "Simulated transitions sample"
      (is (= {[2020 10 :Y-F :Y-F] 7,
              [2020 12 :T-C :T-C] 3,
              [2020 1 :V-B :V-A] 2,
              [2020 11 :X-E :X-E] 1,
              [2020 9 :U-L :NONSEND] 0,
              [2020 5 :V-B :V-B] 2,
              [2020 13 :NONSEND :U-D] 1}
             (-> simulated-transitions'
                 first
                 second
                 (nth 2)
                 (select-keys [[2020 10 :Y-F :Y-F]
                               [2020 12 :T-C :T-C]
                               [2020 1 :V-B :V-A]
                               [2020 11 :X-E :X-E]
                               [2020 9 :U-L :NONSEND]
                               [2020 5 :V-B :V-B]
                               [2020 13 :NONSEND :U-D]])))))
    (testing "Testing projection sample"
      (is (= {[2020 10 :U-I :NONSEND] 0,
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
      (is (= (-> send-output' (nth 3) :total-in-send-by-ay (get 7))
             {:min 181, :q1 181, :q3 181, :low-ci 181.0, :mean 181.0, :high-ci 181.0, :iqr 0, :high-95pc-bound 181, :low-95pc-bound 181, :median 181, :max 181, :std-dev 0.0})))
    (testing "Total cost sample"
      (is (= (-> send-output' (nth 3) :total-cost)
             {:min 2387967, :q1 2390014, :q3 2390014, :low-ci 2388991.0, :mean 2388991.0, :high-ci 2388991.0, :iqr 0, :high-95pc-bound 2390014, :low-95pc-bound 2390014, :median 2390014, :max 2390014, :std-dev 0.0})))
    (testing "Total in send by AY Group Sample"
      (is (= (-> send-output' (nth 4) :total-in-send-by-ay-group (get "NCY 7-11"))
             {:min 924, :q1 924, :q3 924, :low-ci 924.0, :mean 924.0, :high-ci 924.0, :iqr 0, :high-95pc-bound 924, :low-95pc-bound 924, :median 924, :max 924, :std-dev 0.0}
             )))
    (testing "Total in send sample"
      (is (= (-> send-output' (nth 0) :total-in-send)
             {:min 2153, :q1 2154, :q3 2154, :low-ci 2154.0, :mean 2154.0, :high-ci 2154.0, :iqr 0, :high-95pc-bound 2154, :low-95pc-bound 2154, :median 2154, :max 2154, :std-dev 0.0})))
    (testing "Total by need sample"
      (is (= (-> send-output' (nth 5) :total-in-send-by-need (select-keys [:T :U :X]))
             {:T {:min 1427, :q1 1427, :q3 1427, :low-ci 1427.0, :mean 1427.0, :high-ci 1427.0, :iqr 0, :high-95pc-bound 1427, :low-95pc-bound 1427, :median 1427, :max 1427, :std-dev 0.0},
              :U {:min 543, :q1 543, :q3 543, :low-ci 543.0, :mean 543.0, :high-ci 543.0, :iqr 0, :high-95pc-bound 543, :low-95pc-bound 543, :median 543, :max 543, :std-dev 0.0},
              :X {:min 225, :q1 225, :q3 225, :low-ci 225.0, :mean 225.0, :high-ci 225.0, :iqr 0, :high-95pc-bound 225, :low-95pc-bound 225, :median 225, :max 225, :std-dev 0.0}})))
    (testing "Total by setting sample"
      (is (= (-> send-output' (nth 2) :total-in-send-by-setting (select-keys [:L :M :N]))
             {:L {:min 50, :q1 50, :q3 50, :low-ci 50.0, :mean 50.0, :high-ci 50.0, :iqr 0, :high-95pc-bound 50, :low-95pc-bound 50, :median 50, :max 50, :std-dev 0.0},
              :M {:min 2, :q1 2, :q3 2, :low-ci 2.0, :mean 2.0, :high-ci 2.0, :iqr 0, :high-95pc-bound 2, :low-95pc-bound 2, :median 2, :max 2, :std-dev 0.0},
              :N {:min 32, :q1 32, :q3 32, :low-ci 32.0, :mean 32.0, :high-ci 32.0, :iqr 0, :high-95pc-bound 32, :low-95pc-bound 32, :median 32, :max 32, :std-dev 0.0}})))
    (testing "Setting cost sample"
      (is (= (-> send-output' (nth 1) :setting-cost (select-keys [:L :M :N]))
             {:L {:min 53983, :q1 54014, :q3 54014, :low-ci 53999.0, :mean 53999.0, :high-ci 53999.0, :iqr 0, :high-95pc-bound 54014, :low-95pc-bound 54014, :median 54014, :max 54014, :std-dev 0.0},
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
