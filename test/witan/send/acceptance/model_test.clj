(ns witan.send.acceptance.model-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [witan.send.main :as m]
   [witan.send.acceptance.utils :as au]))

(def output-files-to-check ["Output_AY.csv"
                            "Output_AY_Group.csv"
                            "Output_Cost.csv"
                            "Output_Count.csv"
                            "Output_Need.csv"
                            "Output_Setting.csv"
                            "Output_State.csv"
                            "transitions.edn"])

(def beta-files-to-check ["joiner_beta_expectations.csv"
                          "leaver_beta_expectations.csv"
                          "mover_beta_expectations.csv"])

(deftest expected-results
  (let [config (m/read-config "data/demo/config.edn")
        config-for-checking (m/read-config "data/demo/config_for_checking.edn")]

    (au/run-model-for-test config)

    (testing "Output checksums are unchanged"
      (is (= (au/get-md5s config output-files-to-check)
             (au/get-md5s config-for-checking output-files-to-check))))

    (testing "Beta checksums are unchanged"
      (is (= (au/get-md5s config beta-files-to-check)
             (au/get-md5s config-for-checking beta-files-to-check))))))

(defn find-in-map [seq-of-maps col v]
  (get (first (filter #(= (get % col) v) seq-of-maps)) "expectation"))

(deftest expected-filter-by-ay11-cy2015-results
  (let [config (m/read-config "data/demo/config.edn")
        config-splicing (m/read-config "data/demo/config_splicing.edn")
        config-splicing-for-checking (m/read-config "data/demo/config_for_checking_splicing.edn")]

    (run! au/run-model-for-test [config config-splicing])

    (let [load-results-triple (fn [f] [(au/load-results config f),
                                       (au/load-results config-splicing f)
                                       (au/load-results config-splicing-for-checking f)])
          [standard-joiner-exp, scenario-joiner-exp, checking-joiner-exp]
          (load-results-triple "joiner_beta_expectations.csv")
          [standard-leaver-exp, scenario-leaver-exp, checking-leaver-exp]
          (load-results-triple "leaver_beta_expectations.csv")
          [standard-mover-exp, scenario-mover-exp, checking-mover-exp]
          (load-results-triple  "mover_beta_expectations.csv")]

      (testing "some of the rates we expect to change are effected"
        (is (not= (find-in-map standard-joiner-exp "ay" "11")
                  (find-in-map scenario-joiner-exp "ay" "11")))
        (is (= (find-in-map standard-joiner-exp "ay" "10")
               (find-in-map scenario-joiner-exp "ay" "10")))
        (is (not= (find-in-map standard-leaver-exp "ay" "11")
                  (find-in-map scenario-leaver-exp "ay" "11")))
        (is (= (find-in-map standard-leaver-exp "ay" "9")
               (find-in-map scenario-leaver-exp "ay" "9")))
        (is (not= (find-in-map standard-mover-exp "ay" "11")
                  (find-in-map scenario-mover-exp "ay" "11")))
        (is (= (find-in-map standard-mover-exp "ay" "9")
               (find-in-map scenario-mover-exp "ay" "9"))))

      (testing "rates are as expected"
        (is (= (find-in-map checking-joiner-exp "ay" "10")
               (find-in-map scenario-joiner-exp "ay" "10")))
        (is (= (find-in-map checking-leaver-exp "ay" "11")
               (find-in-map scenario-leaver-exp "ay" "11")))
        (is (= (find-in-map checking-mover-exp "ay" "11")
               (find-in-map scenario-mover-exp "ay" "11")))))))
