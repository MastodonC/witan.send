(ns witan.send.acceptance.model-test
  (:require [clojure.java.io :as io]
            [clojure.string :refer [join]]
            [clojure.test :refer [deftest is testing]]
            digest
            [witan.send.main :as m]
            [witan.send.model.output :as so]
            [witan.send.send :as send]
            [witan.send.validate-model :as v]
            [witan.send.model.input :as i]
            [clojure.core.matrix.dataset :as ds]))

(deftest expected-results
  (let [expected-md5s {"Output_AY.csv"       "0dfb8d8bd19e64800a2e37d76d58cafc"
                       "Output_AY_Group.csv" "dac7ec595449d6cfcb991cb656b1d928"
                       "Output_Cost.csv"     "26452e8aafd84d5e3760dc74e754bd0a"
                       "Output_Count.csv"    "8ba1b029910882f7e55fb656304bb38e"
                       "Output_Need.csv"     "eb43d161874b286c87e6e4fd784fda75"
                       "Output_Setting.csv"  "5384fb984eb38c24847adb79ff700ae2"
                       "Output_State.csv"    "9462b190d8d9a0c6d62c19578260481e"
                       "transitions.edn"     "baad299e776f1562f6585ae496a1166c"}
        historic-years (distinct (map :calendar-year (v/load-csv-as-maps "data/demo/data/transitions.csv")))
        expected-plots (into ["Joiner_Probability.pdf" "Joiner_Transitions.pdf" "Leaver_Probability.pdf"
                              "Mover_Probability.pdf" "NCY_Population_Trends.pdf" "Need_Trends.pdf"
                              "Setting_Type_Counts.pdf" "Settings_Trends_1.pdf" "Settings_Trends_2.pdf"
                              "Settings_Trends_3.pdf" "Settings_Trends_4.pdf"
                              "Special_Setting_Counts.pdf" "Total_Cost.pdf" "Total_Population.pdf"]
                             (map #(str "Historic_Transitions_" % ".pdf") historic-years))
        files (keys expected-md5s)
        files-and-plots (into expected-plots files)
        config (m/read-config "data/demo/config.edn")
        output-dir (m/get-output-dir config)]
    (run! #(let [file (join "/" [output-dir %])]
             (when (.exists (io/file file))
               (io/delete-file file))) files-and-plots)
    (-> (send/run-send-workflow config)
        (so/output-send-results (:output-parameters config)))
    (testing "checksums are unchanged"
      (is (= expected-md5s
             (into {} (for [f files]
                        [f (-> (io/file output-dir f) (digest/md5))])))))
    ;;; Following test works locally, but requires R install on CircleCI
    #_(testing "all plots are produced"
        (is (every? true? (map #(.exists (io/file (join "/" [output-dir %]))) expected-plots))))))

(defn load-results [path]
  (let [csv (i/load-csv path)]
    (ds/row-maps (ds/dataset (:column-names csv) (:columns csv)))))

(defn find-in-map [seq-of-maps col v]
  (get (first (filter #(= (get % col) v) seq-of-maps)) "expectation"))

(deftest expected-filter-by-ay11-cy2015-results
  (let [expected-md5s {"Output_AY.csv"       "c8d70d87dc8119b987110ec8ff85650a"
                       "Output_AY_Group.csv" "c1242f6ccf390e6230ee00c87e99aa3a"
                       "Output_Cost.csv"     "646f383501b5767291cbbe5626c76573"
                       "Output_Count.csv"    "3f0fd5fbb4a7ab6311f91808ee46db62"
                       "Output_Need.csv"     "4ace9aa7a86653920373edd580e61b7c"
                       "Output_Setting.csv"  "9651093f9f0a4d55b980f170ff589bde"
                       "Output_State.csv"    "50f2332b826ed4d9557d230372c1beb0"
                       "transitions.edn"     "517fdebfc3cc690513f34c04e9bc9656"}
        files (keys expected-md5s)
        config (m/read-config "data/demo/config_splicing.edn")
        output-dir (m/get-output-dir config)]
    (run! #(let [file (join "/" [output-dir %])]
             (when (.exists (io/file file))
               (io/delete-file file))) files)
    (-> (send/run-send-workflow config)
        (so/output-send-results (:output-parameters config)))
    (testing "checksums are unchanged for scenario"
      (is (= expected-md5s
             (into {} (for [f files]
                        [f (-> (io/file output-dir f) (digest/md5))])))))
    (let [standard-joiner-exp (load-results "data/demo/results_for_checking/joiner_beta_expectations.csv")
          scenario-joiner-exp (load-results (str output-dir "/joiner_beta_expectations.csv"))
          standard-leaver-exp (load-results "data/demo/results_for_checking/leaver_beta_expectations.csv")
          scenario-leaver-exp (load-results (str output-dir "/leaver_beta_expectations.csv"))
          standard-mover-exp (load-results "data/demo/results_for_checking/mover_beta_expectations.csv")
          scenario-mover-exp (load-results (str output-dir "/mover_beta_expectations.csv"))]
      (testing "some of the rates we expect to change are effected"
        (is (not= (find-in-map standard-joiner-exp "ay" "11")
                  (find-in-map scenario-joiner-exp "ay" "11")))
        (is (= (find-in-map standard-joiner-exp "ay" "10")
               (find-in-map scenario-joiner-exp "ay" "10")))
        (is (not= (find-in-map standard-leaver-exp "ay" "11")
                  (find-in-map scenario-leaver-exp "ay" "11")))
        (is (= (find-in-map standard-leaver-exp "ay" "10")
               (find-in-map scenario-leaver-exp "ay" "10")))
        (is (not= (find-in-map standard-mover-exp "ay" "11")
                  (find-in-map scenario-mover-exp "ay" "11")))
        (is (= (find-in-map standard-mover-exp "ay" "10")
               (find-in-map scenario-mover-exp "ay" "10"))))
      (testing "rates are as expected"
        (is (= 8.9221983E-4 (read-string (find-in-map scenario-joiner-exp "ay" "11"))))
        (is (= 0.0255 (read-string (find-in-map scenario-leaver-exp "ay" "11"))))
        (is (= 0.021875 (read-string (find-in-map scenario-mover-exp "ay" "11"))))))))
