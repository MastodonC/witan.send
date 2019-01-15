(ns witan.send.acceptance.model-test
  (:require [clojure.java.io :as io]
            [clojure.string :refer [join]]
            [clojure.test :refer [deftest is testing]]
            digest
            [witan.send.main :as m]
            [witan.send.model.output :as so]
            [witan.send.send :as send]
            [witan.send.validate-model :as v]))

(deftest expected-results
  (let [expected-md5s {"Output_AY.csv" "848944192402c899e9891453e91287b4",
                       "Output_AY_Group.csv" "d742c2727c455e3d8b65f42c58d00048",
                       "Output_State.csv" "4aba396af0f6aaee0734723ebbd0f96c",
                       "Output_Cost.csv" "7d6bffb95741ccb333d9b5dc90e24c4d",
                       "Output_Count.csv" "e9f93ac2c13a41f5536e2bfb97daadd5",
                       "Output_Need.csv" "460b794ff7bfdecc7c7c04657c2db585",
                       "Output_Setting.csv" "2e89259376e530ed84439da0eb32027a",
                       "transitions.edn" "27ca57ba18f6024d335fd57722ef1cbf"}
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

(deftest expected-filter-by-ay11-cy2015-results
  (let [expected-md5s {"Output_AY.csv" "89022266998810dd7f4c1c8283c16744",
                       "Output_AY_Group.csv" "e834a4f17eae2da61a542aa132dedd47",
                       "Output_State.csv" "b15b768bd3a53bfe74f0170fd9a3d481",
                       "Output_Cost.csv" "c672ba67c5f6c004dca67ace5fd4d4ab",
                       "Output_Count.csv" "6c8443f1e724ebda02e3523bca7bc1d5",
                       "Output_Need.csv" "854c72e0cfe17d9a7463b6fb5163cf13",
                       "Output_Setting.csv" "dee6241d19a76bb1b6a9058d8e4e70ce",
                       "transitions.edn" "0144496f21ad6b7c44243acfc4bf98f3"}
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
                        [f (-> (io/file output-dir f) (digest/md5))])))))))
