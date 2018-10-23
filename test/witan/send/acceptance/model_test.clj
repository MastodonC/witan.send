(ns witan.send.acceptance.model-test
  (:require [clojure.test :refer :all]
            [witan.send.send :as send]
            [witan.send.model.output :as so]
            [witan.send.main :as m]
            [clojure.java.io :as io]
            [clojure.string :refer [join]]
            [witan.send.validate-model :as v]))

(deftest expected-results
  (let [expected-md5s {"Output_AY.csv" "9e94286fd0bd9c5f11f6a05fdc059a22",
                       "Output_AY_Group.csv" "19885902c4d32da3200413f3fd561eec",
                       "Output_AY_State.csv" "e6d0f26a8256bc8fbf8909446241d247",
                       "Output_Cost.csv" "472c408577aee6634b17aa7065cba7ae",
                       "Output_Count.csv" "5b12125c72c03f1df18cf28f4d88991f",
                       "Output_Need.csv" "f51084834e693186668087d26278b322",
                       "Output_Setting.csv" "4e0665ddab7c45e966ca9df2d089f42c",
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
        config (m/config "data/demo/config.edn")
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
