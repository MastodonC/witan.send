(ns witan.send.acceptance.model-test
  (:require [clojure.test :refer :all]
            [witan.send.send :as send]
            [witan.send.model.output :as so]
            [witan.send.main :as m]
            [clojure.java.io :as io]
            [clojure.string :refer [join]]))

(deftest expected-results
  (let [expected-md5s {"Output_AY.csv" "9e94286fd0bd9c5f11f6a05fdc059a22",
                       "Output_AY_Group.csv" "19885902c4d32da3200413f3fd561eec",
                       "Output_AY_State.csv" "e6d0f26a8256bc8fbf8909446241d247",
                       "Output_Cost.csv" "472c408577aee6634b17aa7065cba7ae",
                       "Output_Count.csv" "5b12125c72c03f1df18cf28f4d88991f",
                       "Output_Need.csv" "f51084834e693186668087d26278b322",
                       "Output_Setting.csv" "4e0665ddab7c45e966ca9df2d089f42c",
                       "transitions.edn" "27ca57ba18f6024d335fd57722ef1cbf"}
        config (m/config "data/demo/config.edn")
        output-dir (m/get-output-dir config)]
    (run! #(let [file (join "/" [output-dir %])]
             (when (.exists (io/file file))
               (io/delete-file file))) files)
    (->(send/run-send-workflow config)
       (so/output-send-results (:output-parameters config)))
    (testing "checksums are unchanged"
      (is (= expected-md5s
             (into {} (for [f files]
                        [f (-> (io/file output-dir f) (digest/md5))])))))))
