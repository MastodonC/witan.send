(ns witan.send.acceptance.model-test
  (:require [clojure.test :refer :all]
            [witan.send.send :as send]
            [witan.send.model.output :as so]
            [witan.send.main :as m]
            [clojure.java.io :as io]
            [clojure.string :refer [join]]))

(deftest expected-results
  (let [expected-md5s {"Output_AY.csv" "c19aa0d69a3867b9ffa17e45fafc6595",
                       "Output_AY_Group.csv" "6a94aedb0f280b4a6d2054c774c11d36",
                       "Output_AY_State.csv" "85589b91aeb38cbbe169a4c5319e0583",
                       "Output_Cost.csv" "5afc26c0a7a230b6ecae0048abf35070",
                       "Output_Count.csv" "d25aa45ff6d7e4dc46420f081c90159b",
                       "Output_Need.csv" "8f95b0c6178dbd887069dbbceb600db6",
                       "Output_Setting.csv" "2a1fc6520f1722a5d5047eb64d98c276",
                       "transitions.edn" "27ca57ba18f6024d335fd57722ef1cbf"}
        files (keys expected-md5s)
        config (m/config "data/demo/config.edn")
        output-dir (m/get-output-dir config)]
    (run! #(let [file (join "/" [output-dir %])]
             (when (.exists (io/file file))
               (io/delete-file file))) files)
    (->(send/run-send-workflow config)
       (so/output-send-results (:output-parameters config)))
    (is (= expected-md5s
           (into {} (for [f files]
                      [f (-> (io/file output-dir f) (digest/md5))]))))))
