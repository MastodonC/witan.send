(ns witan.send.acceptance.model-test
  (:require [clojure.test :refer :all]
            [witan.send.send :as send]
            [witan.send.model.output :as so]
            [witan.send.main :as m]
            [clojure.java.io :as io]
            [clojure.string :refer [join]]))

(deftest expected-results
  (let [expected-md5s {"Output_AY.csv" "d87dd99f4910c7958240bea30bf2e021"
                       "Output_AY_Group.csv" "748ac7cc64e72da81b14b74a6820a7c5"
                       "Output_AY_State.csv" "7c4a6c769aacbb91e6554f777701f414"
                       "Output_Cost.csv" "01f1da9dcdb093ac821f58ea40f3dc2a"
                       "Output_Count.csv" "4648435ea177ffcfc9605c03e9fe0031"
                       "Output_Need.csv" "338edcad0f4545c4462bd037a28a9734"
                       "Output_Setting.csv" "d78969fd629ec5e56b48eff31092fc7c"
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
