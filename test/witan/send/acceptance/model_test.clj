(ns witan.send.acceptance.model-test
  (:require [clojure.test :refer :all]
            [witan.send.send :as send]
            [witan.send.model.output :as so]
            [witan.send.main :as m]
            [clojure.java.io :as io]
            [clojure.string :refer [join]]))

(deftest expected-results
  (let [expected-md5s {"Output_AY.csv" "87a50609a022e34478d17deed082ad7f"
                       "Output_AY_Group.csv" "51e18eafb686ce388c25a46a23a0d29a"
                       "Output_AY_State.csv" "70fd124bb0f7b5093e800bc63ce1c991"
                       "Output_Cost.csv" "1d7556cc3189491bf824a9785bddfb2c"
                       "Output_Count.csv" "1f84ff94394046f8d37da8bfa4834a0b"
                       "Output_Need.csv" "023b0a445d338f55dcfeee3a3a3677c6"
                       "Output_Setting.csv" "4f0d1e51a49f678df5637c33c604bbe1"
                       "transitions.edn" "4ea4b6076e4fed0ce36cfeeece5aa901"}
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
