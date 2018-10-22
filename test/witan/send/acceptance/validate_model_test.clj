(ns witan.send.acceptance.validate-model-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :refer [join]]
            [witan.send.main :as m]
            [witan.send.validate-model :as vm]))

(deftest expected-validation-results
  (let [expected-md5s {"validation_results_count.csv" "65d53b6c765582fa58761c29bde1e43f",
                       "validation_results_state.csv" "4ec0f87271dbe91cdfeb79595c317f0f"}
        files (keys expected-md5s)
        config (m/config "data/demo/config.edn")
        validation-dir (join "/" [(:project-dir config) "validation"])]
    (run! #(let [file (join "/" [validation-dir %])]
             (when (.exists (io/file file))
               (io/delete-file file))) files)
    (vm/run-send-validation config)
    (is (= expected-md5s
           (into {} (for [f files]
                      [f (-> (io/file validation-dir f) (digest/md5))]))))))
