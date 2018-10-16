(ns witan.send.acceptance.validate-model-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :refer [join]]
            [witan.send.main :as m]
            [witan.send.validate-model :as vm]))

(deftest expected-validation-results
  (let [expected-md5s {"validation_results_count.csv" "93a940a94f260393f80d7c3bd4b0eb81"
                       "validation_results_state.csv" "211d4f28d67d0a4c927f55d2aa90a8f3"}
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
