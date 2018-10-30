(ns witan.send.acceptance.validate-model-test
  (:require [clojure.java.io :as io]
            [clojure.string :refer [join]]
            [clojure.test :refer [deftest is testing]]
            [witan.send.main :as m]
            [witan.send.validate-model :as vm]
            [digest]))

(deftest expected-validation-results
  (let [expected-md5s {"validation_results_count.csv" "0c23282d207431a438fd1a5a94049d9c",
                       "validation_results_state.csv" "e58fe1fe2c6b6bc75866f41a367564b1"}
        files (keys expected-md5s)
        config (m/config "data/demo/config.edn")
        validation-dir (join "/" [(:project-dir config) "validation"])]
    (run! #(let [file (join "/" [validation-dir %])]
             (when (.exists (io/file file))
               (io/delete-file file))) files)
    (vm/run-send-validation config)
    (testing "Make sure new validation results are the same as the old validation results."
      (is (= expected-md5s
             (into {} (for [f files]
                        [f (-> (io/file validation-dir f) (digest/md5))])))))))
