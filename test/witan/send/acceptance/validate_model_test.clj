(ns witan.send.acceptance.validate-model-test
  (:require [clojure.java.io :as io]
            [clojure.string :refer [join]]
            [clojure.test :refer [deftest is testing]]
            digest
            [witan.send.main :as m]
            [witan.send.validate-model :as vm]))

(deftest expected-validation-results
  (let [expected-md5s {"validation_results_count.csv" "b4a4402c4676e456d69ff988c773e221"
                       "validation_results_state.csv" "ccba3a0589a0b0deb01c67c7f028ab45"}
        files (keys expected-md5s)
        config (m/read-config "data/demo/config.edn")
        output-dir (get-in config [:output-parameters :output-dir])
        validation-dir (join "/" [(:project-dir config) (str "validation-" output-dir)])]
    (run! #(let [file (join "/" [validation-dir %])]
             (when (.exists (io/file file))
               (io/delete-file file))) files)
    (vm/run-send-validation config)
    (testing "Make sure new validation results are the same as the old validation results."
      (is (= expected-md5s
             (into {} (for [f files]
                        [f (-> (io/file validation-dir f) (digest/md5))])))))))
