(ns witan.send.acceptance.validate-model-test
  (:require [clojure.test :refer [deftest is testing]]
            [witan.send.main :as m]
            [witan.send.acceptance.utils :as au]))

;; (def validation-files-to-check [;;"validation_results_state.csv"
;;                                 "validation_results_count.csv"])

;; (deftest expected-validation-results
;;   (let [config (m/read-config "data/demo/config.edn")
;;         config-for-checking (m/read-config "data/demo/config_for_checking.edn")]
;;     (au/run-validation-for-test config)
;;     (testing "Make sure new validation results are the same as the old validation results."
;;       (is (= (au/get-md5s config validation-files-to-check)
;;              (au/get-md5s config-for-checking validation-files-to-check))))))
