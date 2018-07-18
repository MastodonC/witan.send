(ns witan.send.acceptance.workspace-test
  (:require [clojure.test :refer :all]
            [witan.send.main :refer [config run-send]]))

(deftest send-test
  (is (= #{:total-in-send-by-ay :total-in-send-by-ay-group 
           :total-in-send :total-in-send-by-need :total-in-send-by-setting}
         (-> (run-send (config "demo/data/config.edn"))
             (first)
             (keys)
             (set)))))
