(ns witan.send.acceptance.model-test
  (:require [clojure.test :refer :all]
            [witan.send.main :refer [config run-send]]))

(deftest send-test
  (is (= #{:total-in-send-by-ay :total-in-send-by-ay-group :by-state :total-cost
           :total-in-send :total-in-send-by-need :total-in-send-by-setting}
         (-> (run-send (config "data/demo"))
             (:send-output)
             (first)
             (keys)
             (set)))))
