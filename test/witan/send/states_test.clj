(ns witan.send.states-test
  (:require [clojure.test :refer [deftest testing is]]
            [witan.send.model.input.valid-states :as vs]
            [witan.send.states :refer [calculate-valid-year-settings-from-setting-academic-years
                                       calculate-valid-mover-transitions
                                       can-move?]]))

(deftest calculate-valid-year-settings-from-setting-academic-years-test
  (testing ""
    (is (calculate-valid-year-settings-from-setting-academic-years (vs/csv->valid-states "data/demo/data/valid-states.csv")))))

(deftest can-move-test
  (let [data (vs/csv->valid-states "data/demo/data/valid-states.csv")
        valid-year-settings (calculate-valid-year-settings-from-setting-academic-years data)
        valid-transitions (calculate-valid-mover-transitions data)]
    (testing ""
      (is (true? (can-move? valid-year-settings 0 :L-A valid-transitions))))))
