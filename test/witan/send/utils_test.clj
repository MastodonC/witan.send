(ns witan.send.utils-test
  (:require [clojure.test :refer :all]
            [witan.send.utils :refer :all]
            [clojure.core.matrix.dataset :as ds]
            [schema.core :as s]
            [witan.send.schemas :as sc]
            [witan.send.send-test :as st]))

(def ds-for-matrix (st/get-individual-input :transition-matrix))

(deftest full-trans-mat-test
  (testing "The transition matrix is generated with the correct format"
    (let [matrix-formatted (full-trans-mat sc/States [0 26] ds-for-matrix)]
      (is (s/validate sc/TransitionMatrix matrix-formatted)))))
