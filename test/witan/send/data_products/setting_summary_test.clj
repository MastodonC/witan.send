(ns witan.send.data-products.setting-summary-test
  (:require [clojure.test :refer [deftest is testing]]
            [witan.send.model.data-products.setting-summary :as sut]))

(deftest population-test
  (testing "Can produce a population for each send setting"
    (is (= {:A 2, :R 4, :B 42, :H 4, :E 1}
           (sut/population {[ 2 :Y-A ] 2
                            [ 14 :T-R ] 4
                            [ 8 :T-B ] 42
                            [ 9 :U-H ] 1
                            [ 11 :U-H ] 2
                            [ 12 :T-E ] 1
                            [ 14 :Z-H ] 1
                            [12 :NONSEND] 2})))))

(deftest cost-test
  (testing "Can produce a population for each send setting"
    (is (= {:A 2000.0, :H 3000.0, :E 1000.0}
           (sut/cost
            {[ :Y :A ] 1000.0
             [ :U :H ] 1000.0
             [ :T :E ] 1000.0
             [ :Z :H ] 1000.0}
            {[ 2 :Y-A ] 2
             [ 11 :U-H ] 2
             [ 12 :T-E ] 1
             [ 14 :Z-H ] 1
             [12 :NONSEND] 2})))))
