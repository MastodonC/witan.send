(ns witan.send.distributions-test
  (:require [clojure.test :refer [deftest is testing]]
            [witan.send.distributions :as sut]))

(deftest test-beta-binomial-draw
  (testing "Does draw produce the same series"
    (is (= [0 0 0 0 0 1 1 0 0 0]
           (let [_ (sut/set-seed! 42)]
             (repeatedly 10
                         #(sut/sample-beta-binomial 1
                                                    {:alpha 1.01964285714285710
                                                     :beta 1.058035714285714})))))))


(deftest test-sample-dirichlet-multinomial
  (testing "Does draw produce the same series"
    (is (= [{:X-H 0, :X-J 1, :X-F 0, :X-C 0} {:X-H 0, :X-J 0, :X-F 1} {:X-H 0, :X-J 0, :X-F 1, :X-C 0} {:X-H 0, :X-J 0, :X-F 1} {:X-H 0, :X-J 0, :X-F 1, :X-C 0} {:X-H 0, :X-J 0, :X-F 1} {:X-H 0, :X-J 1, :X-F 0, :X-C 0} {:X-H 1, :X-J 0, :X-F 0, :X-C 0} {:X-H 0, :X-J 1, :X-F 0} {:X-H 0, :X-J 0, :X-F 0, :X-C 1}]
           (let [_ (sut/set-seed! 42)]
             (repeatedly 10 #(sut/sample-dirichlet-multinomial 1
                                                               {:X-H 0.026244343891402722
                                                                :X-J 0.06425339366515838
                                                                :X-F 0.06425339366515838
                                                                :X-C 0.013574660633484165})))))))
