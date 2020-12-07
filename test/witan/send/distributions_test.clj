(ns witan.send.distributions-test
  (:require [clojure.test :refer [deftest is testing]]
            [same :refer [ish?]]
            [witan.send.distributions :as d]))

(deftest test-dirichlet-multinomial
  (testing "Test Dirichlet multinomial"
    (let [_ (d/set-seed! 42)]
      (is (ish? {:Y-H 0, :Y-L 0, :Y-D 0, :Y-K 0, :Y-A 0, :Y-I 0, :Y-B 0, :Y-J 0, :Y-C 0, :Y-E 1, :Y-R 0, :Y-G 0, :Y-N 0, :Y-S 0}
                (d/sample-dirichlet-multinomial
                 1
                 {:Y-H 0.0017035775127768312
                  :Y-L 0.04940374787052811
                  :Y-D 0.0017035775127768312
                  :Y-K 0.02555366269165247
                  :Y-A 0.02555366269165247
                  :Y-I 0.0017035775127768312
                  :Y-B 0.0017035775127768312
                  :Y-J 0.2402044293015332
                  :Y-C 0.07325383304940375
                  :Y-E 0.12095400340715501
                  :Y-R 0.0017035775127768312
                  :Y-G 0.07325383304940375
                  :Y-N 0.3594548551959114
                  :Y-S 0.023850085178875637}))))))

(deftest test-sample-beta-binomial
  (testing "Sample beta binomial"
    (let [_ (d/set-seed! 0)]
      (is (ish? 1
                (d/sample-beta-binomial 3 {:alpha 0.04196428571428571 :beta 11.958035714285714}))))
    (let [_ (d/set-seed! 42)]
      (is (ish? 0
                (d/sample-beta-binomial 3 {:alpha 0.04196428571428571 :beta 11.958035714285714}))))))
