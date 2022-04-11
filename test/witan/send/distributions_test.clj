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
                  :Y-S 0.023850085178875637}))))
    (let [_ (d/set-seed! 38)]
      (is (= [{:Y-H 0, :Y-L 5, :Y-D 0, :Y-K 0, :Y-A 0, :Y-I 0, :Y-B 0, :Y-J 6, :Y-C 0, :Y-E 12, :Y-R 0, :Y-G 0, :Y-N 27, :Y-S 0}
              {:Y-H 0, :Y-L 0, :Y-D 0, :Y-K 0, :Y-A 0, :Y-I 0, :Y-B 0, :Y-J 0, :Y-C 0, :Y-E 7, :Y-R 0, :Y-G 41, :Y-N 2}
              {:Y-H 0, :Y-L 0, :Y-D 0, :Y-K 0, :Y-A 0, :Y-I 0, :Y-B 0, :Y-J 4, :Y-C 0, :Y-E 0, :Y-R 0, :Y-G 6, :Y-N 40, :Y-S 0}
              {:Y-H 0, :Y-L 0, :Y-D 0, :Y-K 0, :Y-A 0, :Y-I 0, :Y-B 0, :Y-J 0, :Y-C 0, :Y-E 3, :Y-R 0, :Y-G 25, :Y-N 22}
              {:Y-H 0, :Y-L 2, :Y-D 0, :Y-K 0, :Y-A 0, :Y-I 0, :Y-B 0, :Y-J 0, :Y-C 21, :Y-E 27, :Y-R 0, :Y-G 0, :Y-N 0, :Y-S 0}]
             (into []
                   (map (fn [_]
                          (d/sample-dirichlet-multinomial
                           50
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
                            :Y-S 0.023850085178875637})))
                   (range 5))))))

  (testing "Throws if no alphas"
    (is (thrown? clojure.lang.ExceptionInfo
                 (d/sample-dirichlet-multinomial 3 nil)))))

(deftest test-sample-beta-binomial
  (testing "Sample beta binomial"
    (let [_ (d/set-seed! 0)]
      (is (ish? 1
                (d/sample-beta-binomial 3 {:alpha 0.04196428571428571 :beta 11.958035714285714}))))
    (let [_ (d/set-seed! 42)]
      (is (ish? 0
                (d/sample-beta-binomial 3 {:alpha 0.04196428571428571 :beta 11.958035714285714}))))
    (let [_ (d/set-seed! 42)]
      (is (zero?
           (d/sample-beta-binomial -3 {:alpha 0.04196428571428571 :beta 11.958035714285714}))))
    (let [_ (d/set-seed! 42)]
      (is (= [22 21 27 21 20 24 20 16 23 28 11 26 25 27 26 21 27 25 32 37 29 21 22 25 23 23 23 29 24 27 21 28 23 27 18 29 28 19 19 32]
             (into []
                   (map (fn [_]
                          (d/sample-beta-binomial 50 {:alpha 40.04196428571428571 :beta 41.958035714285714})))
                   (range 40)))))
    (let [_ (d/set-seed! 50)]
      (is (= [0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 5 0 4 0 0 0 7 0 0 12 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0]
             (into []
                   (map (fn [_]
                          (d/sample-beta-binomial 500 {:alpha 0.04196428571428571 :beta 11.958035714285714})))
                   (range 40)))))))
