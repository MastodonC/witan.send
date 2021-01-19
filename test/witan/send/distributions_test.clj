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
              {:Y-H 0, :Y-L 0, :Y-D 0, :Y-K 0, :Y-A 0, :Y-I 0, :Y-B 0, :Y-J 0, :Y-C 0, :Y-E 7, :Y-R 0, :Y-G 43, :Y-N 0}
              {:Y-H 0, :Y-L 0, :Y-D 0, :Y-K 0, :Y-A 0, :Y-I 0, :Y-B 0, :Y-J 4, :Y-C 0, :Y-E 0, :Y-R 0, :Y-G 6, :Y-N 40, :Y-S 0}
              {:Y-H 0, :Y-L 0, :Y-D 0, :Y-K 0, :Y-A 0, :Y-I 0, :Y-B 0, :Y-J 0, :Y-C 0, :Y-E 3, :Y-R 0, :Y-G 24, :Y-N 23}
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
      (is (= [22 21 27 20 20 24 20 16 23 28 20 26 25 27 26 21 27 25 32 24 29 21 24 25 23 23 23 29 24 27 21 26 39 27 18 20 28 22 33 32 24 26 25 22 24 23 24 28 24 20 31 24 26 22 24 26 23 22 28 26 26 22 25 30 25 29 36 21 21 19 20 26 23 24 21 25 25 24 23 23 22 21 29 23 30 24 26 21 25 17 24 29 24 32 28 24 20 25 22 26 11 29 21 27 20 26 28 30 24 22 25 26 27 19 21 29 20 20 22 20 26 19 23 23 22 22 22 33 27 27 28 26 32 33 24 23 21 24 26 24 20 24 19 28 22 30 21 26 21 25 29 24 25 18 29 25 27 32 23 28 21 27 23 28 21 32 25 28 29 21 18 29 26 18 26 17 27 28 26 26 31 19 23 24 24 25 22 23 23 21 27 32 26 33 30 27 36 20 29 25]
             (into []
                   (map (fn [_]
                          (d/sample-beta-binomial 50 {:alpha 40.04196428571428571 :beta 41.958035714285714})))
                   (range 200)))))
    (let [_ (d/set-seed! 50)]
      (is (= [0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 5 0 4 0 0 0 7 0 0 12 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 14 0 0 0 0 0 0 0 0 0 0 75 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 72 0 0 0 0 29 4 0 0 1 0 0 9 3 0 0 0 0 3 0 45 29 0 0 0 0 0 0 0 0 0 0 0 2 0 22 0 0 3 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 20 0 0]
             (into []
                   (map (fn [_]
                          (d/sample-beta-binomial 500 {:alpha 0.04196428571428571 :beta 11.958035714285714})))
                   (range 200)))))))