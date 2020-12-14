(ns witan.send.maths-test
  (:require [clojure.test :refer [deftest is testing]]
            [witan.send.maths :as sut]))

(deftest some+-test
  (testing "Adding to nil is identity"
    (let [n (* 10000 (rand))]
      (is (= n (sut/some+ nil n)))))
  (testing "Adding n to n is (* 2 n)"
    (is (let [n (* 10000 (rand))]
          (= (* 2 n) (sut/some+ n n))))))

(deftest round-test
  (testing "Rounds to 0.0"
    (is (= 0.0 (sut/round 0.0009))))
  (testing "Rounds to 0.01"
    (is (= 0.01 (sut/round 0.009)))))

(deftest round0-test
  (testing "Rounds to 1.0"
    (is (= 1.0 (sut/round0 0.9))))
  (testing "Rounds to the same number"
    (is (= (sut/round0 1) (sut/round0 0.9999))))
  (testing "Rounds to the same number"
    (is (= (sut/round0 10.0) (sut/round0 10.000009)))))
