(ns witan.send.model.input-test
  (:require [clojure.test :refer [deftest is testing]]
            [witan.send.model.input :as sut]))

(deftest int-conversion
  (testing "Can convert ints"
    (is (= 2 (sut/->int "2")))))

(deftest double-conversion
  (testing "Can convert ints"
    (is (= 2.0 (sut/->double "2.0")))))
