(ns witan.send.utils-test
  (:require [clojure.test :refer :all]
            [witan.send.utils :refer :all]
            [clojure.core.matrix.dataset :as ds]))

(def ds-for-matrix (ds/dataset [{:age 10 :from-state :Non-SEND
                                 :to-state :ASD-Mainstream :probability 0.3}
                                {:age 10 :from-state :Non-SEND
                                 :to-state :Non-SEND :probability 0.7}
                                {:age 11 :from-state :Non-SEND
                                 :to-state :ASD-Mainstream :probability 0.1}
                                {:age 11 :from-state :Non-SEND
                                 :to-state :Non-SEND :probability 0.9}
                                {:age 12 :from-state :Non-SEND
                                 :to-state :ASD-Mainstream :probability 0.2}
                                {:age 12 :from-state :Non-SEND
                                 :to-state :Non-SEND :probability 0.8}]))

(def transition-matrix {:10 {[:Non-SEND] {:Non-SEND 0.7 :ASD-Mainstream 0.3}}
                        :11 {[:Non-SEND] {:Non-SEND 0.9 :ASD-Mainstream 0.1}}
                        :12 {[:Non-SEND] {:Non-SEND 0.8 :ASD-Mainstream 0.2}}})

(deftest full-trans-mat-test
  (testing "The transition matrix is generated with the correct format"
    (is (= (full-trans-mat [:Non-SEND] [10 12] ds-for-matrix)
           transition-matrix))))
