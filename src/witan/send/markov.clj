(ns witan.send.markov
  (:require [markov-chains.core :as mc]
            [clojure.core.matrix.dataset :as ds]
            [witan.datasets :as wds]
            [witan.send.utils :as u]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is a simplified example to show how the SEND model should work.                          ;;
;; In this example, children ages 0-6 are eligible for SEND and at age 7 and older they are not. ;;
;; There are 2 need types and 2 placement types.                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;age-dependent transition matrices
(def send-matrix-age-0 {[:Non-SEND] {:Non-SEND 1 :N1P1 0 :N1P2 0 :N2P1 0 :N2P2 0 :Too-old 0}
                        [:N1P1] {:Non-SEND 0 :N1P1 1 :N1P2 0 :N2P1 0 :N2P2 0 :Too-old 0}
                        [:N1P2] {:Non-SEND 0 :N1P1 0 :N1P2 1 :N2P1 0 :N2P2 0 :Too-old 0}
                        [:N2P1] {:Non-SEND 0 :N1P1 0 :N1P2 0 :N2P1 1 :N2P2 0 :Too-old 0}
                        [:N2P2] {:Non-SEND 0 :N1P1 0 :N1P2 0 :N2P1 0 :N2P2 1 :Too-old 0}
                        [:Too-old] {:Non-SEND 0 :N1P1 0  :N1P2 0  :N2P1 0 :N2P2 0 :Too-old 1}})

(def send-matrix-age-1 {[:Non-SEND] {:Non-SEND 0.995 :N1P1 0.00025 :N1P2 0.00025 :N2P1 0.00025 :N2P2 0.00025 :Too-old 0}
                        [:N1P1] {:Non-SEND 0 :N1P1 1 :N1P2 0 :N2P1 0 :N2P2 0 :Too-old 0}
                        [:N1P2] {:Non-SEND 0 :N1P1 0 :N1P2 1 :N2P1 0 :N2P2 0 :Too-old 0}
                        [:N2P1] {:Non-SEND 0 :N1P1 0 :N1P2 0 :N2P1 1 :N2P2 0 :Too-old 0}
                        [:N2P2] {:Non-SEND 0 :N1P1 0 :N1P2 0 :N2P1 0 :N2P2 1 :Too-old 0}
                        [:Too-old] {:Non-SEND 0 :N1P1 0 :N1P2 0 :N2P1 0 :N2P2 0 :Too-old 1}})

(def send-matrix-age-2 {[:Non-SEND] {:Non-SEND 0.991 :N1P1 0.00035 :N1P2 0.00035  :N2P1 0.00035 :N2P2 0.00035 :Too-old 0}
                        [:N1P1] {:Non-SEND 0 :N1P1 1 :N1P2 0 :N2P1 0 :N2P2 0 :Too-old 0}
                        [:N1P2] {:Non-SEND 0 :N1P1 0 :N1P2 1 :N2P1 0 :N2P2 0 :Too-old 0}
                        [:N2P1] {:Non-SEND 0 :N1P1 0 :N1P2 0 :N2P1 1 :N2P2 0 :Too-old 0}
                        [:N2P2] {:Non-SEND 0 :N1P1 0 :N1P2 0 :N2P1 0 :N2P2 1 :Too-old 0}
                        [:Too-old] {:Non-SEND 0 :N1P1 0 :N1P2 0 :N2P1 0 :N2P2 0 :Too-old 1}})

(def send-matrix-age-3 {[:Non-SEND] {:Non-SEND 0.99 :N1P1 0.00045 :N1P2 0.00035  :N2P1 0.00035 :N2P2 0.00035 :Too-old 0}
                        [:N1P1] {:Non-SEND 0 :N1P1 1 :N1P2 0 :N2P1 0 :N2P2 0 :Too-old 0}
                        [:N1P2] {:Non-SEND 0 :N1P1 0 :N1P2 1 :N2P1 0 :N2P2 0 :Too-old 0}
                        [:N2P1] {:Non-SEND 0 :N1P1 0 :N1P2 0 :N2P1 1 :N2P2 0 :Too-old 0}
                        [:N2P2] {:Non-SEND 0 :N1P1 0 :N1P2 0 :N2P1 0 :N2P2 1 :Too-old 0}
                        [:Too-old] {:Non-SEND 0 :N1P1 0 :N1P2 0 :N2P1 0 :N2P2 0 :Too-old 1}})

(def send-matrix-age-4 {[:Non-SEND] {:Non-SEND 0.95 :N1P1 0.03  :N1P2 0.005  :N2P1 0.005 :N2P2 0.01 :Too-old 0}
                        [:N1P1] {:Non-SEND 0 :N1P1 1  :N1P2 0  :N2P1 0 :N2P2 0 :Too-old 0}
                        [:N1P2] {:Non-SEND 0 :N1P1 0  :N1P2 1  :N2P1 0 :N2P2 0 :Too-old 0}
                        [:N2P1] {:Non-SEND 0 :N1P1 0  :N1P2 0  :N2P1 1 :N2P2 0 :Too-old 0}
                        [:N2P2] {:Non-SEND 0 :N1P1 0  :N1P2 0  :N2P1 0 :N2P2 1 :Too-old 0}
                        [:Too-old] {:Non-SEND 0 :N1P1 0  :N1P2 0  :N2P1 0 :N2P2 0 :Too-old 1}})

(def send-matrix-age-5 {[:Non-SEND] {:Non-SEND 0.96 :N1P1 0.02  :N1P2 0.005  :N2P1 0.005 :N2P2 0.01 :Too-old 0}
                        [:N1P1] {:Non-SEND 0.05 :N1P1 0.9  :N1P2 0.05  :N2P1 0 :N2P2 0 :Too-old 0}
                        [:N1P2] {:Non-SEND 0 :N1P1 0  :N1P2 1  :N2P1 0 :N2P2 0 :Too-old 0}
                        [:N2P1] {:Non-SEND 0.03 :N1P1 0  :N1P2 0  :N2P1 0.96 :N2P2 0.1 :Too-old 0}
                        [:N2P2] {:Non-SEND 0 :N1P1 0  :N1P2 0  :N2P1 0 :N2P2 1 :Too-old 0}
                        [:Too-old] {:Non-SEND 0 :N1P1 0  :N1P2 0  :N2P1 0 :N2P2 0 :Too-old 1}})

(def send-matrix-age-6 {[:Non-SEND] {:Non-SEND 0 :N1P1 0  :N1P2 0  :N2P1 0 :N2P2 0 :Too-old 1}
                        [:N1P1] {:Non-SEND 0 :N1P1 0  :N1P2 0  :N2P1 0 :N2P2 0 :Too-old 1}
                        [:N1P2] {:Non-SEND 0 :N1P1 0  :N1P2 0  :N2P1 0 :N2P2 0 :Too-old 1}
                        [:N2P1] {:Non-SEND 0 :N1P1 0  :N1P2 0  :N2P1 0 :N2P2 0 :Too-old 1}
                        [:N2P2] {:Non-SEND 0 :N1P1 0  :N1P2 0  :N2P1 0 :N2P2 0 :Too-old 1}
                        [:Too-old] {:Non-SEND 0 :N1P1 0  :N1P2 0  :N2P1 0 :N2P2 0 :Too-old 1}})

;;combine age-dependent matrices into one master transition matrix
(def transition-matrix {:0 send-matrix-age-0
                        :1 send-matrix-age-1
                        :2 send-matrix-age-2
                        :3 send-matrix-age-3
                        :4 send-matrix-age-4
                        :5 send-matrix-age-5
                        :6 send-matrix-age-6
                        :7 send-matrix-age-6})

(def num-simulations 1000)

;;this is equivalent to the output of the get-historic-population workflow fn. 
;;In the simplified example there are only 2 children in the historic population.
(def historic-population (ds/dataset {:sim-num (vec (flatten (repeat 2 (range 1 (inc num-simulations)))))
                                      :id (vec (flatten (concat (repeat num-simulations 1)
                                                                (repeat num-simulations 2))))
                                      :year (vec (flatten (repeat (* 2 num-simulations) 2016)))
                                      :age (vec (flatten (concat (repeat num-simulations 4)
                                                                (repeat num-simulations 2))))
                                      :state (vec (concat (mapcat identity (repeat num-simulations [[:Non-SEND]])) (mapcat identity (repeat num-simulations [[:N1P2]]))))}))

;;this is equivalent to the output of the population-change workflow fn
;;it has a row for each individual (& simulation) who needs to be added to the 2016 cohort in
;;future years based on the population projeciton.
;;In the simplified example there is 1 new child born in 2017 who is added.
(def extra-population (ds/dataset {:sim-num (vec (flatten (range 1 (inc num-simulations))))
                                   :id (vec (flatten (repeat num-simulations 3)))
                                   :year (vec (flatten (repeat num-simulations 2017)))
                                   :age (vec (flatten (repeat num-simulations 0)))
                                   :state (vec (concat (mapcat identity (repeat num-simulations [[:Non-SEND]]))))}))

;;This is equivalent to total population before the loop starts
;;Here in 2016 there is a population of 2 (a 4 yr old & 2 yr old)
;;In 2017, there is one newborn who was not there in 2017
(def total-starting-population (ds/join-rows historic-population extra-population))

(defn project-SEND [total-popn proj-start-year proj-end-year trans-mat]
  (loop [total-popn total-popn
         current-year proj-start-year]
    ;;equivalent of select starting population, apply state changes, append to total population
    (let [starting-popn (wds/select-from-ds total-popn {:year {:eq (dec current-year)}})
          transitioned-popn (-> starting-popn
                                (wds/add-derived-column :state2 [:age :state]
                                                        (fn [a s] (vec (take 1 (mc/generate s ((keyword (str a)) trans-mat))))))
                                (wds/add-derived-column :year [:year] (fn [y] (inc y)))
                                (wds/add-derived-column :age [:age] (fn [a]
                                                                      (cond
                                                                        (< a 6) (inc a)
                                                                        (>= a 6) 7)))
                                (ds/select-columns [:sim-num :id :year :age :state2])
                                (ds/rename-columns {:state2 :state}))
          new-total-popn (ds/join-rows total-popn transitioned-popn)]

      ;;equivalent to finish-looping?
      (if (>= current-year proj-end-year)
        ;;yes: return ordered demand projection
        (u/order-ds new-total-popn [:id :year :sim-num])
        ;;no: increment current year and loop again
        (recur new-total-popn
               (inc current-year))))))

;;this is the ungrouped output from the loop
(def send-projection-2021 (project-SEND total-starting-population
                                        2017
                                        2021
                                        transition-matrix))

;;doesn't include all possible year-age-state combos or any confidence intervals
(def grouped-send-projection (-> send-projection-2021
                                 (wds/rollup :count :id
                                             [:year :age :state])
                                 (ds/rename-columns {:id :count})
                                 (wds/add-derived-column :mean [:count] (fn [c] (/ c num-simulations)))
                                 (u/order-ds [:year :age :state])))
