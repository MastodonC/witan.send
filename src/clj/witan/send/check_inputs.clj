(ns witan.send.check-inputs
  (:require [witan.send.report :as report]))

(defn repl-warn [& args]
  (println (str "\u001B[1;31m" (apply str args) "\u001B[m")))

(defn cost-for-state? [state setting-cost]
  "Takes map containing state and returns true for match in setting-cost"
  (some? (some true? (map #(and (= (:need state) (:need %))
                                (= (:setting state) (:setting %))) setting-cost))))


(defn find-states [need setting transition]
  "Takes :need and :setting key and returns map containing values from transition data"
  (if (= :NONSEND (need transition))
    [:NONSEND]
    {:need (need transition ) :setting (setting transition)}))


(defn set-of-input-states [transitions]
  "Takes row maps from transitions.csv and returns set containing maps of possible states"
  (let [state-1s (map #(find-states :need-1 :setting-1 %) transitions)
        state-2s (map #(find-states :need-2 :setting-2 %) transitions)]
    (-> (concat state-1s state-2s)
        (set)
        (disj [:NONSEND]))))


(defn check-missing-costs [transitions setting-cost]
  (let [states (set-of-input-states transitions)]
    (doseq [s states]
      (if-not (cost-for-state? s setting-cost)
        (do (report/info (report/bold "Inconsistent inputs!")
                         " Missing cost for state in transitions.csv: "
                         (str (:need s)) (str (:setting s)) "\n")
            (repl-warn "Entry in transitions.csv without cost: "(:need s) (:setting s)))))))



;

; every state has a cost

; check all input in transitions are in valid states

; check population complete for each input year



