(ns witan.send.check-inputs
  (:require [witan.send.report :as r]))


(defn repl-warn [& args]
  (println (str "\u001B[1;31m" (apply str args) "\u001B[m")))


(defn cost-for-state? [state setting-cost]
  "Takes map containing state and returns true for match in setting-cost"
  (some? (some true? (map #(and (= (:need state) (:need %))
                                (= (:setting state) (:setting %))) setting-cost))))


(defn find-states [need setting academic-year transition]
  "Takes :need and :setting key and returns map containing values from transition data"
  (if (= :NONSEND (need transition))
    :NONSEND
    {:need (need transition) :setting (setting transition) :academic-year (academic-year transition)}))


(defn set-of-input-states [transitions with-academic-year?]
  "Takes transitions and returns set containing maps of possible states"
  (let [state-1s (map #(find-states :need-1 :setting-1 :academic-year-1 %) transitions)
        state-2s (map #(find-states :need-2 :setting-2 :academic-year-2 %) transitions)
        all-states (filter #(not= :NONSEND %) (concat state-1s state-2s))]
    (if with-academic-year?
      (set all-states)
      (set (map #(dissoc % :academic-year) all-states)))))


(defn check-missing-costs [transitions setting-cost]
  "Produces warnings for states without costs via REPL and SEND_report.md"
  (let [states (set-of-input-states transitions false)]
    (doseq [s states]
      (if-not (cost-for-state? s setting-cost)
        (do (r/info (r/bold "Inconsistent inputs!")
                         " Missing cost for state in transitions.csv: "
                         (str (:need s) (:setting s)) "\n")
            (repl-warn "Entry in transitions without cost: "(:need s) (:setting s)))))))


(defn valid-ay-for-state? [state valid-ays]
  "Takes map containing state with academic year and returns true for valid states"
  (some? (some true? (map #(and (= (:setting state) (:setting %))
                                (and (>= (:academic-year state) (:min-academic-year %))
                                     (<= (:academic-year state) (:max-academic-year %))))
                          valid-ays))))


(defn check-states-in-valid-ays [transitions valid-setting-academic-years]
  "Produces warnings for states that are outside valid academic years via REPL and SEND_report.md"
  (let [states (set-of-input-states transitions true)]
    (doseq [s states]
      (if-not (valid-ay-for-state? s valid-setting-academic-years)
        (do (r/info (r/bold "Inconsistent inputs!")
                    " Invalid setting for academic year in transitions.csv: "
                    (str (:need s) (:setting s) " academic-year:" (:academic-year s)))
            (repl-warn "Invalid academic year in transitions: "  (str (:need s) (:setting s) " academic-year:" (:academic-year s))))))))


(defn run-input-checks [transitions setting-cost valid-setting-academic-years]
  "Takes row-maps of input CSVs and runs checks"
  (check-missing-costs transitions setting-cost)
  (check-states-in-valid-ays transitions valid-setting-academic-years))




