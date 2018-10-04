(ns witan.send.check-inputs
  (:require [witan.send.report :as r]
            [witan.send.utils :as u]))


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

(defn joiner? [age transition]
  (and (= (:academic-year-2 transition) age) (= (:setting-1 transition) :NONSEND)))

(defn leaver? [age transition]
  (and (= (:academic-year-1 transition) age) (= (:setting-2 transition) :NONSEND)))

(defn check-all-ages-present [transitions]
  "Checks all ages are represented in both academic years 1 and 2"
  (let [transitions-to-ages (distinct (map :academic-year-2 transitions))
        transitions-from-ages (distinct (map :academic-year-1 transitions))
        all-ages (sort (distinct (concat transitions-to-ages transitions-from-ages)))
        possible-ages (range (first all-ages) (+ 1 (last all-ages)))]
    (doseq [a (drop 1 possible-ages)]
      (when-not (some #(= a %) transitions-to-ages)
        (do (r/info (r/bold (str "There are no transitions to AY " a)))
            (repl-warn (str "There are no transitions to AY " a)))))
    (doseq [a (drop-last 1 possible-ages)]
      (when-not (some #(= a %) transitions-from-ages)
        (do (r/info (r/bold (str "There are no transitions from AY " a)))
            (repl-warn (str "There are no transitions from AY " a)))))))

(defn check-joiner-leaver-gaps [transitions]
  "bring in range of ages in dataset, check that all ages have joiners and leavers"
  (let [ages (sort (distinct (concat (map :academic-year-2 transitions) (map :academic-year-1 transitions))))]
    (doseq [a (drop-last 1 ages)]
      (when (empty? (filter #(joiner? a %) transitions))
        (do (r/info (r/bold (str "There are no joiners to AY " a)))
            (repl-warn (str "There are no joiners to AY " a)))))
    (doseq [a (drop 1 ages)]
      (when (empty? (filter #(leaver? a %) transitions))
        (do (r/info (r/bold (str "There are no leavers from AY " a)))
            (repl-warn (str "There are no leavers from AY " a)))))))

(defn check-ages-go-up-one-year [transitions]
  "Checks academic year 2 is always academic year 1 + 1"
  (doseq [t transitions]
    (when-not (= (+ 1 (:academic-year-1 t)) (:academic-year-2 t))
      (do (r/info (r/bold (str "Acamdeic years 1 and 2 are not incremental for " t)))
          (repl-warn (str "Acamdeic years 1 and 2 are not incremental for " t))))))

(defn check-missing-costs [transitions setting-cost]
  "Produces warnings for states without costs via REPL and SEND_report.md"
  (let [states (set-of-input-states transitions false)]
    (doseq [s states]
      (when-not (cost-for-state? s setting-cost)
        (do (r/info (r/bold "Inconsistent inputs!")
                    " Missing cost for state in transitions.csv: "
                    (str (:need s) (:setting s)))
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
  (check-joiner-leaver-gaps transitions)
  (check-all-ages-present transitions)
  (check-ages-go-up-one-year transitions)
  (check-missing-costs transitions setting-cost)
  (check-states-in-valid-ays transitions valid-setting-academic-years))
