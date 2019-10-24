(ns witan.send.check-inputs
  (:require [witan.send.report :as r]))

(defn repl-warn [& args]
  (println (str "\u001B[1;31m" (apply str args) "\u001B[m")))

(defn find-states
  "Takes :need and :setting key and returns map containing values from transition data"
  [need setting academic-year transition]
  (if (= :NONSEND (need transition))
    :NONSEND
    {:need (need transition) :setting (setting transition) :academic-year (academic-year transition)}))

(defn set-of-input-states
  "Takes transitions and returns set containing maps of possible states"
  [transitions with-academic-year?]
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

(defn log-warnings [warnings]
  (doseq [w warnings]
    (r/info (r/bold w))
    (repl-warn w)))

(defn check-all-ages-present
  "Checks all ages are represented in both academic years 1 and 2"
  [transitions]
  (let [transitions-to-ages (into #{} (map :academic-year-2 transitions))
        transitions-from-ages (into #{} (map :academic-year-1 transitions))
        all-ages (into transitions-to-ages transitions-from-ages)
        possible-ages (range (apply min all-ages) (+ 1 (apply max all-ages)))]
    (into (->> possible-ages
               (drop 1)
               (remove (fn [a] (some #(= a %) transitions-to-ages)))
               (map #(str "There are no transitions to AY " %)))
          (->> possible-ages
               (drop-last 1)
               (remove (fn [a] (some #(= a %) transitions-from-ages)))
               (map #(str "There are no transitions from AY " %))))))

(defn check-joiner-leaver-gaps
  "bring in range of ages in dataset, check that all ages have joiners and leavers"
  [transitions]
  (let [ages (sort (distinct (concat (map :academic-year-2 transitions) (map :academic-year-1 transitions))))]
    (into (->> ages
               (drop-last 1)
               (filter (fn [a] (empty? (filter #(joiner? a %) transitions))))
               (map #(str "There are no joiners to AY " %)))
          (->> ages
               (drop 1)
               (filter (fn [a] (empty? (filter #(leaver? a %) transitions))))
               (map #(str "There are no leavers from AY " %))))))

(defn check-ages-go-up-one-year
  "Checks academic year 2 is always academic year 1 + 1"
  [transitions]
  (->> transitions
       (remove (fn [t] (= (+ 1 (:academic-year-1 t)) (:academic-year-2 t))))
       (map #(str "Academic years 1 and 2 are not incremental for " %))))

(defn cost-for-need-setting?
  "Takes seq containing need-setting and returns true for match in costs"
  [need-setting costs]
  (some? (some true? (map #(and (= (first need-setting) (:need %))
                                (= (last need-setting) (:setting %))) costs))))

(defn count-of-input-need-settings
  "Counts need setting pair occurrences in ay1 and ay2 cols in transitions"
  [transitions]
  (let [need-setting-1 (map #(vals (select-keys % [:need-1 :setting-1])) transitions)
        need-setting-2 (map #(vals (select-keys % [:need-2 :setting-2])) transitions)
        all-need-settings (filter #(not= :NONSEND (first %)) (concat need-setting-1 need-setting-2))]
    (frequencies all-need-settings)))

(defn check-missing-costs
  "Produces warnings for states without costs via REPL and SEND_report.md"
  [transitions costs]
  (let [need-settings (count-of-input-need-settings transitions)]
    (->> need-settings
         (remove #(cost-for-need-setting? (first %) costs))
         (map #(str "Missing cost for state in transitions.csv: " (first %)
                    ", with " (last %) " occurrences")))))


(defn valid-ay-for-state?
  "Takes map containing state with academic year and returns true for valid states"
  [state valid-ays]
  (some? (some true? (map #(and (= (:setting state) (:setting %))
                                (and (>= (:academic-year state) (:min-academic-year %))
                                     (<= (:academic-year state) (:max-academic-year %))))
                          valid-ays))))


(defn check-states-in-valid-ays
  "Produces warnings for states that are outside valid academic years via REPL and SEND_report.md"
  [transitions valid-states]
  (let [states (set-of-input-states transitions true)]
    (->> states
         (remove #(valid-ay-for-state? % valid-states))
         (map #(str "Invalid setting for academic year in transitions.csv: "
                    (:need %) " " (:setting %) " academic-year: " (:academic-year %))))))

(defn miscoded-nonsend?
  "Checks setting-n and need-n are both NONSEND"
  [transition]
  (or (and (not= (:setting-1 transition) :NONSEND) (= (:need-1 transition) :NONSEND))
      (and (= (:setting-1 transition) :NONSEND) (not= (:need-1 transition) :NONSEND))
      (and (not= (:setting-2 transition) :NONSEND) (= (:need-2 transition) :NONSEND))
      (and (= (:setting-2 transition) :NONSEND) (not= (:need-2 transition) :NONSEND))))

(defn check-nonsend-states-valid
  "Checks setting-n and need-n are both NONSEND"
  [transitions]
  (let [count-nonsend-errors (->> transitions
                                  (filter miscoded-nonsend?)
                                  count)]
    (when (> count-nonsend-errors 0)
      (str "There are " count-nonsend-errors " occurrences where only one of need-1 or setting-1 are Non-SEND in transitions"))))



(defn run-input-checks
  "Takes row-maps of input CSVs and runs checks"
  [transitions costs valid-states]
  (log-warnings (check-joiner-leaver-gaps transitions))
  (log-warnings (check-all-ages-present transitions))
  (log-warnings (check-ages-go-up-one-year transitions))
  (log-warnings (check-missing-costs transitions costs))
  (log-warnings (check-states-in-valid-ays transitions valid-states))
  (log-warnings (check-nonsend-states-valid transitions)))
