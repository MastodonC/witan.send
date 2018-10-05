(ns witan.send.check-inputs
  (:require [witan.send.report :as r]
            [witan.send.utils :as u]))


(defn repl-warn [& args]
  (println (str "\u001B[1;31m" (apply str args) "\u001B[m")))


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

(defn log-warnings [warnings]
  (doseq [w warnings]
    (r/info (r/bold w))
    (repl-warn w)))

(defn check-all-ages-present [transitions]
  "Checks all ages are represented in both academic years 1 and 2"
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

(defn check-joiner-leaver-gaps [transitions]
  "bring in range of ages in dataset, check that all ages have joiners and leavers"
  (let [ages (sort (distinct (concat (map :academic-year-2 transitions) (map :academic-year-1 transitions))))]
    (into (->> ages
               (drop-last 1)
               (filter (fn [a] (empty? (filter #(joiner? a %) transitions))))
               (map #(str "There are no joiners to AY " %)))
          (->> ages
               (drop 1)
               (filter (fn [a] (empty? (filter #(leaver? a %) transitions))))
               (map #(str "There are no leavers from AY " %))))))

(defn check-ages-go-up-one-year [transitions]
  "Checks academic year 2 is always academic year 1 + 1"
  (->> transitions
       (remove (fn [t] (= (+ 1 (:academic-year-1 t)) (:academic-year-2 t))))
       (map #(str "Academic years 1 and 2 are not incremental for " %))))

(defn cost-for-state? [state setting-cost]
  "Takes map containing state and returns true for match in setting-cost"
  (some? (some true? (map #(and (= (:need state) (:need %))
                                (= (:setting state) (:setting %))) setting-cost))))

(defn check-missing-costs [transitions setting-cost]
  "Produces warnings for states without costs via REPL and SEND_report.md"
  (let [states (set-of-input-states transitions false)]
    (->> states
         (remove #(cost-for-state? % setting-cost))
         (map #(str "Missing cost for state in transitions.csv: " (:need %) " " (:setting %))))))


(defn valid-ay-for-state? [state valid-ays]
  "Takes map containing state with academic year and returns true for valid states"
  (some? (some true? (map #(and (= (:setting state) (:setting %))
                                (and (>= (:academic-year state) (:min-academic-year %))
                                     (<= (:academic-year state) (:max-academic-year %))))
                          valid-ays))))


(defn check-states-in-valid-ays [transitions valid-setting-academic-years]
  "Produces warnings for states that are outside valid academic years via REPL and SEND_report.md"
  (let [states (set-of-input-states transitions true)]
    (->> states
         (remove #(valid-ay-for-state? % valid-setting-academic-years))
         (map #(str "Invalid setting for academic year in transitions.csv: "
                    (:need %) " " (:setting %) " academic-year: " (:academic-year %))))))


(defn run-input-checks [transitions setting-cost valid-setting-academic-years]
  "Takes row-maps of input CSVs and runs checks"
  (log-warnings (check-joiner-leaver-gaps transitions))
  (log-warnings (check-all-ages-present transitions))
  (log-warnings (check-ages-go-up-one-year transitions))
  (log-warnings (check-missing-costs transitions setting-cost))
  (log-warnings (check-states-in-valid-ays transitions valid-setting-academic-years)))
