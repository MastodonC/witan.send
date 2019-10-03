(ns witan.send.goal-seek
  (:require [witan.send.multi-config :as mc]
            [witan.send.main :as main]
            [witan.send.model.input :as i]))

(defn update-transition-modifier [m n]
  "Takes a map of keys partially matching a transition and a new modifier"
  [(assoc m :modify-transition-by n)])

(defn create-transition-modifier-seq [m start end step]
  "Takes a map of keys partially matching a transition and start, end and step range to modify
   the transition by, creating a sequence of configs for :transitions-to-change"
  [[[:transition-parameters :transitions-to-change]
    (mapv #(update-transition-modifier m %) (range start end step))]])

(defn csv->state-pop [filepath]
  (i/csv->x (map #(-> % (update :calendar-year i/->int)
                      (update :academic-year i/->int)
                      (update :need keyword)
                      (update :setting keyword)
                      (update :population i/->int)))
            filepath))

(defn get-target-pop [config]
  (let [state (-> (get-in config [:transition-parameters :transitions-to-change])
                  first
                  (clojure.set/rename-keys {:setting-2 :setting :need-2 :need :academic-year-2 :ay})
                  (select-keys [:setting :need]))
        out-dir (:output-dir (:output-parameters config))
        state-pop (str (:project-dir config)
                       "/"
                       out-dir
                       "/Output_State_pop_only.csv")
        results (filter #(and (= (:setting state) (:setting %))
                              (= (:need state) (:need %)))
                        (csv->state-pop state-pop))]
    (map #(merge (assoc {} :year (key %))
                 (apply merge-with + (map (fn [m] (select-keys m [:population])) (val %))))
         (group-by :calendar-year results))))

(defn update-results-path [config]
  (assoc-in config [:output-parameters :output-dir] (str "results/" (get-in config [:output-parameters :output-dir]))))

(defn get-current-pop [year result]
  (:population (first (filter #(= year (:year %)) result))))

(defn pop-diff-by-year [year result]
  (- (get-current-pop year result)
     (get-current-pop (- year 1) result)))

(defn within-pop-range? [pop diff]
  (and (<= (first pop) diff)
       (>= (second pop) diff)))

(defn target-pop-exceeded? [pop target-pop]
  (> pop (apply max target-pop)))

(defn target-results [m start end step base-config target]
  "Takes a map of keys partially matching a transition, a start, end and step range to modify
   the transition by, a template config and a map containing a target population range and year

   e.g. {:year 2019 :population [6 12]}"
  (loop [configs (map update-results-path (mc/generate-configs (create-transition-modifier-seq m start end step) base-config))]
    (let [[config & rest-configs] configs
          result (do (main/run-recorded-send config)
                     (get-target-pop config))
          current-pop (get-current-pop (:year target) result)
          diff (pop-diff-by-year (:year target) result)]
      (if (target-pop-exceeded? current-pop (:population target))
        (println "Population already exceeds target population")
        (when-not (within-pop-range? (:population target) diff)
          (recur rest-configs))))))
