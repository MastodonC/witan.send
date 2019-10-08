(ns witan.send.goal-seek
  (:require [witan.send.multi-config :as mc]
            [witan.send.main :as main]
            [witan.send.model.input :as i]
            [witan.send.model.prepare :as p]))

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

(defn state-pop [config]
  "Takes a config and returns the state to be modified and output path of
   Output_State_pop_only.csv results"
  (let [state (-> (get-in config [:transition-parameters :transitions-to-change])
                  first
                  (clojure.set/rename-keys {:setting-2 :setting :need-2 :need :academic-year-2 :ay})
                  (select-keys [:setting :need]))
        out-dir (:output-dir (:output-parameters config))
        state-pop (str (:project-dir config)
                       "/"
                       out-dir
                       "/Output_State_pop_only.csv")]
    [state state-pop]))

(defn get-target-pop [[state state-pop]]
  "Takes a vector with a state to find (e.g. {:setting :SFSS, :need :ASD}) and filepath
  for a Output_State_pop_only.csv file to return summed populations sorted by calender year
  for ease of reading in REPL"
  (->> (csv->state-pop state-pop)
       (filter #(every? identity (p/test-predicates % state))) ;; need something to build predicates
       (group-by :calendar-year)
       (map #(merge (assoc {} :year (key %))
                    (apply merge-with + (map (fn [m] (select-keys m [:population])) (val %)))))
       (sort-by :year)))

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
                     (get-target-pop (state-pop config)))
          current-pop (get-current-pop (:year target) result)
          _ (println (-> config
                         (get-in [:transition-parameters :transitions-to-change])
                         first
                         :modify-transition-by))
          _ (println result)
          diff (pop-diff-by-year (:year target) result)]
      (if (target-pop-exceeded? current-pop (:population target))
        (println "Population exceeds target population")
        (when-not (within-pop-range? (:population target) diff)
          (recur rest-configs))))))
