(ns witan.send.goal-seek
  (:require [witan.send.multi-config :as mc]
            [witan.send.main :as main]
            [witan.send.model.input :as i]
            [witan.send.model.prepare :as p]
            [witan.send.maths :as math]))

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
  (if-let [out (:population (first (filter #(= year (:year %)) result)))]
    out
    0))

(defn pop-diff-by-year [year result]
  (- (get-current-pop year result)
     (get-current-pop (- year 1) result)))

(defn within-pop-range? [pop diff]
  (and (<= (first pop) diff)
       (>= (second pop) diff)))

(defn target-pop-exceeded? [pop target-pop]
  (> pop (apply max target-pop)))

(defn target-results [m base-config target & step]
  "Takes a map of keys partially matching a transition, a baseline config to use as a template,
   a map containing a target population range and year, e.g. {:year 2019 :population 6} and an
   optional range step value"
  (let [step (if step
               step
               0.1)
        state (create-transition-modifier-seq m 1 2 1)
        baseline-pop (->> (-> base-config
                              (assoc-in (first (first state))
                                        (first (second (first state))))
                              state-pop
                              get-target-pop)
                          (filter #(= (:year %) (:year target)))
                          first
                          :population)
        target-pop (:population target)
        target-year (:year target)
        target-pop-range (vector (- target-pop 1) (+ target-pop 1))
        initial-modifier (math/round (/ target-pop baseline-pop))]
    (loop [configs (map update-results-path (-> (create-transition-modifier-seq m
                                                                                (- initial-modifier 1)
                                                                                (+ initial-modifier 1) step)
                                                (mc/generate-configs base-config)))]
      (let [[config & rest-configs] configs
            result (do (main/run-recorded-send config)
                       (get-target-pop (state-pop config)))
            current-pop (get-current-pop (:year target) result)
            modifier (-> config
                         (get-in [:transition-parameters :transitions-to-change])
                         first
                         :modify-transition-by)
            achieved-pop (->> result
                              (filter #(= (:year %) target-year))
                              first
                              :population)
            diff (pop-diff-by-year target-year result)]
        (do (println "Modifier:" modifier)
            (println "Population:" achieved-pop)
            (if (target-pop-exceeded? current-pop target-pop-range)
              (println "Population exceeds target population")
              (when-not (within-pop-range? target-pop-range diff)
                (recur rest-configs))))))))
