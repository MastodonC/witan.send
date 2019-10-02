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
        state-pop (str (System/getProperty "user.home")
                       "/code/witan.send.afc/"
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

(defn target-results [m start end step base-config target]
  "Takes a map of keys partially matching a transition, a start, end and step range to modify
   the transition by, a template config and a map containing a target population and year"
  (let [configs (mc/generate-configs (create-transition-modifier-seq m start end step) base-config)]
    (map (fn [config] (do (-> config main/run-recorded-send)
                          (let [state (-> (get-in config [:transition-parameters :transitions-to-change])
                                          first
                                          (clojure.set/rename-keys {:setting-2 :setting :need-2 :need :academic-year-2 :ay})
                                          (select-keys [:setting :need]))
                                out-dir (:output-dir (:output-parameters config))
                                state-pop (str (System/getProperty "user.home")
                                               "/code/witan.send.afc/"
                                               out-dir
                                               "/Output_State_pop_only.csv")
                                results (filter #(and (= (:setting state) (:setting %))
                                                      (= (:need state) (:need %)))
                                                (csv->state-pop state-pop))]
                            (map #(merge (assoc {} :year (key %))
                                         (apply merge-with + (map (fn [m] (select-keys m [:population])) (val %))))
                                 (group-by :calendar-year results))))) configs)))
