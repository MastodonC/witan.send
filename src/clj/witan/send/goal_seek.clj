(ns witan.send.goal-seek
  (:require [witan.send.multi-config :as mc]
            [witan.send.main :as main]
            [witan.send.model.input :as i]
            [witan.send.model.prepare :as p]
            [witan.send.maths :as math]
            [witan.send.send :as s]
            [witan.send.states :as st]
            [witan.send.model.output :as so]
            [witan.send.metadata :as md]))

(defn update-transition-modifier
  "Takes a map of keys partially matching a transition and a new modifier"
  [m n]
  [(assoc m :modify-transition-by n)])

(defn create-transition-modifier-seq
  "Takes a map of keys partially matching a transition and start, end and step range to modify
   the transition by, creating a sequence of configs for :transitions-to-change"
  [m start end step]
  [[[:transition-parameters :transitions-to-change]
    (mapv #(update-transition-modifier m %) (range start end step))]])

(defn csv->state-pop [filepath]
  (i/csv->x (map #(-> % (update :calendar-year i/->int)
                      (update :academic-year i/->int)
                      (update :need keyword)
                      (update :setting keyword)
                      (update :population i/->int)))
            filepath))

(defn match-need-setting [data state]
  (= (second (key data)) (st/join-need-setting (:need state) (:setting state))))

(defn match-ay [data state]
  (let [ay-state (:academic-year state)]
    (if (coll? ay-state)
      (true? (some (fn [ay] (= (first (key data)) ay)) ay-state))
      (= (first (key data)) ay-state))))

(defn get-target-pop
  "Takes a vector with a state to find (e.g. {:setting :SFSS, :need :ASD}) and filepath
  for a Output_State_pop_only.csv file to return summed populations sorted by calender year
  for ease of reading in REPL"
  [config state-pop]
  (let [state (-> (get-in config [:transition-parameters :transitions-to-change])
                  first
                  (clojure.set/rename-keys {:setting-2 :setting :need-2 :need :academic-year-2 :academic-year})
                  (select-keys [:setting :need :academic-year]))]
    (->> state-pop
         (filter #(= (second (key %)) (st/join-need-setting (:need state) (:setting state))))
         (map val)
         (apply merge-with +)
         :median)))

(defn update-results-path [config results-path]
  (assoc-in config [:output-parameters :output-dir] (str results-path (get-in config [:output-parameters :output-dir]))))

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

(defn target-pop-to-high? [pop target-pop]
  (> pop (apply max target-pop)))

(defn target-pop-to-low? [pop target-pop]
  (< pop (apply min target-pop)))

(defn assoc-transition-params
  "Takes a config and creates or updates :transitions-to-change key with a partial
   transition map"
  [config m]
  (let [state (create-transition-modifier-seq m 1 2 1)]
    (-> config
        (assoc-in (first (first state))
                  (first (second (first state)))))))

(defn generate-configs [config m start end step]
  (mc/generate-configs (create-transition-modifier-seq m start end step)
                       config))

;;;;;; From the Clojure Cookbook ;;;;;;;;

(defn mean [coll]
  (let [sum (apply + coll)
        count (count coll)]
    (if (pos? count)
      (/ sum count)
      0)))

(defn median [coll]
  (let [sorted (sort coll)
        cnt (count sorted)
        halfway (quot cnt 2)]
    (if (odd? cnt)
      (nth sorted halfway) ; (1)
      (let [bottom (dec halfway)
            bottom-val (nth sorted bottom)
            top-val (nth sorted halfway)]
        (mean [bottom-val top-val])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn target-result
  "Takes a baseline config to use as a template, a target year and a
   map of keys partially matching a transition, and a value to modify by"
  [config target-year m]
  (let [config (assoc-in config [:transition-parameters :transitions-to-change] (vector m))
        modifier (-> config
                     (get-in [:transition-parameters :transitions-to-change])
                     first
                     :modify-transition-by)
        _ (println "Modifier:" modifier)
        projection (s/run-send-workflow config false)
        base-year (+ 1 (apply max (into (sorted-set) (map :calendar-year (:transitions projection)))))
        output (:send-output projection)
        result (into {} (map #(assoc {} %1 %2) (range base-year (+ base-year (count output))) output))
        target-year-result (get-in result [target-year :by-state])
        target-pop-result (get-target-pop config target-year-result)]
    (println "Population:" target-pop-result)
    [projection target-pop-result]))

(defn target-results
  "Takes a baseline config to use as a template, a map containing a target population
   range and year, e.g. {:year 2019 :population 6}, a map of keys partially matching
   a transition, a boolean to output results and an optional range step value"
  [base-config target m results-path & step]
  (let [step (if step
               step
               1)
        baseline-pop (->> (str (get-in base-config [:output-parameters :output-dir]) "/Output_State_pop_only.csv")
                          csv->state-pop
                          (filter #(and (= (:need %) (:need-2 m))
                                        (= (:calendar-year %) (:year target))
                                        (= (:setting %) (:setting-2 m))))
                          (map #(select-keys % [:population]))
                          (apply merge-with +)
                          :population)
        target-pop (:population target)
        target-year (:year target)
        target-pop-range (if (vector? target-pop)
                           target-pop
                           (vector (- target-pop 1) (+ target-pop 1)))
        target-pop-median (if (vector? target-pop)
                            (median target-pop)
                            target-pop)]
    (println "Modifying:" m)
    (loop [modifier (math/round (/ target-pop-median baseline-pop))
           tested-modifiers [modifier]]
      (let [config (first (generate-configs base-config m modifier (+ modifier 1) step))
            [projection pop-result] (target-result config
                                                   target-year
                                                   (first (get-in config [:transition-parameters :transitions-to-change])))]
        (cond
          (target-pop-to-high? pop-result target-pop-range)
          (let [new-modifier (if (<= (apply min tested-modifiers)
                                     (math/round (* modifier (- (/ pop-result target-pop-median) 1))))
                               (math/round (* modifier (- (/ pop-result target-pop-median) 1)))
                               (math/round (* modifier (- (/ pop-result target-pop-median) 0.5))))]
            (recur new-modifier
                   (into tested-modifiers [new-modifier])))

          (target-pop-to-low? pop-result target-pop-range)
          (let [new-modifier (if (<= (apply max tested-modifiers)
                                     (math/round (* modifier (+ (/ pop-result target-pop-median) 1))))
                               (math/round (* modifier (+ (/ pop-result target-pop-median) 1)))
                               (math/round (* modifier (+ (/ pop-result target-pop-median) 0.5))))]
            (recur new-modifier
                   (into tested-modifiers [new-modifier])))

          (within-pop-range? target-pop-range pop-result)
          (let [config (update-results-path config results-path)]
            (so/output-send-results projection (:output-parameters config))
            (main/save-runtime-config config)
            (main/save-runtime-metadata config (md/metadata config))))))))
