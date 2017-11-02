(ns witan.send.validate
  (:require [witan.send.test-utils :as tu]
            [clojure.core.matrix.dataset :as ds]
            [witan.send.params :as p]
            [witan.send.schemas :as sc]
            [witan.send.acceptance.workspace-test :as w]
            [witan.send.utils :as u]
            [clojure.string :as str]
            [incanter.core :as i])
  (:import [org.apache.commons.math3.distribution BetaDistribution]))

(defn associate-params-with-subsequent-year
  [coll]
  (mapv #(assoc %1 :params %2) (drop 1 coll) (map :params coll)))

(defn vals-to-optimise
  [joiners population]
  (let [population-academic-years (-> population vals first keys)
        joiner-calendar-years (keys joiners)]
    (mapcat
     (fn [ay]
       (-> (map
            (fn [cy]
              (let [j (get-in joiners [cy ay] 0)
                    p (get-in population [cy ay])]
                #_{:calendar-year cy :joiners j :population p}
                {:calendar-year cy :academic-year ay :joiners j :population p :alpha j :beta (- p j)}))
            joiner-calendar-years)
           #_(associate-params-with-subsequent-year)))
     population-academic-years)))

(defn joiner-rate
  []
  (let [joiners (-> (tu/csv-to-dataset "data/demo/transitions.csv" sc/TransitionCounts)
                    ds/row-maps
                    p/calculate-joiners-per-calendar-year)
        population (-> (tu/csv-to-dataset "data/demo/population.csv" sc/PopulationDataset)
                       ds/row-maps
                       p/calculate-population-per-calendar-year)
        calendar-year 2013]
    (reduce (fn [coll academic-year]
              (let [j (get-in joiners [calendar-year academic-year] 0)]
                (assoc-in coll [academic-year calendar-year]
                          {:alpha j
                           :beta (- (get-in population [calendar-year academic-year])
                                    j)})))
            {}
            sc/academic-years)))


(defn leaver-rate
  []
  (let [transitions (-> (tu/csv-to-dataset "data/demo/transitions.csv" sc/TransitionCounts)
                        ds/row-maps)
        filtered (remove (fn [{:keys [setting-1]}] (= setting-1 sc/non-send)) transitions)
        ]
    (reduce (fn [coll {:keys [calendar-year academic-year-1 setting-1 setting-2]}]
              (let [leaver? (= setting-2 sc/non-send)]
                (-> coll
                    (update-in [academic-year-1 calendar-year :alpha] u/some+ (if leaver? 1 0))
                    (update-in [academic-year-1 calendar-year :beta] u/some+ (if leaver? 0 1)))))
            {}
            filtered)))

(defn mover-rate
  []
  (let [transitions (-> (tu/csv-to-dataset "data/demo/transitions.csv" sc/TransitionCounts)
                        ds/row-maps)
        filtered (remove (fn [{:keys [setting-1 setting-2]}]
                           (or (= setting-1 sc/non-send)
                               (= setting-2 sc/non-send))) transitions)]
    (reduce (fn [coll {:keys [calendar-year academic-year-1 setting-1 setting-2]}]
              (let [mover? (not= setting-1 setting-2)]
                (-> coll
                    (update-in [academic-year-1 calendar-year :alpha] u/some+ (if mover? 1 0))
                    (update-in [academic-year-1 calendar-year :beta] u/some+ (if mover? 0 1)))))
            {}
            filtered)))


(defn print-results
  [results]
  (let [calendar-year 2013
        academic-years (keys results)]
    (->> (for [academic-year (sort academic-years)]
           #_(->> (concat
                   [academic-year]
                   (for [col cols]
                     (do (println (get-in row [col :alpha]))
                         (try
                           (double
                            (/ (get-in row [col :alpha])
                               (+ (get-in row [col :alpha] 0)
                                  (get-in row [col :beta] 0))))
                           (catch Exception e 0)))))
                  (str/join ","))
           (str/join "\t" [academic-year
                           (get-in results [academic-year calendar-year :alpha] 0)
                           (get-in results [academic-year calendar-year :beta])]))
         (str/join "\n"))))
