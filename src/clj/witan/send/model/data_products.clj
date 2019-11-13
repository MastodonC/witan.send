(ns witan.send.model.data-products
  (:require [witan.send.constants :as constants]
            [witan.send.maths :as m]
            [witan.send.schemas :as sc]
            [witan.send.states :as states]
            [witan.send.utils :as u])
  (:import org.HdrHistogram.IntCountsHistogram))

(def number-of-significant-digits 3)

(defn roll-up-calendar-year-by-need-setting
  "This rolls up all need-settings, even invalid ones."
  [calendar-year]
  (transduce
   (map (fn [[[ay need-setting] population]] [need-setting population]))
   (fn
     ([totals] totals)
     ([totals [need-setting population]]
      (update totals need-setting m/some+ population)))
   {}
   calendar-year))

(defn roll-up-calendar-year-by-state
  "This only rolls up valid need-setings (aka states)."
  [all-valid-states calendar-year]
  (reduce
   (fn [acc state]
     (assoc acc state (get calendar-year state 0)))
   {}
   all-valid-states))

(defn roll-up-calendar-year-by-ay [calendar-year]
  (reduce
   (fn [acc [[ay need-setting] pop]]
     (update acc ay + pop)) ;; can be + b/c we shouldn't have pops to add outside of the known academic years
   (into (sorted-map) (map vector sc/academic-years (repeat 0)))
   calendar-year))

(defn roll-up-calendar-year-by-total-send-population [calendar-year]
  (transduce
   (remove (fn [[[ay need-setting] population]]
             (= need-setting constants/non-send)))
   (fn
     ([total] total)
     ([total [[ay need-setting] population]] (update total :total-population + population)))
   {:total-population 0}
   calendar-year))

(defn roll-up-calendar-year-by-total-in-send-by-need [calendar-year]
  (transduce
   (map (fn [[[ay need-setting] population]]
          [(-> need-setting
               (states/split-need-setting)
               first)
           population]))
   (fn
     ([totals] totals)
     ([totals [need population]]
      (update totals need m/some+ population)))
   {}
   calendar-year))

(defn roll-up-calendar-year-by-total-in-send-by-setting [calendar-year]
  (transduce
   (map (fn [[[ay need-setting] population]]
          [(-> need-setting
               (states/split-need-setting)
               second)
           population]))
   (fn
     ([totals] totals)
     ([totals [setting population]]
      (update totals setting m/some+ population)))
   {}
   calendar-year))

(defn roll-up-calendar-year-by-cost-per-setting [cost-lookup calendar-year]
  (transduce
   (comp
    (remove (fn [[[ay need-setting] population]]
              (= need-setting constants/non-send))) ;; remove non-send
    ;; convert need-setting population to need-setting cost
    (map (fn [[[ay need-setting] population]] [need-setting population]))
    (map (fn [[need-setting population]] [(states/split-need-setting need-setting) population]))
    (map (fn [[[need setting :as need-setting] population]] [setting
                                                             (* (get cost-lookup need-setting 0)
                                                                population)]))
    (map (fn [[setting cost]] [setting cost])))
   (fn
     ([totals] totals)
     ([totals [setting cost]] (update totals setting (fnil + 0) cost)))
   {}
   calendar-year))

(defn roll-up-calendar-year-by-total-cost [cost-lookup calendar-year]
  (transduce
   (comp
    (remove (fn [[[ay need-setting] population]]
              (= need-setting constants/non-send))) ;; remove non-send
    ;; convert need-setting population to need-setting cost
    (map (fn [[[ay need-setting] population]] [need-setting population]))
    (map (fn [[need-setting population]] [(states/split-need-setting need-setting) population]))
    (map (fn [[need-setting population]] [need-setting
                                          (* (get cost-lookup need-setting 0)
                                             population)]))
    (map (fn [[need-setting cost]] cost)))
   (fn
     ([total] total)
     ([total cost] (update total :total-cost + cost)))
   {:total-cost 0.0}
   calendar-year))

(defn roll-up-total-in-send-by-ay-group [calendar-year]
  (transduce
   (comp
    (map (fn [[[ay need-setting] population]] [ay population]))
    (map (fn [[ay population]] [(u/ay-groups ay) population])))
   (fn
     ([totals] totals)
     ([totals [ay-group population]]
      (update totals ay-group m/some+ population)))
   {}
   calendar-year))

(defn histogram-summary-rf
  ([] (transient {:simulation-years (repeat {})
                  :number-of-simulations 0}))
  ([{:keys [simulation-years number-of-simulations] :as acc} new-simulation-years]
   (assoc! acc
           :simulation-years
           (into []
                 (map
                  (fn [[^IntCountsHistogram histogram ^IntCountsHistogram new-histogram]]
                    (merge-with
                     (fn [^IntCountsHistogram histogram ^IntCountsHistogram new-histogram]
                       (if histogram
                         (doto histogram (.add new-histogram))
                         new-histogram))
                     histogram
                     new-histogram)))
                 (sequence (map (fn [x1 x2] [x1 x2]))
                           simulation-years
                           new-simulation-years))
           :number-of-simulations (inc number-of-simulations)))
  ([{:keys [simulation-years ^int number-of-simulations]}]
   (into []
         (map
          (fn [year]
            (into {}
                  (map (fn [[k ^IntCountsHistogram histogram]]
                         [k (let [result {:median (dec (.getValueAtPercentile histogram 50.0))
                                          :mean (dec (.getMean histogram))
                                          :std-dev (.getStdDeviation histogram)
                                          :iqr (- (.getValueAtPercentile histogram 75.0) (.getValueAtPercentile histogram 25.0))
                                          :min (dec (.getValueAtPercentile histogram 0.0))
                                          :max (dec (.getValueAtPercentile histogram 100.0))
                                          :q1 (dec (.getValueAtPercentile histogram 25.0))
                                          :q3 (dec (.getValueAtPercentile histogram 75.0))
                                          :low-95pc-bound (dec (.getValueAtPercentile histogram 2.5))
                                          :high-95pc-bound (dec (.getValueAtPercentile histogram 97.5))}]
                              (u/confidence-intervals result number-of-simulations))]))
                  year)))
         simulation-years)))

(defn rolled-up-simulations-with-n-to-simulations-with-histograms [rollup-fn]
  (map (fn [simulation]
         (into []
               (comp
                (map :model)
                (map rollup-fn)
                (map (fn [year]
                       (into {}
                             (map (fn [[k n]]
                                    [k (let [hist (IntCountsHistogram. number-of-significant-digits)]
                                         (doto hist (.recordValue (inc n))))]))
                             year))))
               simulation))))

(defn summarise-results [rollup-fn projections]
  (transduce
   (comp
    (rolled-up-simulations-with-n-to-simulations-with-histograms rollup-fn))
   histogram-summary-rf
   projections))

(defn data-products [valid-states cost-lookup simulations]
  (let [by-state (future (summarise-results
                          (partial roll-up-calendar-year-by-state
                                   (states/calculate-valid-states-from-setting-academic-years valid-states))
                          simulations))
        total-in-send-by-ay (future (summarise-results roll-up-calendar-year-by-ay simulations))
        total-in-send (future (into []
                                    (map :total-population)
                                    (summarise-results roll-up-calendar-year-by-total-send-population simulations)))
        total-in-send-by-need (future (summarise-results roll-up-calendar-year-by-total-in-send-by-need simulations))
        total-in-send-by-setting (future (summarise-results roll-up-calendar-year-by-total-in-send-by-setting simulations))
        total-cost (future (into []
                                 (map :total-cost)
                                 (summarise-results (partial roll-up-calendar-year-by-total-cost cost-lookup)
                                                    simulations)))
        setting-cost (future (into []
                                   (summarise-results (partial roll-up-calendar-year-by-cost-per-setting cost-lookup)
                                                      simulations)))
        total-in-send-by-ay-group (future (summarise-results roll-up-total-in-send-by-ay-group simulations))]
    {:by-state @by-state
     :total-in-send-by-ay @total-in-send-by-ay
     :total-in-send @total-in-send
     :total-in-send-by-need @total-in-send-by-need
     :total-in-send-by-setting @total-in-send-by-setting
     :total-cost @total-cost
     :setting-cost @setting-cost
     :total-in-send-by-ay-group @total-in-send-by-ay-group}))

(defn ->send-output-style [data-products]
  (let [data-keys (keys data-products)]
    (into []
          (map #(zipmap data-keys %))
          (apply map vector ((apply juxt data-keys) data-products)))))
