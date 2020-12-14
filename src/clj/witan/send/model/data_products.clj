(ns witan.send.model.data-products
  (:require [witan.send.constants :as constants]
            [witan.send.maths :as m]
            [witan.send.schemas :as sc]
            [witan.send.states :as states]
            [witan.send.utils :as u]
            [witan.send.model.data-products.setting-summary :as setting-summary])
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

(defn summarise-results [product-name rollup-fn projections]
  (println "Producing " product-name)
  (let [product (transduce
                 (comp
                  (rolled-up-simulations-with-n-to-simulations-with-histograms rollup-fn))
                 histogram-summary-rf
                 projections)]
    (println product-name "... done!")
    product))

(defn data-products [valid-states cost-lookup simulations]
  (println "Creating data products.")
  (let [total-in-send-by-ay (future (summarise-results
                                     "Total in SEND by AY"
                                     roll-up-calendar-year-by-ay simulations))
        total-in-send-by-ay-group (future (summarise-results
                                           "Total in SEND by AY Group"
                                           roll-up-total-in-send-by-ay-group simulations))
        total-send-population (future (into []
                                            (map :total-population)
                                            (summarise-results
                                             "Total SEND Population"
                                             roll-up-calendar-year-by-total-send-population simulations)))
        total-cost (future (into []
                                 (map :total-cost)
                                 (summarise-results
                                  "Total Cost"
                                  (partial roll-up-calendar-year-by-total-cost cost-lookup)
                                  simulations)))
        total-in-send-by-need (future (summarise-results
                                       "Total in SEND by Need"
                                       roll-up-calendar-year-by-total-in-send-by-need simulations))
        population-by-setting (future (summarise-results
                                       "Total in SEND by Setting"
                                       setting-summary/population simulations))
        cost-by-setting (future (into []
                                      (summarise-results
                                       "SEND cost by Setting"
                                       (partial setting-summary/cost cost-lookup) simulations)))
        dp {:total-in-send-by-ay @total-in-send-by-ay
            :total-in-send-by-ay-group @total-in-send-by-ay-group
            :total-in-send @total-send-population
            :total-cost @total-cost
            :total-in-send-by-need @total-in-send-by-need
            :total-in-send-by-setting @population-by-setting
            :setting-cost @cost-by-setting}]
    (println "Data products created.")
    dp))

(defn ->send-output-style [data-products]
  (let [data-keys (keys data-products)]
    (into []
          (map #(zipmap data-keys %))
          (apply map vector ((apply juxt data-keys) data-products)))))
