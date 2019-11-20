(ns witan.send.model.data-products.setting-summary
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [medley.core :as medley]
            [witan.send.maths :as m]
            [witan.send.constants :as constants]
            [witan.send.states :as states]))

(def need-setting-and-population
  (comp
   ;; remove non-send
   (remove (fn [[[ay need-setting] population]]
             (= need-setting constants/non-send)))
   ;; [[need setting] population]
   (map (fn [[[ay need-setting] population]]
          [(states/split-need-setting need-setting) population]))))

(defn population [simulation-for-calendar-year]
  (transduce
   (comp
    need-setting-and-population
    (map (fn [[[need setting] population]] [setting population])))
   (fn
     ([totals] totals)
     ([totals [setting population]]
      (update totals setting (fnil + 0) population)))
   {}
   simulation-for-calendar-year))

(defn cost [cost-lookup simulation-for-calendar-year]
  (transduce
   (comp
    need-setting-and-population
    (map (fn [[[need setting :as need-setting] population]]
           [setting
            (* (get cost-lookup need-setting 0)
               population)]))
    (map (fn [[setting cost]]
           [setting cost])))
   (fn
     ([totals] totals)
     ([totals [setting cost]] (update totals setting (fnil + 0) cost)))
   {}
   simulation-for-calendar-year))


(def columns [:calendar-year :setting :mean :std-dev :iqr :min :low-95pc-bound :q1 :median :q3 :high-95pc-bound :max :low-ci :high-ci])

(defn ->output-setting-csv [dir initial-projection-year send-output]
  (with-open [writer (io/writer (io/file (str dir "/Output_Setting.csv")))]
    (csv/write-csv writer
                   (into [(mapv name columns)]
                         (comp
                          (mapcat (fn [[output year]]
                                    (map (fn [[setting stats]]
                                           (-> (medley/map-vals m/round stats)
                                               (assoc :setting (name setting) :calendar-year year)))
                                         (:total-in-send-by-setting output))))
                          (map (apply juxt columns)))
                         (sequence
                          (map (fn [output year] [output year]))
                          send-output (iterate inc initial-projection-year))))))

(defn ->output-setting-cost-csv [dir initial-projection-year send-output]
  (with-open [writer (io/writer (io/file (str dir "/Output_Setting_Cost.csv")))]
    (csv/write-csv writer
                   (into [(mapv name columns)]
                         (comp
                          (mapcat (fn [[output year]]
                                    (map (fn [[setting stats]]
                                           (-> (medley/map-vals m/round stats)
                                               (assoc :setting (name setting) :calendar-year year)))
                                         (:setting-cost output))))
                          (map (apply juxt columns)))
                         (sequence
                          (map (fn [output year] [output year]))
                          send-output (iterate inc initial-projection-year))))))
