(ns witan.send.parquet
  (:require [com.climate.claypoole :as cp]
            [com.climate.claypoole.lazy :as lazy]
            [tablecloth.api :as tc]
            [tech.v3.libs.parquet :as parquet]
            [witan.send :as send]
            [witan.send.states :as states]
            [clojure.java.io :as io]))

(defn unpack-transition-counts [{:keys [transitions simulation]}]
  (map (fn [map-entry]
         (let [key (first map-entry)
               transition-count (second map-entry)
               [need-1 setting-1] (-> key
                                      (nth 2)
                                      (states/split-need-setting))
               [need-2 setting-2] (-> key
                                      (nth 3)
                                      (states/split-need-setting))]
           {:calendar-year (dec (nth key 0))
            :calendar-year-2 (nth key 0)
            :academic-year-1 (dec (nth key 1))
            :academic-year-2 (nth key 1)
            :need-1 need-1
            :setting-1 setting-1
            :need-2 need-2
            :setting-2 setting-2
            :transition-count transition-count
            :simulation simulation}))
       transitions))

(defn add-simulation-idx [simulation-idx sd]
  (assoc sd :simulation simulation-idx))

(def ->dataset-seq-xf
  (comp
   (map (fn [[simulation-idx simulation-data]]
          (map (partial add-simulation-idx simulation-idx) simulation-data)))
   (map (fn [simulation]
          (as-> simulation $
            (filter :transitions $)
            (mapcat unpack-transition-counts $)
            (tc/dataset $)
            (tc/map-columns $ :need-1 [:need-1] name)
            (tc/map-columns $ :need-2 [:need-2] name)
            (tc/map-columns $ :setting-1 [:setting-1] name)
            (tc/map-columns $ :setting-2 [:setting-2] name)
            (tc/convert-types $ {:academic-year-1  :int16
                                 :academic-year-2  :int16
                                 :calendar-year    :int16
                                 :calendar-year-2  :int16
                                 :simulation       :int16
                                 :transition-count :int32}))))))

(defn output-ds-seq
  [out-dir prefix ds-seq]
  (when (false? (.exists (io/file out-dir)))
    (.mkdir (java.io.File. out-dir)))
  (let [start          (System/currentTimeMillis)
        idx            (-> ds-seq first :simulation first)
        num-ds         (count ds-seq)
        _              (println (format "Processing Simulation %d" idx))
        base-file-name (format "%s-%05d.parquet" prefix idx)
        _              (println (format "File: %s" base-file-name))
        file-name      (str out-dir base-file-name)]
    {:file-name          file-name
     :number-of-datasets num-ds
     :status             (parquet/ds-seq->parquet file-name ds-seq)
     :elapsed-time       (- (System/currentTimeMillis) start)}))


(defn create-projections-and-save [{:keys [prefix out-dir config] :as projection-config}
                                   sim-range]
  (->> (into []
             (comp
              (send/create-projections-xf projection-config
                                          (:projection-parameters config))
              ->dataset-seq-xf)
             sim-range)
       (output-ds-seq out-dir prefix)))

(defn parallel-create-projections-and-save
  "Takes a map of
   - :config the read in config file (required)
   - :out-dir as a location for the parquet files to be written to (required)
   - :prefix as a prefix for the parquet files (require)
   - :standard-projection as the baseline model (required)
   - :seed-year 1 greater than the max year of the input transitions as the jumping off point (required)
   - :scenario-projection as the scenario model (optional)
   - :modify-transitions-data-range years to apply the scenario-projection (optional)

  It then runs the projection in groups of 50 simulations that get
  written to the same parquet file, so 1000 simulations would create
  20 parquet files.

  The projection is run in the order that things are completed to best
  saturate the cores of whatever machine is running the simulation.
  "
  [{:keys [config] :as projection-config}]
  (cp/with-shutdown! [cpu-pool (cp/threadpool (- (cp/ncpus) 2))]
    (->> (range (get-in config [:projection-parameters :simulations] 1)) ;; Just 1 simulation if nothing is specified
         (partition-all 50)
         (lazy/upmap cpu-pool (partial create-projections-and-save projection-config))
         vec)))
