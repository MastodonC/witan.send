(ns witan.send
  (:require [aero.core :as aero]
            [clojure.java.io :as io]
            [clojure.core.async :as async]
            [tablecloth.api :as tc]
            [witan.send.distributions :as d]
            [witan.send.model.prepare :as p]
            [witan.send.model.input.population :as wip]
            [witan.send.model.input.settings-to-change :as wistc]
            [witan.send.model.input.transitions :as wit]
            [witan.send.model.input.valid-states :as wivs]
            [witan.send.model.run :as r]
            [witan.send.states :as states]))

;; FIXME: this should go in its own config namespace
(defn read-config
  "Read a config file and merge it with schema inputs"
  [config-path]
  (let [project-dir (or (.getParent (io/as-file config-path))
                        (System/getProperty "user.dir"))]
    (merge-with merge
                (aero/read-config config-path)
                ;; default-schemas
                {:project-dir project-dir}
                {:output-parameters {:project-dir project-dir}})))

(defn build-input-datasets
  "Build a map of the datasets to use for input"
  [project-dir {:keys [_costs population settings-to-change transitions valid-states]}]
  (let [input-datasets {;; :costs (wic/csv->costs (str project-dir "/" costs))
                        :population (wip/csv->population (str project-dir "/" population))
                        :transitions (wit/csv->transitions (str project-dir "/" transitions))
                        :valid-states (wivs/csv->valid-states (str project-dir "/" valid-states))}]
    (if settings-to-change
      (assoc input-datasets :settings-to-change (wistc/csv->settings-to-change (str project-dir "/" settings-to-change))) ;; do we need this?
      input-datasets)))

(defn train-model [{:keys [input-datasets config print-warnings?]
                    :as state}]
  (p/prepare-send-inputs input-datasets (:transition-parameters config) print-warnings?))

#_(defn run-model [{:keys [model config]
                    :as state}]
    (r/run-send-model model (:projection-parameters config)))

(defn create-projections [simulations modify-transitions-date-range make-setting-invalid inputs modified-inputs population-by-state projected-population seed-year]
  (map (fn [simulation-run]
         [simulation-run (reductions (partial r/run-model-iteration
                                              modify-transitions-date-range
                                              make-setting-invalid
                                              inputs
                                              modified-inputs)
                                     {:model population-by-state}
                                     (r/projected-future-pop-by-year projected-population seed-year))])
       (range simulations)))

(def ->dataset-seq-xf
  (comp
   (map (fn [[simulation-idx simulation-data]]
          (map (fn [sd]
                 (assoc sd :simulation simulation-idx))
               simulation-data)))
   (map (fn [simulation]
          (->> simulation
               (filter :transitions)
               (mapcat (fn [{:keys [transitions simulation]}]
                         (map (fn [map-entry]
                                (let [key (first map-entry)
                                      transition-count (second map-entry)
                                      [need-1 setting-1] (-> key
                                                             (nth 2)
                                                             (states/split-need-setting))
                                      [need-2 setting-2] (-> key
                                                             (nth 3)
                                                             (states/split-need-setting))]
                                  {:calendar-year (nth key 0)
                                   :academic-year (nth key 1)
                                   :need-1 need-1
                                   :setting-1 setting-1
                                   :need-2 need-2
                                   :setting-2 setting-2
                                   :transition-count transition-count
                                   :simulation simulation}))
                              transitions)))
               (tc/dataset))))))

(defn ->dataset-seq [simulations]
  (sequence
   ->dataset-seq-xf
   simulations))


(defn run-model
  "Outputs the population for the last year of historic data, with one
   row for each individual/year/simulation. Also includes age & state columns"
  [{:keys [standard-projection scenario-projection modify-transition-by
           modify-transitions-date-range seed-year make-setting-invalid]
    :as _trained-model}
   {:keys [random-seed simulations]
    :as _projection-parameters}]
  (d/set-seed! random-seed)
  (println "Preparing" simulations "simulations...")
  (let [{:keys [population population-by-state
                projected-population cost-lookup
                valid-states transitions] :as inputs} standard-projection
        modified-inputs (when ((complement nil?) scenario-projection)
                          (assoc scenario-projection :valid-year-settings
                                 (states/calculate-valid-year-settings-from-setting-academic-years valid-states)))
        inputs (assoc inputs :valid-year-settings (states/calculate-valid-year-settings-from-setting-academic-years valid-states))
        projection (create-projections simulations
                                       modify-transitions-date-range
                                       make-setting-invalid
                                       inputs
                                       modified-inputs
                                       population-by-state
                                       projected-population
                                       seed-year)]
    (->dataset-seq projection)))


(comment

  ;; EXAMPLE: This is what a baseline run would look like
  (let [config (read-config "data/demo/config.edn")
        input-datasets (build-input-datasets (:project-dir config) (:file-inputs config))
        model (train-model {:input-datasets input-datasets
                            :config config
                            :print-warnings? true})
        model-results (run-model model (:projection-parameters config))]
    ;; mush the seq of datasets together (tho probably better to run
    ;; it through a ds/reducer of some sort
    (apply tc/concat-copying model-results))

  ;; Or do each step so you can tweak the results of one
  (def config (read-config "data/demo/config.edn"))
  (def input-datasets (build-input-datasets (:project-dir config) (:file-inputs config)))
  (def model (train-model {:input-datasets input-datasets
                           :config config
                           :print-warnings? true}))
  (def model-results (run-model model (:projection-parameters config)))
  (->dataset-seq model-results)

  )


(comment

  ;; FIXME: I don't like these versions as much

  ;; FIXME: I prefer ->dataset-seq-xf
  (def ->dataset-xf
    (comp
     (mapcat (fn [[simulation-idx simulation-data]]
               (map (fn [sd]
                      (assoc sd :simulation simulation-idx))
                    simulation-data)))
     (filter :transitions)
     (mapcat (fn [{:keys [transitions simulation]}]
               (map (fn [map-entry]
                      (let [key (first map-entry)
                            transition-count (second map-entry)
                            [need-1 setting-1] (-> key
                                                   (nth 2)
                                                   (states/split-need-setting))
                            [need-2 setting-2] (-> key
                                                   (nth 3)
                                                   (states/split-need-setting))]
                        {:calendar-year (nth key 0)
                         :academic-year (nth key 1)
                         :need-1 need-1
                         :setting-1 setting-1
                         :need-2 need-2
                         :setting-2 setting-2
                         :transition-count transition-count
                         :simulation simulation}))
                    transitions)))))

  ;; FIXME: I prefer ->dataset-seq
  (defn ->dataset [simulations]
    (-> (let [to (async/chan 1024)]
          (async/pipeline
           (* 3 (quot (.availableProcessors (Runtime/getRuntime)) 4))
           to
           ->dataset-xf
           (async/to-chan!! simulations))
          (async/<!! (async/into [] to)))
        (tc/dataset)
        (tc/convert-types {:academic-year :int16
                           :calendar-year :int16
                           :simulation :int16
                           :transition-count :int32})))

  )
