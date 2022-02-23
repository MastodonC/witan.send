(ns witan.send.send
  (:require [witan.send.model.prepare :as p]
            [witan.send.model.run :as r]
            [witan.send.model.input.costs :as wic]
            [witan.send.model.input.population :as wip]
            [witan.send.model.input.settings-to-change :as wistc]
            [witan.send.model.input.transitions :as wit]
            [witan.send.model.input.valid-states :as wivs]
            [witan.send.report :refer [reset-send-report]]))

(defn build-input-datasets
  "Build a map of the datasets to use for input"
  [project-dir {:keys [costs population settings-to-change transitions valid-states]} schema-inputs]
  (let [input-datasets {:costs (wic/csv->costs (str project-dir "/" costs))
                        :population (wip/csv->population (str project-dir "/" population))
                        :transitions (wit/csv->transitions (str project-dir "/" transitions))
                        :valid-states (wivs/csv->valid-states (str project-dir "/" valid-states))}]
    (if settings-to-change
      (assoc input-datasets :settings-to-change (wistc/csv->settings-to-change (str project-dir "/" settings-to-change)))
      input-datasets)))

(defn run-send-workflow
  "Run the send model, the function expects a map as seen in
  data/demo/config.edn (typically use `(config \"data/demo\")` to
  generate it)"
  ([config print-warnings?]
   (reset-send-report)
   (let [input (build-input-datasets (:project-dir config) (:file-inputs config) (:schema-inputs config))
         ;; validate-input (i/check-if-dataset-is-valid input)
         ]
     (-> (p/prepare-send-inputs input (:transition-parameters config) print-warnings?)
         (r/run-send-model (:projection-parameters config)))
     ;; FIXME: Put in appropriate validation
     #_(if (= true validate-input)
         (-> (p/prepare-send-inputs input (:transition-parameters config))
             (r/run-send-model (:projection-parameters config)))
         validate-input)))
  ([config]
   (run-send-workflow config true)))

(defn input-analysis
  ([config]
   (reset-send-report)
   (let [print-warnings? true]
     (-> (build-input-datasets (:project-dir config) (:file-inputs config) (:schema-inputs config))
         (p/prepare-send-inputs (:transition-parameters config) print-warnings?)))))
