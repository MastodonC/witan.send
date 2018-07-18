(ns witan.send.main
  (:require [schema.core :as s]
            [witan.send.send :as send]
            [witan.send.schemas :as sc]
            [witan.send.report :refer [generate-report-header]]
            [aero.core :refer [read-config]])
  (:gen-class))

(defn config [config-file]
  "Read a config file and merge it with schema inputs"
  (merge (read-config config-file)
         {:schema-inputs {:settings-to-change sc/SettingsToChange
                          :transition-matrix sc/TransitionCounts
                          :population sc/PopulationDataset
                          :setting-cost sc/NeedSettingCost
                          :valid-setting-academic-years sc/ValidSettingAcademicYears}}))

(defn run-send
  "Run the send model, the function expects a map as seen in
  data/demo/config.edn (typically use `(config \"config.edn\")` to
  generate it)"
  [config]
  (generate-report-header config)  
  (-> (send/build-input-datasets (:file-inputs config) (:schema-inputs config))
      (send/prepare-send-inputs (:transition-parameters config))
      (send/run-send-model (:run-parameters config))
      (send/output-send-results (:output-parameters config))))

(defn -main
  "Run the send model, defaulting to the demo if no config file passed in."
  ([] (-main "data/demo/config.edn"))
  ([config-file]
   (run-send (config config-file))))
