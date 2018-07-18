(ns witan.send.main
  (:require [schema.core :as s]
            [witan.send.send :as send]
            [witan.send.schemas :as sc]
            [witan.send.report :refer [generate-report-header]]
            [aero.core :refer [read-config]])
  (:gen-class))

(defn config [project-dir]
  "Read a config file and merge it with schema inputs"
  (merge (read-config (str project-dir "/config.edn"))
         {:schema-inputs {:settings-to-change sc/SettingsToChange
                          :transition-matrix sc/TransitionCounts
                          :population sc/PopulationDataset
                          :setting-cost sc/NeedSettingCost
                          :valid-setting-academic-years sc/ValidSettingAcademicYears}}
         {:project-dir project-dir}
         {:output-parameters {:project-dir project-dir}}))

(defn run-send
  "Run the send model, the function expects a map as seen in
  data/demo/config.edn (typically use `(config \"data/demo\")` to
  generate it)"
  [config]
  (generate-report-header config)  
  (-> (send/build-input-datasets (:project-dir config) (:file-inputs config) (:schema-inputs config))
      (send/prepare-send-inputs (:transition-parameters config))
      (send/run-send-model (:run-parameters config))
      (send/output-send-results (:output-parameters config))))

(defn -main
  "Run the send model, defaulting to the inbuilt demo data if no project passed in."
  ([] (-main "data/demo/"))
  ([project-dir]
   (run-send (config project-dir))))
