(ns witan.send.main
  (:gen-class)
  (:require [aero.core :as aero]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [witan.send.metadata :as md]
            [witan.send.model.output :as so]
            [witan.send.schemas :as sc]
            [witan.send.send :as send]
            [witan.send.validate-model :as vm]))

(defn config [config-path]
  "Read a config file and merge it with schema inputs"
  (let [project-dir (.getParent(java.io.File. config-path))]
    (merge-with merge
              (read-config config-path)
              {:schema-inputs {:settings-to-change sc/SettingsToChange
                               :transition-matrix sc/TransitionCounts
                               :population sc/PopulationDataset
                               :setting-cost sc/NeedSettingCost
                               :valid-setting-academic-years sc/ValidSettingAcademicYears}}
              {:project-dir project-dir}
              {:output-parameters {:project-dir project-dir}})))

(defn get-output-dir [config]
  (string/join "/" [(:project-dir config)
                    (get-in config [:output-parameters :output-dir])]))

(defn save-runtime-config
  [config]
  (spit (string/join "/" [(get-output-dir config) "runtime-config.edn"])
        (with-out-str
          (pprint (-> config
                      (dissoc :schema-inputs)
                      (dissoc :project-dir))))))

(defn save-runtime-metadata
  [config metadata]
  (spit (string/join "/" [(get-output-dir config) "runtime-metadata.edn"])
        (with-out-str
          (pprint (md/merge-end-time metadata)))))

;; The run-* fns are just handy repl shortcuts from the main ns.
(defn run-send
  ([] (run-send (config "data/demo/config.edn")))
  ([config]
   (send/run-send-workflow config)))

(defn run-validation
  ([] (run-validation (config "data/demo/config.edn")))
  ([config]
   (vm/run-send-validation config)))

(defn -main
  "Run the send model producing outputs, defaulting to the inbuilt demo
  data if no project passed in.  If the config is set to run validation
  do that as well.  Save the config and metadata also re the run also."
  ([] (-main "data/demo/config.edn"))
  ([config-path]
   (let [config (config config-path)
         metadata (md/metadata config)]
     (-> (send/run-send-workflow config)
         (so/output-send-results (:output-parameters config)))
     (when (get-in config [:validation-parameters :run-as-default])
       (vm/run-send-validation config))
     (save-runtime-config config)
     (save-runtime-metadata config metadata))))
