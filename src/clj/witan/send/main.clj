(ns witan.send.main
  (:gen-class)
  (:require [aero.core :as aero]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [witan.send.metadata :as md]
            [witan.send.model.output :as so]
            [witan.send.send :as send]
            [witan.send.validate-model :as vm]
            [clojure.java.io :as io]))

;; FIXME: correct when we have better validation
(def default-schemas
  {:schema-inputs #_{:settings-to-change sc/SettingsToChange
                     :transitions sc/TransitionCounts
                     :population sc/PopulationDataset
                     :costs sc/NeedSettingCost
                     :valid-states sc/ValidSettingAcademicYears}
   {}})

(defn read-config
  "Read a config file and merge it with schema inputs"
  [config-path]
  (let [project-dir (or (.getParent (io/as-file config-path))
                        (System/getProperty "user.dir"))]
    (merge-with merge
                (aero/read-config config-path)
                default-schemas
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
  ([] (run-send (read-config "data/demo/config.edn")))
  ([config]
   (send/run-send-workflow config)))

(defn run-validation
  ([] (run-validation (read-config "data/demo/config.edn")))
  ([config]
   (vm/run-send-validation config)))

(defn run-recorded-send [config]
  (try
    (let [metadata (md/metadata config)]
      (so/output-send-results
       (send/run-send-workflow config)
       (:output-parameters config))
      (when (get-in config [:validation-parameters :run-validation])
        (vm/run-send-validation config))
      (save-runtime-config config)
      (save-runtime-metadata config metadata))
    (catch Exception e
      (throw (ex-info "Couldn't run recorded send." {:config config} e)))))

(defn -main
  "Run the send model producing outputs, defaulting to the inbuilt demo
  data if no project passed in.  If the config is set to run validation
  do that as well.  Save the config and metadata also re the run also."
  ([] (-main "data/demo/config.edn"))
  ([config-path]
   (-> config-path
       read-config
       run-recorded-send)))
