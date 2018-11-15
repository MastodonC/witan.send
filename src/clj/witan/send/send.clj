(ns witan.send.send
  (:require [witan.send.model.input :as i]
            [witan.send.model.prepare :as p]
            [witan.send.model.run :as r]
            [witan.send.report :refer [reset-send-report]]))

(defn run-send-workflow
  "Run the send model, the function expects a map as seen in
  data/demo/config.edn (typically use `(config \"data/demo\")` to
  generate it)"
  ([config]
   (reset-send-report)
   (-> (i/build-input-datasets (:project-dir config) (:file-inputs config) (:schema-inputs config))
       (p/prepare-send-inputs (:transition-parameters config))
       (r/run-send-model (:projection-parameters config)))))
