(ns witan.send.config
  (:require [aero.core :as aero]
            [clojure.java.io :as io]))

(defn read-config
  "Read a config file and merge it with schema inputs"
  [config-path]
  (let [project-dir (or (.getParent (io/as-file config-path))
                        (System/getProperty "user.dir"))]
    (merge-with merge
                (aero/read-config config-path)
                ;; default-schemas
                {:project-dir project-dir}
                {:output-parameters {:project-dir project-dir}}
                {:file-inputs {:config config-path}})))
