(ns witan.send.acceptance.utils
  (:require [me.raynes.fs :as fs]
            [witan.send.main :as m]
            [witan.send.model.output :as so]
            [witan.send.send :as send]
            [clojure.java.io :as io]
            [witan.send.validate-model :as vm]
            [witan.send.model.input :as i]
            [clojure.core.matrix.dataset :as ds]
            digest))

(defn get-md5s [config files]
  (if-let [output-dir (m/get-output-dir config)]
    (into {} (for [f files]
               [f (-> (io/file output-dir f) (digest/md5))]))))

(defn- init-run-for-test [f]
  (fn [config]
    (when-let [output-dir (m/get-output-dir config)]
      (when (fs/directory? output-dir)
        (fs/delete-dir output-dir))
      (f config))))

(def run-model-for-test
  (init-run-for-test (fn [config] (-> (send/run-send-workflow config)
                                      (so/output-send-results (:output-parameters config))))))

(def run-validation-for-test
  (init-run-for-test (fn [config]
                       (-> (send/run-send-workflow config)
                           (so/output-send-results (:output-parameters config)))
                       (vm/run-send-validation config))))

(defn load-results [config file]
  (when-let [output-dir (m/get-output-dir config)]
    (let [csv (i/load-csv (str output-dir "/" file))]
      (ds/row-maps (ds/dataset (:column-names csv) (:columns csv))))))
