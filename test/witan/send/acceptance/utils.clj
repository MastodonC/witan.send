(ns witan.send.acceptance.utils
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as data-csv]
            [me.raynes.fs :as fs]
            [witan.send.main :as m]
            [witan.send.model.output :as so]
            [witan.send.send :as send]
            [witan.send.validate-model :as vm]
            digest))

;; (defn get-md5s [config files]
;;   (if-let [output-dir (m/get-output-dir config)]
;;     (into {}
;;           (map (fn [f]
;;                  [f (-> (io/file output-dir f) (digest/md5))]))
;;           files)))

;; (defn- init-run-for-test [f]
;;   (fn [config]
;;     (when-let [output-dir (m/get-output-dir config)]
;;       (when (fs/directory? output-dir)
;;         (fs/delete-dir output-dir))
;;       (f config))))

;; (def run-model-for-test
;;   (init-run-for-test (fn [config] (-> (send/run-send-workflow config)
;;                                       (so/output-send-results (:output-parameters config))))))

;; (def run-validation-for-test
;;   (init-run-for-test (fn [config]
;;                        (-> (send/run-send-workflow config)
;;                            (so/output-send-results (:output-parameters config)))
;;                        (vm/run-send-validation config))))

;; (defn load-results [config file]
;;   (when-let [output-dir (m/get-output-dir config)]
;;     (with-open [reader (io/reader (str output-dir "/" file))]
;;       (let [[header & data] (data-csv/read-csv reader)
;;             header (map keyword header)]
;;         (into []
;;               (map #(zipmap header %))
;;               data)))))
