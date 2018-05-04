(ns witan.send.validate-model
  (:require [witan.send.test-utils :as tu]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [witan.send.acceptance.workspace-test :refer [run-model]]))



; TODOs
;; setup function:
;;; write validations dir
;;; write path/temp dir


(defn csv-data->maps [csv-data]
  (map zipmap
       (->> (first csv-data)
            (map keyword)
            repeat)
       (rest csv-data)))

(defn write-csv [transitions path]
  (with-open [writer (io/writer (io/file path))]
    (let [columns (into [] (keys (first transitions)))
          headers (mapv name columns)
          rows (mapv #(mapv % columns) transitions)]
    (csv/write-csv writer (into [headers] rows)))))

(defn load-transitions [path]
  (csv-data->maps
    (with-open [reader (io/reader (str "data/" path "transitions.csv"))]
      (doall
        (csv/read-csv reader)))))

(defn get-validation-years [transitions]
  (->> (map :calendar-year transitions)
       (distinct)
       (map #(Integer/parseInt %))
       (drop-last)))

(defn return-fold [bound year transitions]
  (filter #(bound (Integer/parseInt (:calendar-year %)) year) transitions))

(defn copy-if-exists [source-path destination-path file]
  (let [source (str source-path file)]
    (if (.exists (clojure.java.io/as-file source))
      (io/copy (io/file source) (io/file (str destination-path file))))))

(defn copy-input-files [input-path year]
  (let [source-path (str "data/" input-path)
        destination-path (str source-path "temp/" year "/")]
    (.mkdir (java.io.File. destination-path))
    (doseq [file ["modify-settings.csv" "population.csv" "need-setting-costs.csv" "valid-setting-academic-years.csv"]]
      (copy-if-exists source-path destination-path file))))

(defn move-target-files [year]
  (doseq [file ["Output_Count.csv" "Output_AY_State.csv"]]
    (io/copy (io/file (str "target/" file)) (io/file (str "validation/" year "_modelled_" file)))))


(defn validate-fold [input-path year transitions settings]
  (if (contains? settings :override-inputs-path)
    (throw (IllegalArgumentException. "Overriding inputs will break model validation")))
  (let [train (return-fold <= year transitions)
        test (return-fold > year transitions)
        train-settings (merge settings {:override-inputs-path (str input-path "temp/" year "/")})]
    (copy-input-files input-path year)
    (write-csv train (str "data/" input-path "temp/" year "/transitions.csv"))
    (write-csv test (str "validation/test_" year ".csv"))
    (run-model train-settings)
    (move-target-files year)))






;; move files to temp folder
;; run model with temp files



;;for each year
  ;; train = split up to fold
  ;; test = split after fold
  ;; write test to csv
  ;; run model with csvs
  ;; store results







;; split files into folds

;; run model with individual folds
  ;; shuttle results into unqiue folders

;; colate results