(ns witan.send.validate-model
  (:require [witan.send.test-utils :as tu]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]))



;; get files to validate on


;;; write validations dir

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

(defn get-unique-years [transitions]
  (->> (map :calendar-year transitions)
       (distinct)
       (map #(Integer/parseInt %))))


(defn return-up-to-fold [year-limit transitions]
  (filter #(<= (Integer/parseInt (:calendar-year %)) year-limit) transitions))

(defn return-after-fold [year-limit transitions]
  (filter #(> (Integer/parseInt (:calendar-year %)) year-limit) transitions))





(defn split-and-test [year transitions]
  (let [train (return-up-to-fold year transitions)
        test (return-after-fold year transitions)]
    (write-csv test (str "validate/test" year ".csv"))))



;;for each year
  ;; train = split up to fold
  ;; test = split after fold
  ;; write test to csv
  ;; run model with csvs
  ;; store results

(defn split-into-folds [transitions]
  (let [years (get-unique-years transitions)]))






;; split files into folds

;; run model with individual folds
  ;; shuttle results into unqiue folders

;; colate results