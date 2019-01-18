(ns witan.send.model.input
  (:require [clojure.core.matrix.dataset :as ds]
            [clojure.data.csv :as data-csv]
            [clojure.java.io :as io]
            [schema.coerce :as coerce]
            [witan.send.schemas :as sc]))

(defn blank-row? [row]
  (every? #(= "" %) row))

(defn load-csv
  "Loads csv file with each row as a vector.
   Stored in map separating column-names from data"
  ([filename]
   (let [parsed-csv (with-open [in-file (io/reader filename)]
                      (into [] (->> in-file
                                    data-csv/read-csv
                                    (remove (fn [row] (blank-row? row))))))
         parsed-data (rest parsed-csv)
         headers (first parsed-csv)]
     {:column-names headers
      :columns (vec parsed-data)})))

(defn apply-row-schema
  [col-schema csv-data]
  (let [row-schema (sc/make-row-schema col-schema)]
    (map (coerce/coercer row-schema coerce/string-coercion-matcher)
         (:columns csv-data))))

(defn apply-col-names-schema
  [col-schema csv-data]
  (let [col-names-schema (sc/make-col-names-schema col-schema)]
    ((coerce/coercer col-names-schema coerce/string-coercion-matcher)
     (:column-names csv-data))))

(defn apply-schema-coercion [data schema]
  {:column-names (apply-col-names-schema schema data)
   :columns (vec (apply-row-schema schema data))})

(defn csv-to-dataset
  "Takes in a file path and a schema. Creates a dataset with the file
   data after coercing it using the schema."
  [filepath schema]
  (-> (load-csv filepath)
      (apply-schema-coercion schema)
      (as-> {:keys [column-names columns]} (ds/dataset column-names columns))))

(defn is-dataset-erroneous? [dataset]
  (->> dataset second :column-names first ((complement instance?) clojure.lang.Keyword)))

(defn check-dataset [pred s datasets]
  (->> datasets
       (filter pred)
       (map first)
       (clojure.string/join ", ")
       (str s)))

(defn check-if-dataset-is-valid
  "Checks each expected dataset for errors"
  [datasets]
  (if (some true? (map is-dataset-erroneous? datasets))
    (check-dataset is-dataset-erroneous? "The following datasets may not exist or have errors in them: " (filter is-dataset-erroneous? datasets))
    true))

(defn build-input-datasets
  "Build a map of the datasets to use for input"
  [project-dir file-inputs schema-inputs]
  (into {} (for [[k v] file-inputs]
             [k (csv-to-dataset (str project-dir "/" v) (k schema-inputs))])))
