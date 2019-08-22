(ns witan.send.model.input
  (:require [clojure.data.csv :as data-csv]
            [clojure.java.io :as io]))

(defn ->int [x]
  (cond (int? x)
        x
        (double? x)
        (int x)
        (string? x)
        (int (Double/valueOf x))
        :else
        (throw (ex-info (format "Failed to parse supplied value '%s'" x)
                        {:value x}))))

(defn ->double [x]
  (cond (double? x)
        x
        (int? x)
        (double x)
        (string? x)
        (Double/valueOf x)
        :else
        (throw (ex-info (format "Failed to parse supplied value '%s'" x)
                        {:value x}))))

(defn csv->x [xf file-name]
  (with-open [reader (io/reader file-name)]
    (let [[header & data] (data-csv/read-csv reader)
          header (map keyword header)]
      (into []
            (comp
             (map #(zipmap header %))
             xf)
            data))))
