(ns witan.send.report
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def send-report (atom []))

(def send-report-file "target/SEND_Log.md")

(defn bold [string]
  (str "__" string "__"))

(defn info [& messages]
  (swap! send-report conj (apply str messages)))

(defn write-send-report
  ([]
   (write-send-report send-report-file))
  ([report-file]
   (io/delete-file report-file :quiet)
   (spit report-file (str/join "\n\n" @send-report) :append true)))

(defn reset-send-report
  []
  (reset! send-report []))
