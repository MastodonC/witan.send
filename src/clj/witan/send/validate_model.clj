(ns witan.send.validate-model
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [me.raynes.fs :refer [copy-dir]]
            [witan.send.main :refer [run-send config]]))


(defn temp-dir [project-dir]
  "Returns path to validation temp dir for project"
  (str project-dir "data/temp/"))


(defn csv-data->maps [csv-data]
  "Returns CSV data as seq of maps"
  (map zipmap
       (->> (first csv-data)
            (map keyword)
            repeat)
       (rest csv-data)))


(defn write-csv [file data]
  "Write seq of maps to CSV"
  (with-open [writer (io/writer (io/file file))]
    (let [columns (into [] (keys (first data)))
          headers (mapv name columns)
          rows (mapv #(mapv % columns) data)]
      (csv/write-csv writer (into [headers] rows)))))


(defn load-csv-as-maps [file]
  "Loads CSV data as maps"
  (csv-data->maps
    (with-open [reader (io/reader file)]
      (doall
        (csv/read-csv reader)))))


(defn get-validation-years [transitions]
  "Returns list of years to use for delineate validation folds"
  (->> (map :calendar-year transitions)
       (distinct)
       (map #(Integer/parseInt %))
       (drop-last)))


(defn return-fold [bound-fn year transitions]
  "Splits transitions data based on condition passed with bound-fn and year e.g. > 2016"
  (filter #(bound-fn (Integer/parseInt (:calendar-year %)) year) transitions))


(defn return-testable-data [file test-years]
  "Creates subset of model output for years where there is ground truth for comparison"
  (->> (for [year test-years] (filter #(= year (:calendar-year %)) (load-csv-as-maps file)))
       (mapcat seq)))


(defn append-count-with-test [model-count test-data n-transitions]
  "Adds total population count ground truth to model output"
  (let [test-year (str (dec (Integer/parseInt (:calendar-year model-count))))
        test-data-for-year  (filter #(= test-year (:calendar-year %)) test-data)
        send-test-data-for-year  (filter #(not= "NONSEND" (:need-2 %)) test-data-for-year)
        ground-truth-count (count send-test-data-for-year)]
    (assoc model-count :ground-truth (str ground-truth-count) :n-transition n-transitions)))


(defn append-state-with-test [model-state test-data n-transitions]
  "Adds states ground truth from test-data to model output"
  (let [test-year (str (dec (Integer/parseInt (:calendar-year model-state))))
        [need setting] (str/split (str/replace (:state model-state) ":" "") #"-")
        academic-year (:academic-year model-state)
        test-data-for-state (filter #(and (= test-year (:calendar-year %))
                                          (= need (:need-2 %))
                                          (= setting (:setting-2 %))
                                          (= academic-year (:academic-year-2 %))) test-data)
        ground-truth-count (count test-data-for-state)]
    (assoc model-state :ground-truth (str ground-truth-count) :n-transitions (str n-transitions))))


(defn collate-fold [project-dir test-data year n-transitions]
  (let [test-years (->> (map :calendar-year test-data)
                        (distinct)
                        (map #(inc (Integer/parseInt %)))
                        (map str))
        model-state (return-testable-data (str (temp-dir project-dir) year "/Output_AY_State.csv") test-years)
        model-count (return-testable-data (str (temp-dir project-dir) year "/Output_Count.csv") test-years)
        count-results (map #(append-count-with-test % test-data n-transitions) model-count)
        state-results (map #(append-state-with-test % test-data n-transitions) model-state)]
    {:count count-results :state state-results}))


(defn validate-fold [config year]
  "Splits project transitions.csv into training and test data, before and after year argument.
  Runs model with config, overriding default transition matrix with training data.
  Returns map containing results of comparing model predictions with test data for total count and individual states"

  (let [project-dir (:project-dir config)
        transitions (load-csv-as-maps (str project-dir (:transition-matrix (:file-inputs config))))
        train-data (return-fold <= year transitions)
        n-transitions (count (distinct (map :calendar-year train-data)))
        test-data (return-fold > year transitions)
        fold-train-path (str "data/temp/" year "/transitions.csv")
        fold-config (-> config
                      (assoc-in [:file-inputs :transition-matrix] fold-train-path)
                      (assoc-in [:run-parameters :seed-year] (inc year))
                      (assoc-in [:output-parameters :run-charts] false)
                      (assoc-in [:output-parameters :output-dir] (str "data/temp/" year "/")))]
    (.mkdir (java.io.File. (str project-dir "data/temp/" year "/")))
    (write-csv (str project-dir fold-train-path) train-data)
    (write-csv (str (temp-dir project-dir) year "/test.csv") test-data)
    (run-send fold-config)
    (collate-fold project-dir test-data year n-transitions)))


(defn write-validation-results [project-dir results]
  "Takes seq of maps containing results returned by (validate-fold) and writes to disk"
  (->> (map :count results)
       flatten
       (write-csv (str project-dir "validation/validation_result_count.csv")))
  (->> (map :state results)
       flatten
       (write-csv (str project-dir "validation/validation_results_state.csv"))))


(defn setup-validation-dirs [project-dir]
  "Create dirs required for validation process"
  (doseq [dir [(temp-dir project-dir) (str project-dir "validation/")]]
    (.mkdir (java.io.File. dir))))


(defn tear-down-validation-dirs [project-dir keep-temp-files?]
  "Remove input data and results for separate folds, move to validation dir if keep-temp-files is true"
  (if keep-temp-files?
    (copy-dir (temp-dir project-dir) (str project-dir "validation/")))
  (doseq [file (reverse (file-seq (io/file (temp-dir project-dir))))]
    (io/delete-file file)))


(defn run-validation [project-dir keep-temp-files?]
  "Main function to validate SEND model.

  Args:
    project-dir: the same path you would would pass to run the main send model, which must contain a config.edn file
    keep-temp-files? true if you wish to inspect training, test and output data for individual folds, false otherwise

  Results are stored in a directory called validation within the project"

  (setup-validation-dirs project-dir)
  (let [config (config project-dir)
        years-to-validate (-> (str project-dir (:transition-matrix (:file-inputs config)))
                              load-csv-as-maps
                              get-validation-years)
        results (doall (map #(validate-fold config %) years-to-validate))]
    (write-validation-results project-dir results))
  (tear-down-validation-dirs project-dir keep-temp-files?))

