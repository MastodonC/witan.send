(ns witan.send.validate-model
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [me.raynes.fs :refer [copy-dir]]
            [witan.send.acceptance.workspace-test :refer [run-model]]))


(def static-input-files ["modify-settings.csv" "population.csv" "need-setting-costs.csv" "valid-setting-academic-years.csv"])


(defn temp-dir [input-path]
  (str "data/" input-path "temp/"))


(defn csv-data->maps [csv-data]
  (map zipmap
       (->> (first csv-data)
            (map keyword)
            repeat)
       (rest csv-data)))


(defn write-csv [file data]
  (with-open [writer (io/writer (io/file file))]
    (let [columns (into [] (keys (first data)))
          headers (mapv name columns)
          rows (mapv #(mapv % columns) data)]
    (csv/write-csv writer (into [headers] rows)))))


(defn load-csv-as-maps [file]
  (csv-data->maps
    (with-open [reader (io/reader file)]
      (doall
        (csv/read-csv reader)))))


(defn get-validation-years [transitions]
  (->> (map :calendar-year transitions)
       (distinct)
       (map #(Integer/parseInt %))
       (drop-last)))


(defn return-fold [bound-fn year transitions]
  (filter #(bound-fn (Integer/parseInt (:calendar-year %)) year) transitions))


(defn copy-if-exists [source-path destination-path file]
  (let [source (str source-path file)]
    (if (.exists (clojure.java.io/as-file source))
      (io/copy (io/file source) (io/file (str destination-path file))))))


(defn copy-input-files [input-path year]
  (let [source-path (str "data/" input-path)
        destination-path (str (temp-dir input-path) year "/")]
    (.mkdir (java.io.File. destination-path))
    (doseq [file static-input-files]
      (copy-if-exists source-path destination-path file))))


(defn move-target-files [input-path year]
  (doseq [file ["Output_Count.csv" "Output_AY_State.csv"]]
    (io/copy (io/file (str "target/" file)) (io/file (str (temp-dir input-path) year "_" file)))))


(defn return-testable-data [file test-years]
  (->> (for [year test-years] (filter #(= year (:calendar-year %)) (load-csv-as-maps file)))
       (mapcat seq)))


(defn append-count-with-test [model-count test-data n-transitions]
  (let [test-year (str (dec (Integer/parseInt (:calendar-year model-count))))
        test-data-for-year  (filter #(= test-year (:calendar-year %)) test-data)
        send-test-data-for-year  (filter #(not= "NONSEND" (:need-2 %)) test-data-for-year)
        ground-truth-count (count send-test-data-for-year)]
    (assoc model-count :ground-truth (str ground-truth-count) :n-transition n-transitions)))


(defn append-state-with-test [model-state test-data n-transitions]
  (let [test-year (str (dec (Integer/parseInt (:calendar-year model-state))))
        [need setting] (str/split (str/replace (:state model-state) ":" "") #"-")
        academic-year (:academic-year model-state)
        test-data-for-state (filter #(and (= test-year (:calendar-year %))
                                          (= need (:need-2 %))
                                          (= setting (:setting-2 %))
                                          (= academic-year (:academic-year-2 %))) test-data)
        ground-truth-count (count test-data-for-state)]
    (assoc model-state :ground-truth (str ground-truth-count) :n-transitions (str n-transitions))))


(defn collate-fold [input-path year n-transitions]
  (let [test-data (load-csv-as-maps (str (temp-dir input-path) "test_" year ".csv"))
        test-years (->> (map :calendar-year test-data)
                        (distinct)
                        (map #(inc (Integer/parseInt %)))
                        (map str))
        model-state (return-testable-data (str (temp-dir input-path) year "_Output_AY_State.csv") test-years)
        model-count (return-testable-data (str (temp-dir input-path) year "_Output_Count.csv") test-years)
        count-results (map #(append-count-with-test % test-data n-transitions) model-count)
        state-results (map #(append-state-with-test % test-data n-transitions) model-state)]
    (write-csv (str (temp-dir input-path) "results_" year "_count.csv") count-results)
    (write-csv (str (temp-dir input-path) "results_" year "_state.csv") state-results)))


(defn validate-fold [input-path year transitions settings]
  (let [train-data (return-fold <= year transitions)
        n-transitions (count (distinct (map :calendar-year train-data)))
        test-data (return-fold > year transitions)
        fold-settings (merge settings {:output? true :override-inputs-path (str input-path "temp/" year "/")})]
    (copy-input-files input-path year)
    (write-csv (str (temp-dir input-path) year "/transitions.csv") train-data)
    (write-csv (str (temp-dir input-path) "test_" year ".csv") test-data)
    (run-model fold-settings)
    (move-target-files input-path year)
    (collate-fold input-path year n-transitions)))


(defn run-validation [input-path settings-map keep-temp-files?]
  "Function to validate SEND model.
  input-path is a string with the data directory to validate the model on e.g. demo/
  settings-map expects a hash map containing run-model parameters to use for validation eg. {:iterations 100}
  (note that run-model's :output? and :override-inputs-path arguments are both required for validation and are overwritten if provided)
  keep-temp-files? can be set to false unless debugging etc."
  (doseq [path [(temp-dir input-path) "validation/"]]
    (.mkdir (java.io.File. path)))
  (let [transitions (load-csv-as-maps (str "data/" input-path "transitions.csv"))
        years-to-validate (get-validation-years transitions)]
    (doseq [year years-to-validate]
      (validate-fold input-path year transitions settings-map))
    (->> (map #(load-csv-as-maps (str "data/" input-path "temp/results_" % "_count.csv")) years-to-validate)
         (flatten)
         (write-csv (str "validation/validation_result_count_" (str/join (drop-last input-path)) ".csv")))
    (->> (map #(load-csv-as-maps (str "data/" input-path "temp/results_" % "_state.csv")) years-to-validate)
         (flatten)
         (write-csv (str "validation/validation_result_state_" (str/join (drop-last input-path)) ".csv"))))
  (if keep-temp-files?
    (copy-dir (temp-dir input-path) (str "validation/" (str/join (drop-last input-path)) "_temp_files/")))
  (doseq [file (reverse (file-seq (io/file (temp-dir input-path))))]
    (io/delete-file file)))
