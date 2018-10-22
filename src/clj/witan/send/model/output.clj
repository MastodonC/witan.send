(ns witan.send.model.output
  (:require [clojure.core.matrix.dataset :as ds]
            [clojure.java.io :as io]
            [witan.send.states :as states]
            [witan.send.report :as report]
            [witan.send.params :as p]
            [witan.send.schemas :as sc]
            [medley.core :as medley]
            [clojure.java.shell :as sh]
            [witan.send.utils :as u :refer [round]]
            [clojure.data.csv :as csv])
  (:import [org.apache.commons.math3.distribution BetaDistribution]))

(defn transition-present? [transition projection]
  (some #(= % transition) projection))

(defn confidence-bounds
  [results calendar-year]
  (let [academic-years (keys results)]
    (->> (for [academic-year (sort academic-years)]
           (let [alpha (get-in results [academic-year calendar-year :alpha] 0)
                 beta (get-in results [academic-year calendar-year :beta])]
             (apply vector academic-year
                    (if (and (pos? alpha) (pos? beta))
                      [(.inverseCumulativeProbability (BetaDistribution. alpha beta) 0.025)
                       (.inverseCumulativeProbability (BetaDistribution. alpha beta) 0.975)]
                      [0 0])))))))

(defn create-keys [string year-count]
  (map (fn [n] (keyword (str string n))) (range year-count)))

(defn pull-year
  [data pos]
  (->> (nth data pos)
       (map (fn [[a b c]] (if (zero? b) [a :NA :NA] [a b c])))
       (apply mapv vector)))

(defn create-CI-map [string data pos year-count]
  (reduce into {}
          (map (fn [k v]
                 (assoc {} k v))
               (create-keys string year-count)
               (map #(nth (nth data %) pos) (range year-count)))))

(defn valid-years-vector? [data]
  (not= (last data) 0))

(defn prep-ribbon-plot-data
  [data years colours]
  (let [n-years (count years)
        filter-data (map #(pull-year data %) (range n-years))
        ay (first (first filter-data))
        uppers (create-CI-map "upper" filter-data 1 n-years)
        lowers (create-CI-map "lower" filter-data 2 n-years)
        non-zero-data (filter valid-years-vector? (first data))
        min-year (first (first non-zero-data))
        max-year (first (last non-zero-data))]
    (merge lowers uppers {:year ay})))

(defn leaver-rate [transitions-filtered]
  (reduce (fn [coll {:keys [calendar-year academic-year-1 setting-1 setting-2]}]
            (let [leaver? (= setting-2 sc/non-send)]
              (-> coll
                  (update-in [academic-year-1 calendar-year :alpha] u/some+ (if leaver? 1 0))
                  (update-in [academic-year-1 calendar-year :beta] u/some+ (if leaver? 0 1)))))
          {} transitions-filtered))

(defn mover-rate [transitions-filtered]
  (reduce (fn [coll {:keys [calendar-year academic-year-1 setting-1 setting-2]}]
            (let [mover? (not= setting-1 setting-2)]
              (-> coll
                  (update-in [academic-year-1 calendar-year :alpha] u/some+ (if mover? 1 0))
                  (update-in [academic-year-1 calendar-year :beta] u/some+ (if mover? 0 1)))))
          {}
          transitions-filtered))

(defn joiner-rate [joiners population ages years]
  (reduce (fn [coll academic-year]
            (reduce (fn [collection calendar-year]
                      (let [j (get-in joiners [calendar-year academic-year] 0)]
                        (assoc-in collection [academic-year calendar-year]
                                  {:alpha j
                                   :beta (- (get-in population [calendar-year academic-year]) j)})))
                    coll years)) {} ages))

(defn output-transitions [file projections]
  (binding [*print-length* nil] (spit file (pr-str projections))))

(defn ribbon-data-rows [ribbon-data]
  (mapv (fn [x] (mapv #(nth % x) (map val ribbon-data))) (range (count (val (last ribbon-data))))))

(defn output-send-results
  "Groups the individual data from the loop to get a demand projection, and applies the cost profile
   to get the total cost."
  [{:keys [projection send-output transition-matrix valid-setting-academic-years
           population modify-transition-by settings-to-change]}
   {:keys [run-outputs run-charts project-dir output-dir settings-to-exclude-in-charts keep-temp-files?]}]
  (let [transitions-data (ds/row-maps transition-matrix)
        transform-transitions (->> transitions-data
                                   (map #(vector
                                          (:academic-year-2 %)
                                          (states/state (:need-1 %) (:setting-1 %))
                                          (states/state (:need-2 %) (:setting-2 %))))
                                   distinct)
        transform-projection (->> projection
                                  keys
                                  (map #(vec (drop 1 %)))
                                  distinct)
        dir (str project-dir "/" output-dir)]
    (if (not (.isDirectory (io/file dir)))
      (.mkdir (io/file dir)))
    (when (every? (fn [transition] (transition-present? transition transform-projection)) transform-transitions)
      (report/info (report/bold "Not every historic transition present in projection!") "Consider checking valid state input.\n"))
    (when run-outputs
      (let [valid-settings (assoc (->> (ds/row-maps valid-setting-academic-years)
                                       (reduce #(assoc %1 (:setting %2) (:setting-group %2)) {}))
                                  :NON-SEND "Other")
            years (sort (distinct (map :calendar-year transitions-data)))
            initial-projection-year (+ 1 (last years))
            joiners-count (p/calculate-joiners-per-calendar-year transitions-data)
            population-count (-> population
                                 ds/row-maps
                                 p/calculate-population-per-calendar-year)
            ages (-> population-count first val keys)
            n-colours (take (count years) ["#1b9e77" "#d95f02" "#7570b3" "#e7298a" "#D55E00" "#CC79A7"])
            joiner-rates (joiner-rate joiners-count population-count ages years)
            joiner-rates-CI (map #(confidence-bounds joiner-rates %) years)
            joiner-ribbon-data (prep-ribbon-plot-data joiner-rates-CI years n-colours)
            filter-leavers (remove (fn [{:keys [setting-1]}] (= setting-1 sc/non-send)) transitions-data)
            leaver-rates (leaver-rate filter-leavers)
            leaver-rates-CI (map #(confidence-bounds leaver-rates %) years)
            leaver-ribbon-data (prep-ribbon-plot-data leaver-rates-CI years n-colours)
            filter-movers (remove (fn [{:keys [setting-1 setting-2]}]
                                    (or (= setting-1 sc/non-send)
                                        (= setting-2 sc/non-send))) transitions-data)
            mover-rates (mover-rate filter-movers)
            mover-rates-CI (map #(confidence-bounds mover-rates %) years)
            mover-ribbon-data (prep-ribbon-plot-data mover-rates-CI years n-colours)]
        ;;future-transitions (mapcat u/projection->transitions projection) ;; for projection investigation

        (report/info "First year of input data: " (report/bold (first years)))
        (report/info "Final year of input data: " (report/bold (inc (last years))))
        (report/info "Final year of projection: " (report/bold (+ (last years) (count (map :total-in-send send-output)))))
        (output-transitions (str dir "/transitions.edn") projection)
        (with-open [writer (io/writer (io/file (str dir "/Output_AY_State.csv")))]
          (let [columns [:calendar-year :academic-year :state :mean :std-dev :iqr :min :low-95pc-bound :q1 :median :q3 :high-95pc-bound :max]]
            (->> (mapcat (fn [output year]
                           (map (fn [[[academic-year state] stats]]
                                  (-> (medley/map-vals round stats)
                                      (assoc :academic-year academic-year :state state :calendar-year year))) (:by-state output))) send-output (range initial-projection-year 3000))
                 (map (apply juxt columns))
                 (concat [(map name columns)])
                 (csv/write-csv writer))))
        (with-open [writer (io/writer (io/file (str dir "/Output_AY.csv")))]
          (let [columns [:calendar-year :academic-year :mean :std-dev :iqr :min :low-95pc-bound :q1 :median :q3 :high-95pc-bound :max]]
            (->> (mapcat (fn [output year]
                           (map (fn [[academic-year stats]]
                                  (-> (medley/map-vals round stats)
                                      (assoc :academic-year academic-year :calendar-year year)))
                                (:total-in-send-by-ay output))) send-output (range initial-projection-year 3000))
                 (map (apply juxt columns))
                 (concat [(map name columns)])
                 (csv/write-csv writer))))
        (with-open [writer (io/writer (io/file (str dir "/Output_Need.csv")))]
          (let [columns [:calendar-year :need :mean :std-dev :iqr :min :low-95pc-bound :q1 :median :q3 :high-95pc-bound :max]]
            (->> (mapcat (fn [output year]
                           (map (fn [[need stats]]
                                  (-> (medley/map-vals round stats)
                                      (assoc :need (name need) :calendar-year year)))
                                (:total-in-send-by-need output))) send-output (range initial-projection-year 3000))
                 (map (apply juxt columns))
                 (concat [(map name columns)])
                 (csv/write-csv writer))))
        (with-open [writer (io/writer (io/file (str dir "/Output_Setting.csv")))]
          (let [columns [:calendar-year :setting :mean :std-dev :iqr :min :low-95pc-bound :q1 :median :q3 :high-95pc-bound :max]]
            (->> (mapcat (fn [output year]
                           (map (fn [[setting stats]]
                                  (-> (medley/map-vals round stats)
                                      (assoc :setting (name setting) :calendar-year year)))
                                (:total-in-send-by-setting output))) send-output (range initial-projection-year 3000))
                 (map (apply juxt columns))
                 (concat [(map name columns)])
                 (csv/write-csv writer))))
        (with-open [writer (io/writer (io/file (str dir "/Output_Count.csv")))]
          (let [columns [:calendar-year :mean :std-dev :iqr :min :low-95pc-bound :q1 :median :q3 :high-95pc-bound :max]]
            (->> (map (fn [stats year]
                        (-> (medley/map-vals round stats)
                            (assoc :calendar-year year)))
                      (map :total-in-send send-output) (range initial-projection-year 3000))
                 (map (apply juxt columns))
                 (concat [(map name columns)])
                 (csv/write-csv writer))))
        (with-open [writer (io/writer (io/file (str dir "/Output_Cost.csv")))]
          (let [columns [:calendar-year :mean :std-dev :iqr :min :low-95pc-bound :q1 :median :q3 :high-95pc-bound :max]]
            (->> (map (fn [stats year]
                        (-> (medley/map-vals round stats)
                            (assoc :calendar-year year)))
                      (map :total-cost send-output) (range initial-projection-year 3000))
                 (map (apply juxt columns))
                 (concat [(map name columns)])
                 (csv/write-csv writer))))
        (with-open [writer (io/writer (io/file (str dir "/Output_AY_Group.csv")))]
          (let [columns [:calendar-year :ay-group :mean :std-dev :iqr :min :low-95pc-bound :q1 :median :q3 :high-95pc-bound :max]]
            (->> (mapcat (fn [output year]
                           (map (fn [[ay-group stats]]
                                  (-> (medley/map-vals round stats)
                                      (assoc :ay-group ay-group :calendar-year year)))
                                (:total-in-send-by-ay-group output))) send-output (range initial-projection-year 3000))
                 (map (apply juxt columns))
                 (concat [(map name columns)])
                 (csv/write-csv writer))))
        (when run-charts
          (with-open [writer (io/writer (io/file (str dir "/historic-data.csv")))]
            (let [columns [:calendar-year :setting-1 :need-1 :academic-year-1 :setting-2 :need-2]
                  headers (mapv name columns)
                  rows (mapv #(mapv % columns) transitions-data)]
              (csv/write-csv writer (into [headers] rows))))
          (with-open [writer (io/writer (io/file (str dir "/valid-settings.csv")))]
            (csv/write-csv writer valid-settings))
          (with-open [writer (io/writer (io/file (str dir "/joiner-rates.csv")))]
            (let [columns (into [] (keys joiner-ribbon-data))
                  headers (mapv name columns)
                  rows (ribbon-data-rows joiner-ribbon-data)]
              (csv/write-csv writer (into [headers] rows))))
          (with-open [writer (io/writer (io/file (str dir "/leaver-rates.csv")))]
            (let [columns (into [] (keys leaver-ribbon-data))
                  headers (mapv name columns)
                  rows (ribbon-data-rows leaver-ribbon-data)]
              (csv/write-csv writer (into [headers] rows))))
          (with-open [writer (io/writer (io/file (str dir "/mover-rates.csv")))]
            (let [columns (into [] (keys mover-ribbon-data))
                  headers (mapv name columns)
                  rows (ribbon-data-rows mover-ribbon-data)]
              (csv/write-csv writer (into [headers] rows))))
          (println "Producing charts...")
          (with-open [in (io/input-stream (io/resource "send-charts.R"))]
            (io/copy in (io/file "/tmp/send-charts.R")))
          (sh/sh "Rscript" "--vanilla" "/tmp/send-charts.R" dir settings-to-exclude-in-charts)
          (when-not keep-temp-files?
            (run! #(io/delete-file (str dir "/" %) :quiet)
                  ["historic-data.csv" "valid-settings.csv" "joiner-rates.csv"
                   "leaver-rates.csv" "mover-rates.csv"])))))
    (report/write-send-report (str dir "/SEND_Log.md"))))
