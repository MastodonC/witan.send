(ns witan.send.model.output
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [medley.core :as medley]
            [witan.send.constants :as c]
            [witan.send.maths :as m]
            [witan.send.params :as p]
            [witan.send.report :as report]
            [witan.send.states :as states]
            [witan.send.model.data-products.setting-summary :as setting-summary])
  (:import org.apache.commons.math3.distribution.BetaDistribution))

(defn transition-present? [transition projection]
  (some #(= % transition) projection))

(defn confidence-bounds
  [results calendar-year]
  (when-let [academic-years (keys results)]
    (for [academic-year (sort academic-years)]
      (let [alpha (get-in results [academic-year calendar-year :alpha] 0)
            beta (get-in results [academic-year calendar-year :beta])]
        (apply vector academic-year
               (if (and (pos? alpha) (pos? beta))
                 [(.inverseCumulativeProbability (BetaDistribution. alpha beta) 0.025)
                  (.inverseCumulativeProbability (BetaDistribution. alpha beta) 0.975)]
                 [0 0]))))))

(defn create-keys [string year-count]
  (map (fn [n] (keyword (str string n))) (range year-count)))

(defn pull-year
  [data pos]
  (when (seq? data)
    (->> (nth data pos)
         (map (fn [[a b c]] (if (zero? b) [a :NA :NA] [a b c])))
         (apply mapv vector))))

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
  (when (first data)
    (let [n-years (count years)
          filter-data (map #(pull-year data %) (range n-years))
          ay (first (first filter-data))
          uppers (create-CI-map "upper" filter-data 1 n-years)
          lowers (create-CI-map "lower" filter-data 2 n-years)
          non-zero-data (filter valid-years-vector? (first data))
          min-year (first (first non-zero-data))
          max-year (first (last non-zero-data))]
      (merge lowers uppers {:year ay}))))

(defn leaver-rate [transitions-filtered]
  (reduce (fn [coll {:keys [calendar-year academic-year-1 setting-1 setting-2]}]
            (let [leaver? (= setting-2 c/non-send)]
              (-> coll
                  (update-in [academic-year-1 calendar-year :alpha] m/some+ (if leaver? 1 0))
                  (update-in [academic-year-1 calendar-year :beta] m/some+ (if leaver? 0 1)))))
          {} transitions-filtered))

(defn mover-rate [transitions-filtered]
  (reduce (fn [coll {:keys [calendar-year academic-year-1 setting-1 setting-2]}]
            (let [mover? (not= setting-1 setting-2)]
              (-> coll
                  (update-in [academic-year-1 calendar-year :alpha] m/some+ (if mover? 1 0))
                  (update-in [academic-year-1 calendar-year :beta] m/some+ (if mover? 0 1)))))
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

(defn output-transitions-edn
  [dir filename projections]
  (binding [*print-length* nil] (spit (str dir "/" filename ".edn") (pr-str projections))))

(defn output-transitions-csv
  [dir filename projections simulations]
  (with-open [writer (io/writer (io/file (str dir "/" filename ".csv")))]
    (let [columns [:calendar-year :academic-year-1 :setting-1 :need-1 :academic-year-2 :setting-2 :need-2 :avg-number-of]
          headers (mapv name columns)
          parse-need-setting #(map name ((comp reverse states/split-need-setting) %))
          rows (sort (remove empty?
                             (for [[[cy ay need-setting-source need-setting-destination] transitions] projections]
                               (when (> (m/round0 (/ transitions simulations)) 0)
                                 (into [] (flatten [cy
                                                    ay
                                                    (parse-need-setting need-setting-source)
                                                    (inc ay)
                                                    (parse-need-setting need-setting-destination)
                                                    (m/round0 (/ transitions simulations))]))))))]
      (csv/write-csv writer (into [headers] rows)))))


(defn format-transition-count [idx [[cy ay need-setting-1 need-setting-2] transition-count]]
  (let [[need-1 setting-1]  (states/split-need-setting need-setting-1)
        [need-2 setting-2]  (states/split-need-setting need-setting-2)]
    [idx cy
     (name setting-1) (name need-1) ay
     (name setting-2) (name need-2) (inc ay)
     transition-count]))

(defn format-year [idx year]
  (into []
        (map (fn [transition-count]
               (format-transition-count idx transition-count)))
        year))

(defn format-simulation [[idx years]]
  (into []
        (mapcat (partial format-year idx))
        years))

(defn output-simulated-transitions [dir filename simulated-transitions]
  (with-open [writer (io/writer (io/file (str dir "/" filename ".csv")))]
    (let [header
          ["simulation" "calendar-year" "setting-1" "need-1" "academic-year-1" "setting-2" "need-2" "academic-year-2" "transition-count"]]
      (csv/write-csv writer
                     (into [header]
                           (mapcat format-simulation)
                           simulated-transitions)))))

(defn ribbon-data-rows [ribbon-data]
  (mapv (fn [x] (mapv #(nth % x) (map val ribbon-data))) (range (count (val (last ribbon-data))))))

(defn bound-or-interval?
  "default behaviour is to use bounds rather than CI"
  [string]
  (= string "interval"))

(defn qstr [s]
  (if (or (nil? s)
          (= (System/getProperty "os.name") "Windows 10"))
    (str "\"" s "\"")
    s))

(defn r-plots [dir pop-path settings-to-exclude use-confidence-bound-or-interval]
  (let [send-charts (str (System/getProperty "java.io.tmpdir") "/send-charts.R")
        response (sh/sh "Rscript" "--vanilla" send-charts
                        "--output-dir" (qstr dir)
                        "--population-file" (qstr pop-path)
                        "--exclude-settings" (qstr settings-to-exclude)
                        "--bound" (if (bound-or-interval? use-confidence-bound-or-interval)
                                    (qstr ".ci")
                                    (qstr ".95pc.bound")))]
    (if-not (zero? (:exit response))
      (throw (Exception. (:err response))))))

; Helper Fns for outputting Beta parameters as expectations
(defn expectation
  "Calculate the expectation from the Beta distros alpha and beta parameters"
  [alpha beta]
  (let [total (+ alpha beta)]
    (if (not= 0 total)
      (/ alpha total)
      0)))

(defn beta-params-expectation
  "Return alpha, beta and expectation as a vector from a hash"
  [{:keys [:alpha :beta]}]
  (mapv float [alpha beta (expectation alpha beta)]))

(defn beta-expectations
  "Beta disto parameters are stored in a map typically keyed by an entities index (ay, n, s).
  Apply expectation to the values of this map"
  [betas]
  (-> (for [[k params] betas]
        (if (vector? k)                                     ; this is weak, better to use schema checking here
          (into [] (concat (states/split-entity k) (beta-params-expectation params)))
          (into [k] (beta-params-expectation params))))
      (sort)))

(defn normalisation-constant-dirichlet [params]
  (reduce + (map val params)))

(defn dirichlet-expectations
  "Dirichlet distro parameters are stored in a map typically keyed by an entities index (ay, n, s)."
  [alphas]
  (reduce (fn [acc [k params]]
            (into acc
                  (let [prefix (if (vector? k)
                                 (let [v (states/split-entity k)
                                       ay+1 (inc (first v))]
                                   (conj v ay+1))
                                 [k])
                        normalisation (normalisation-constant-dirichlet params)]
                    (map (fn [p] (into []
                                       (into prefix
                                             (conj (mapv name (states/split-need-setting (key p)))
                                                   (/ (val p) normalisation)
                                                   normalisation
                                                   (val p)))))
                         params))))
          [] alphas))

(defn output-beta-expectations
  "Write out beta expectations to a supplied `dir` and `file` using a map of beta parameters.
  Except an argument to overwrite the column headings when the index is not
  entity (ay, n, s) based e.g joiner beta params are indexed by ay."
  [dir filename betas & cols]
  (with-open [writer (io/writer (io/file (str dir "/" filename ".csv")))]
    (let [columns (or (first cols) [:ay :need :setting :alpha :beta :expectation])
          headers (mapv name columns)
          rows (beta-expectations betas)]
      (csv/write-csv writer (into [headers] rows)))))

(defn output-dirichlet-expectations
  "Write out beta expectations to a supplied `dir` and `file` using a map of beta parameters.
  Except an argument to overwrite the column headings when the index is not
  entity (ay, n, s) based e.g joiner beta params are indexed by ay."
  [dir filename alphas & cols]
  (with-open [writer (io/writer (io/file (str dir "/" filename ".csv")))]
    (let [columns (or (first cols) [:ay :need :setting :ay-destination :need-destination :setting-destination :expectation :normalisation-constant :alpha])
          headers (mapv name columns)
          rows (dirichlet-expectations alphas)]
      (csv/write-csv writer (into [headers] rows)))))

(defn parse-pop-state
  "Pop values are keyed by an entities index (ay, n, s)."
  [pop-state]
  (-> (for [[k pop] pop-state]
        (into [] (flatten [(states/split-entity k) pop])))
      (sort)))

(defn output-initial-population-state
  "Write out initial population state"
  [dir filename pop-state & cols]
  (with-open [writer (io/writer (io/file (str dir "/" filename ".csv")))]
    (let [columns (or (first cols) [:ay :need :setting :pop])
          headers (mapv name columns)
          rows (parse-pop-state pop-state)]
      (csv/write-csv writer (into [headers] rows)))))

; Core function
(defn output-send-results
  "Groups the individual data from the loop to get a demand projection, and applies the cost profile
   to get the total cost."
  [{:keys [projection send-output transitions valid-states
           population standard-projection scenario-projection
           simulations simulated-transitions]}
   {:keys [run-outputs run-charts project-dir output-dir settings-to-exclude-in-charts
           keep-temp-files? use-confidence-bound-or-interval population-file]}]
  (let [transform-transitions (->> transitions
                                   (map #(vector
                                          (:academic-year-2 %)
                                          (states/join-need-setting (:need-1 %) (:setting-1 %))
                                          (states/join-need-setting (:need-2 %) (:setting-2 %))))
                                   distinct)
        transform-projection (->> projection
                                  keys
                                  (map #(vec (drop 1 %)))
                                  distinct)
        dir (str project-dir "/" output-dir)]
    (when (not (.isDirectory (io/file dir)))
      (.mkdir (io/file dir)))
    (when (every? (fn [transition] (transition-present? transition transform-projection)) transform-transitions)
      (report/info (report/bold "Not every historic transition present in projection!") "Consider checking valid state input.\n"))
    (when run-outputs
      (let [valid-settings (assoc (reduce #(assoc %1 (:setting %2) (:setting-group %2)) {} valid-states)
                                  :NON-SEND "Other")
            years (sort (distinct (map :calendar-year transitions)))
            initial-projection-year (+ 1 (last years))
            joiners-count (p/calculate-joiners-per-calendar-year transitions)
            population-count (-> population
                                 p/calculate-population-per-calendar-year)
            ages (-> population-count first val keys)
            n-colours (take (count years) ["#1b9e77" "#d95f02" "#7570b3" "#e7298a" "#D55E00" "#CC79A7"])
            joiner-rates (joiner-rate joiners-count population-count ages years)
            joiner-rates-CI (map #(confidence-bounds joiner-rates %) years)
            joiner-ribbon-data (prep-ribbon-plot-data joiner-rates-CI years n-colours)
            filter-leavers (remove (fn [{:keys [setting-1]}] (= setting-1 c/non-send)) transitions)
            leaver-rates (leaver-rate filter-leavers)
            leaver-rates-CI (map #(confidence-bounds leaver-rates %) years)
            leaver-ribbon-data (prep-ribbon-plot-data leaver-rates-CI years n-colours)
            filter-movers (remove (fn [{:keys [setting-1 setting-2]}]
                                    (or (= setting-1 c/non-send)
                                        (= setting-2 c/non-send))) transitions)
            mover-rates (mover-rate filter-movers)
            mover-rates-CI (map #(confidence-bounds mover-rates %) years)
            mover-ribbon-data (prep-ribbon-plot-data mover-rates-CI years n-colours)]
        ;;future-transitions (mapcat u/projection->transitions projection) ;; for projection investigation

        (report/info "First year of input data: " (report/bold (first years)))
        (report/info "Final year of input data: " (report/bold (inc (last years))))
        (report/info "Final year of projection: " (report/bold (+ (last years) (count (map :total-in-send send-output)))))
        (output-transitions-edn dir "transitions" projection)
        (output-transitions-csv dir "transitions" projection simulations)
        (output-simulated-transitions dir "simulated-transitions" simulated-transitions)
        (with-open [writer (io/writer (io/file (str dir "/Output_State.csv")))]
          (let [columns [:calendar-year :academic-year :need-setting :mean :std-dev :iqr :min :low-95pc-bound :q1 :median :q3 :high-95pc-bound :max :low-ci :high-ci]]
            (->> (mapcat (fn [output year]
                           (map (fn [[[academic-year need-setting] stats]]
                                  (-> (medley/map-vals m/round stats)
                                      (assoc :academic-year academic-year :need-setting (name need-setting) :calendar-year year)))
                                (:by-state output))) send-output (range initial-projection-year 3000))
                 (map (apply juxt columns))
                 (into [(mapv name columns)])
                 (csv/write-csv writer))))
        (with-open [writer (io/writer (io/file (str dir "/Output_State_pop_only.csv")))]
          (let [columns [:calendar-year :academic-year :need-setting :mean]
                output (->> (mapcat (fn [output year]
                                      (map (fn [[[academic-year need-setting] stats]]
                                             (-> (medley/map-vals m/round0 stats)
                                                 (assoc :academic-year academic-year
                                                        :need-setting (into [] (map name (states/split-need-setting need-setting)))
                                                        :calendar-year year)))
                                           (:by-state output))) send-output (range initial-projection-year 3000))
                            (map (apply juxt columns))
                            (sort)
                            (into [(mapv name [:calendar-year :academic-year :need :setting :population])])
                            (mapv flatten))]
            (csv/write-csv writer output)))
        (with-open [writer (io/writer (io/file (str dir "/Output_AY.csv")))]
          (let [columns [:calendar-year :academic-year :mean :std-dev :iqr :min :low-95pc-bound :q1 :median :q3 :high-95pc-bound :max :low-ci :high-ci]]
            (->> (mapcat (fn [output year]
                           (map (fn [[academic-year stats]]
                                  (-> (medley/map-vals m/round stats)
                                      (assoc :academic-year academic-year :calendar-year year)))
                                (:total-in-send-by-ay output))) send-output (range initial-projection-year 3000))
                 (map (apply juxt columns))
                 (into [(mapv name columns)])
                 (csv/write-csv writer))))
        (with-open [writer (io/writer (io/file (str dir "/Output_Need.csv")))]
          (let [columns [:calendar-year :need :mean :std-dev :iqr :min :low-95pc-bound :q1 :median :q3 :high-95pc-bound :max :low-ci :high-ci]]
            (->> (mapcat (fn [output year]
                           (map (fn [[need stats]]
                                  (-> (medley/map-vals m/round stats)
                                      (assoc :need (name need) :calendar-year year)))
                                (:total-in-send-by-need output))) send-output (range initial-projection-year 3000))
                 (map (apply juxt columns))
                 (into [(mapv name columns)])
                 (csv/write-csv writer))))
        (setting-summary/->output-setting-csv dir initial-projection-year send-output)
        (setting-summary/->output-setting-cost-csv dir initial-projection-year send-output)
        (with-open [writer (io/writer (io/file (str dir "/Output_Count.csv")))]
          (let [columns [:calendar-year :mean :std-dev :iqr :min :low-95pc-bound :q1 :median :q3 :high-95pc-bound :max :low-ci :high-ci]]
            (->> (map (fn [stats year]
                        (-> (medley/map-vals m/round stats)
                            (assoc :calendar-year year)))
                      (map :total-in-send send-output) (range initial-projection-year 3000))
                 (map (apply juxt columns))
                 (into [(mapv name columns)])
                 (csv/write-csv writer))))
        (with-open [writer (io/writer (io/file (str dir "/Output_Cost.csv")))]
          (let [columns [:calendar-year :mean :std-dev :iqr :min :low-95pc-bound :q1 :median :q3 :high-95pc-bound :max :low-ci :high-ci]]
            (->> (map (fn [stats year]
                        (-> (medley/map-vals m/round stats)
                            (assoc :calendar-year year)))
                      (map :total-cost send-output) (range initial-projection-year 3000))
                 (map (apply juxt columns))
                 (into [(mapv name columns)])
                 (csv/write-csv writer))))
        (with-open [writer (io/writer (io/file (str dir "/Output_AY_Group.csv")))]
          (let [columns [:calendar-year :ay-group :mean :std-dev :iqr :min :low-95pc-bound :q1 :median :q3 :high-95pc-bound :max :low-ci :high-ci]]
            (->> (mapcat (fn [output year]
                           (map (fn [[ay-group stats]]
                                  (-> (medley/map-vals m/round stats)
                                      (assoc :ay-group ay-group :calendar-year year)))
                                (:total-in-send-by-ay-group output))) send-output (range initial-projection-year 3000))
                 (map (apply juxt columns))
                 (into [(mapv name columns)])
                 (csv/write-csv writer))))
        (output-beta-expectations dir "joiner_beta_expectations" (standard-projection :joiner-beta-params)
                                  [:ay :alpha :beta :expectation])
        (output-beta-expectations dir "mover_beta_expectations" (standard-projection :mover-beta-params))
        (output-beta-expectations dir "leaver_beta_expectations" (standard-projection :leaver-beta-params))
        (output-dirichlet-expectations dir "movers_dirichlet_expectations" (standard-projection :mover-state-alphas))
        (output-dirichlet-expectations dir "joiners_dirichlet_expectations" (standard-projection :joiner-state-alphas)
                                       [:ay :need :setting :expectation :normalisation-constant :alpha])
        (output-initial-population-state dir "initial_population_state" (standard-projection :population-by-state))
        (when scenario-projection
          (do
            (output-beta-expectations dir "scenario_joiner_beta_expectations" (scenario-projection :joiner-beta-params)
                                      [:ay :alpha :beta :expectation])
            (output-beta-expectations dir "scenario_mover_beta_expectations" (scenario-projection :mover-beta-params))
            (output-beta-expectations dir "scenario_leaver_beta_expectations" (scenario-projection :leaver-beta-params))
            (output-dirichlet-expectations dir "scenario_movers_dirichlet_expectations" (scenario-projection :mover-state-alphas))
            (output-dirichlet-expectations dir "scenario_joiners_dirichlet_expectations" (scenario-projection :joiner-state-alphas)
                                           [:ay :need :setting :expectation :normalisation-constant :alpha])
            (output-initial-population-state dir "scenario_initial_population_state" (standard-projection :population-by-state))))

        (when run-charts
          (with-open [writer (io/writer (io/file (str dir "/historic-data.csv")))]
            (let [columns [:calendar-year :setting-1 :need-1 :academic-year-1 :setting-2 :need-2]
                  headers (mapv name columns)
                  rows (mapv #(mapv % columns) transitions)]
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
            (io/copy in (io/file (System/getProperty "java.io.tmpdir") "send-charts.R")))
          (r-plots dir (str project-dir "/" population-file)
                   settings-to-exclude-in-charts
                   use-confidence-bound-or-interval)
          (when-not keep-temp-files?
            (run! #(io/delete-file (str dir "/" %) :quiet)
                  ["historic-data.csv" "valid-settings.csv" "joiner-rates.csv"
                   "leaver-rates.csv" "mover-rates.csv"])))))
    (report/write-send-report (str dir "/SEND_Log.md"))))
