(ns witan.send.send
  (:require [schema.core :as s]
            [witan.send.schemas :as sc]
            [clojure.core.matrix.dataset :as ds]
            [clojure.data.csv :as csv]
            [witan.datasets :as wds]
            [witan.datasets.stats :as wst]
            [witan.send.params :as p]
            [witan.send.step :as step]
            [witan.send.states :as states]
            [witan.send.utils :as u]
            [clojure.java.io :as io]
            [incanter.stats :as stats]
            [medley.core :as medley]
            [redux.core :as r]
            [clojure.string :as str]
            [clojure.java.shell :as sh]
            [witan.send.report :as report]
            [witan.send.check-inputs :refer [run-input-checks]]
            [clojure.walk :refer [postwalk]]
            [clojure.set :refer [rename-keys]]
            [witan.send.test-utils :as tu]
            [witan.send.report :refer [reset-send-report]])
  (:import [org.apache.commons.math3.distribution BetaDistribution]))

(defn initialise-model [send-data]
  (reduce (fn [coll {:keys [academic-year need setting population]}]
            (assoc coll [academic-year (states/state need setting)] population))
          {} send-data))

(defn incorporate-new-states-for-academic-year-state
  "Take a model + transitions tuple as its first argument.
  Returns a model + transitions tuple with `next-states-sample` incorporated."
  [[model transitions] academic-year state next-states-sample calendar-year]
  (vector
   (reduce (fn [coll [next-state n]]
             (cond-> coll
               (pos? n)
               (update [academic-year next-state] u/some+ n)))
           model next-states-sample)
   (reduce (fn [coll [next-state n]]
             (cond-> coll
               (pos? n)
               (update [calendar-year academic-year state next-state] u/some+ n)))
           transitions next-states-sample)))

(defn apply-leavers-movers-for-cohort-unsafe
  "We're calling this function 'unsafe' because it doesn't check whether the state or
  or academic year range is valid."
  [[model transitions] [[year state] population]
   {:keys [joiner-beta-params joiner-state-alphas
           leaver-beta-params mover-beta-params
           mover-state-alphas
           valid-year-settings] :as params}
   calendar-year]
  (if-let [probs (get mover-state-alphas [(dec year) state])]
    (let [leaver-params (get leaver-beta-params [(dec year) state])
          l (u/sample-beta-binomial population leaver-params)
          next-states-sample (if (states/can-move? valid-year-settings year state)
                               (let [mover-params (get mover-beta-params [(dec year) state])]
                                 (u/sample-send-transitions state (- population l) probs mover-params))
                               {state (- population l)})
          [model transitions] (incorporate-new-states-for-academic-year-state [model transitions] year state next-states-sample calendar-year)]
      [model
       (update transitions [calendar-year year state sc/non-send] u/some+ l)])
    [model transitions]))

(defn apply-leavers-movers-for-cohort
  "Take single cohort of users and process them into the model state.
  Calls 'unsafe' equivalent once we've removed non-send and children outside
  valid academic year range."
  [[model transitions :as model-state] [[year state] population :as cohort]
   {:keys [joiner-beta-params joiner-state-alphas
           leaver-beta-params
           mover-beta-params mover-state-alphas
           valid-year-settings] :as params}
   calendar-year]
  (cond
    (= state sc/non-send)
    model-state

    (or (<= year sc/min-academic-year)
        (> year sc/max-academic-year))
    [model
     (cond-> transitions
       (pos? population)
       (update [calendar-year year state sc/non-send] u/some+ population))]
    :else
    (apply-leavers-movers-for-cohort-unsafe model-state cohort params calendar-year)))

(defn apply-joiners-for-academic-year
  [[model transitions] academic-year population {:keys [joiner-beta-params joiner-state-alphas]} calendar-year]
  (let [betas (get joiner-beta-params academic-year)
        alphas (get joiner-state-alphas academic-year)
        pop (get population academic-year)]
    (if (and alphas betas pop (every? pos? (vals betas)))
      (let [joiners (u/sample-beta-binomial pop betas)]
        (if (zero? joiners)
          [model transitions]
          (let [joiner-states (u/sample-dirichlet-multinomial joiners alphas)]
            (incorporate-new-states-for-academic-year-state [model transitions] academic-year sc/non-send joiner-states calendar-year))))
      [model transitions])))

(defn run-model-iteration
  "Takes the model & transitions, transition params, and the projected population and produce the next state of the model & transitions"
  [modify-transitions-from
   simulation {:keys [joiner-beta-params joiner-state-alphas
                      leaver-beta-params
                      mover-beta-params mover-state-alphas
                      valid-year-settings] :as standard-projection}
   {:keys [modified-joiner-beta-params modified-joiner-state-alphas
           modified-leaver-beta-params modified-mover-beta-params
           modified-mover-state-alphas] :as scenario-projection}
   {:keys [model transitions]} [calendar-year projected-population]]
  (let [params (if (nil? modify-transitions-from)
                 (if ((complement nil?) scenario-projection)
                   scenario-projection
                   standard-projection)
                 (if (>= calendar-year modify-transitions-from)
                   scenario-projection
                   standard-projection))
        cohorts (step/age-population projected-population model)
        [model transitions] (reduce (fn [model-state cohort]
                                      (apply-leavers-movers-for-cohort model-state cohort params calendar-year))
                                    [{} {}]
                                    cohorts)
        [model transitions] (reduce (fn [model-state academic-year]
                                      (apply-joiners-for-academic-year model-state academic-year projected-population params calendar-year))
                                    [model transitions]
                                    sc/academic-years)]
    {:model model :transitions transitions}))

(defn stitch-ay-state-params
  [x a b]
  (reduce
   (fn [coll [[ay state] v]]
     (cond-> coll
       (>= ay x)
       (assoc [ay state] v)))
   (reduce (fn [coll [[ay state] v]]
             (cond-> coll
               (< ay x)
               (assoc [ay state] v)))
           {} a)
   b))

(defn stitch-ay-params
  [x a b]
  (reduce
   (fn [coll [ay v]]
     (cond-> coll
       (>= ay x)
       (assoc ay v)))
   (reduce (fn [coll [ay v]]
             (cond-> coll
               (< ay x)
               (assoc ay v)))
           {} a)
   b))

(defn generate-transition-key [{:keys [transition-type cy ay need setting move-state]}]
  (when (not= move-state (states/state need setting))
    (case transition-type
      "joiners"
      (vector cy ay :NONSEND (states/state need setting))

      "leavers"
      (vector cy ay (states/state need setting) :NONSEND)

      "movers-to"
      (vector cy ay move-state (states/state need setting))

      "movers-from"
      (vector cy ay (states/state need setting) move-state))))

(defn update-ifelse-assoc [m k arithmetic-fn v]
  (if (contains? m k)
    (update m k #(arithmetic-fn % v))
    (assoc m k v)))

(defn modify-transitions [transitions [first-state second-state] arithmetic-fn v]
  (if (contains? transitions first-state)
    (let [pop (get transitions first-state)
          mod-pop (u/int-round (arithmetic-fn pop v))
          diff (- pop mod-pop)
          assoc-first-state (assoc transitions first-state mod-pop)]
      (if (nil? second-state)
        assoc-first-state
        (update-ifelse-assoc assoc-first-state second-state + diff)))
    transitions))

(defn prep-inputs [initial-state splice-ncy valid-states valid-transitions transition-matrix
                   transition-matrix-filtered population valid-setting-academic-years
                   original-transitions setting-cost filter-transitions-from]
  (let [start-map {:population-by-age-state initial-state
                   :valid-setting-academic-years valid-setting-academic-years
                   :transition-matrix original-transitions
                   :population population
                   :setting-cost-lookup (->> (ds/row-maps setting-cost)
                                             (map (juxt (juxt :need :setting) :cost))
                                             (into {}))
                   :projected-population (->> (ds/row-maps population)
                                              (group-by :calendar-year)
                                              (medley/map-vals #(u/total-by-academic-year %)))}]
    (if transition-matrix-filtered
      (merge start-map
             {:joiner-beta-params (stitch-ay-params splice-ncy
                                                    (p/beta-params-joiners valid-states
                                                                           transition-matrix
                                                                           (ds/row-maps population))
                                                    (p/beta-params-joiners valid-states
                                                                           transition-matrix-filtered
                                                                           (ds/row-maps population)))
              :leaver-beta-params (stitch-ay-state-params splice-ncy
                                                          (p/beta-params-leavers valid-states transition-matrix)
                                                          (p/beta-params-leavers valid-states transition-matrix-filtered))
              :joiner-state-alphas (stitch-ay-params splice-ncy
                                                     (p/alpha-params-joiner-states valid-states (u/transitions-map transition-matrix))
                                                     (p/alpha-params-joiner-states valid-states (u/transitions-map transition-matrix-filtered)))

              :mover-beta-params (stitch-ay-state-params splice-ncy
                                                         (p/beta-params-movers valid-states valid-transitions transition-matrix)
                                                         (p/beta-params-movers valid-states valid-transitions transition-matrix-filtered))
              :mover-state-alphas (stitch-ay-state-params splice-ncy
                                                          (p/alpha-params-movers valid-states valid-transitions transition-matrix)
                                                          (p/alpha-params-movers valid-states valid-transitions transition-matrix-filtered))})
      (merge start-map
             {:joiner-beta-params (p/beta-params-joiners valid-states
                                                         transition-matrix
                                                         (ds/row-maps population))
              :leaver-beta-params (p/beta-params-leavers valid-states transition-matrix)
              :joiner-state-alphas (p/alpha-params-joiner-states valid-states (u/transitions-map transition-matrix))
              :mover-beta-params (p/beta-params-movers valid-states valid-transitions transition-matrix)
              :mover-state-alphas  (p/alpha-params-movers valid-states valid-transitions transition-matrix)}))))

(defn build-states-to-change [input valid-needs valid-settings ages years transition-type]
  (let [to-maps (ds/row-maps input)
        settings-to-change (if (= :nil (-> to-maps
                                           first
                                           :setting-2))
                             (map #(vector (:setting-1 %)) to-maps)
                             (map #(vector (:setting-1 %) (:setting-2 %)) to-maps))]
    (->> (for [year years
               age ages
               need valid-needs
               setting valid-settings
               setting-to-change settings-to-change]
           (let [keys {:transition-type transition-type :cy year :ay age
                       :need need :move-state (states/state need setting)}]
             (if (= :nil (-> to-maps
                             first
                             :setting-2))
               (vector (generate-transition-key (merge keys {:setting (first setting-to-change)})))
               (vector (generate-transition-key (merge keys {:setting (first setting-to-change)}))
                       (generate-transition-key (merge keys {:setting (second setting-to-change)}))))))
         (remove #(or (nil? (second %)) (nil? (first %))))
         distinct)))

(defn build-input-datasets
  "Build a map of the datasets to use for input"
  [project-dir file-inputs schema-inputs]
  (into {} (for [[k v] file-inputs]
             [k (tu/csv-to-dataset (str project-dir "/" v) (k schema-inputs))])))

(defn prepare-send-inputs
  "Outputs the population for the last year of historic data, with one
   row for each individual/year/simulation. Also includes age & state columns"
  [{:keys [settings-to-change transition-matrix population
           setting-cost valid-setting-academic-years]}
   {:keys [which-transitions? modify-transition-by splice-ncy filter-transitions-from]}]
  (run-input-checks (ds/row-maps transition-matrix)
                    (ds/row-maps setting-cost)
                    (ds/row-maps valid-setting-academic-years))
  (let [original-transitions transition-matrix
        transition-matrix (ds/row-maps transition-matrix)
        ages (distinct (map :academic-year (ds/row-maps population)))
        years (distinct (map :calendar-year transition-matrix))
        initialise-validation (ds/row-maps valid-setting-academic-years)
        valid-transitions (states/calculate-valid-mover-transitions
                           initialise-validation)
        valid-needs (states/calculate-valid-needs-from-setting-academic-years
                     initialise-validation)
        valid-settings (states/calculate-valid-settings-from-setting-academic-years
                        initialise-validation)
        valid-states (states/calculate-valid-states-from-setting-academic-years
                      initialise-validation)
        valid-year-settings (states/calculate-valid-year-settings-from-setting-academic-years
                             initialise-validation)
        states-to-change (when (not= 1 modify-transition-by)
                           (mapcat (fn [transition-type] (build-states-to-change settings-to-change valid-needs valid-settings ages years transition-type)) which-transitions?))
        modified-transition-matrix (when (not= 1 modify-transition-by)
                                     (let [result (u/movers-to-transitions-map transition-matrix states-to-change)]
                                       (mapcat (fn [[k v]] (u/back-to-transitions-matrix k v)) result)))
        transitions (if modified-transition-matrix
                      (u/transitions-map modified-transition-matrix)
                      (u/transitions-map transition-matrix))
        transition-matrix-filtered (when filter-transitions-from
                                     (mapcat (fn [year] (filter #(= (:calendar-year %) year)
                                                                (or modified-transition-matrix transition-matrix)))
                                             filter-transitions-from))
        max-transition-year (apply max (map :calendar-year transition-matrix))
        initial-state (->> (filter #(= (:calendar-year %) max-transition-year) transition-matrix)
                           (filter #(not= (:setting-2 %) :NONSEND))
                           (postwalk #(if (map? %) (dissoc % :calendar-year :setting-1 :need-1 :academic-year-1) %))
                           (frequencies)
                           (map #(assoc (first %) :population (last %) :calendar-year (inc max-transition-year)))
                           (map #(rename-keys % {:setting-2 :setting, :need-2 :need :academic-year-2 :academic-year}))
                           (initialise-model))]
    (when (not= 1 modify-transition-by)
      (report/info "\nModified transitions by " (report/bold modify-transition-by)))
    (if modified-transition-matrix
      (report/info "\nUsed " (report/bold "modified") " transitions matrix\n")
      (report/info "\nUsed " (report/bold "input") " transitions matrix\n"))
    (s/validate (sc/TransitionsMap+ valid-needs valid-settings) transitions)
    (s/validate (sc/NeedSettingCost+ valid-needs valid-settings) setting-cost)
    {:standard-projection (prep-inputs initial-state splice-ncy
                                       valid-states valid-transitions transition-matrix
                                       transition-matrix-filtered
                                       population valid-setting-academic-years original-transitions setting-cost
                                       filter-transitions-from)
     :scenario-projection (when modified-transition-matrix
                            (prep-inputs initial-state splice-ncy valid-states
                                         valid-transitions modified-transition-matrix
                                         transition-matrix-filtered population valid-setting-academic-years
                                         original-transitions setting-cost filter-transitions-from))
     :modify-transition-by modify-transition-by
     :settings-to-change settings-to-change}))

(defn projection->transitions
  [projections]
  (apply merge-with + (mapcat #(map :transitions %) projections)))

(defn output-transitions [file projections]
  (binding [*print-length* nil] (spit file (pr-str projections))))

(defn values-rf
  "Associate a reducing function to be used for each value of map indexed by key"
  [kvs]
  (->> (for [[k v] kvs]
         [k (r/pre-step v k)])
       (into {})
       (r/fuse)))

(def number-of-significant-digits 3)

(defn reduce-rf [iterations valid-states setting-cost-lookup]
  (u/partition-rf iterations
                  (r/fuse {:by-state (u/model-states-rf valid-states (u/histogram-rf number-of-significant-digits))
                           :total-in-send-by-ay (r/pre-step (u/with-keys-rf (u/histogram-rf number-of-significant-digits) sc/academic-years) u/model-population-by-ay)
                           :total-in-send (r/pre-step (u/histogram-rf number-of-significant-digits) u/model-send-population)
                           :total-in-send-by-need (r/pre-step (u/merge-with-rf (u/histogram-rf number-of-significant-digits)) u/model-population-by-need)
                           :total-in-send-by-setting (r/pre-step (u/merge-with-rf (u/histogram-rf number-of-significant-digits)) u/model-population-by-setting)
                           :total-cost (r/pre-step (u/histogram-rf number-of-significant-digits) (comp (partial u/total-need-setting-cost setting-cost-lookup)
                                                                                                       u/model-population-by-need-setting))
                           :total-in-send-by-ay-group (r/pre-step (u/merge-with-rf (u/histogram-rf number-of-significant-digits))
                                                                  u/model-population-by-ay-group)})))

(defn combine-rf [iterations]
  (u/partition-rf iterations
                  (values-rf {:by-state (u/merge-with-rf (u/histogram-combiner-rf number-of-significant-digits))
                              :total-in-send-by-ay (u/merge-with-rf (u/histogram-combiner-rf number-of-significant-digits))
                              :total-in-send (u/histogram-combiner-rf number-of-significant-digits)
                              :total-in-send-by-need (u/merge-with-rf (u/histogram-combiner-rf number-of-significant-digits))
                              :total-in-send-by-setting (u/merge-with-rf (u/histogram-combiner-rf number-of-significant-digits))
                              :total-cost (u/histogram-combiner-rf number-of-significant-digits)
                              :total-in-send-by-ay-group (u/merge-with-rf (u/histogram-combiner-rf number-of-significant-digits))})))

(defn run-send-model
  "Outputs the population for the last year of historic data, with one
   row for each individual/year/simulation. Also includes age & state columns"
  [{:keys [standard-projection scenario-projection modify-transition-by settings-to-change]}
   {:keys [seed-year random-seed simulations modify-transitions-from]}]
  (u/set-seed! random-seed)
  (let [{:keys [population population-by-age-state projected-population joiner-beta-params
                joiner-state-alphas leaver-beta-params mover-beta-params mover-state-alphas
                setting-cost-lookup valid-setting-academic-years
                transition-matrix] :as inputs} standard-projection
        modified-inputs (when ((complement nil?) scenario-projection)
                          (assoc scenario-projection :valid-year-settings
                                 (->> (ds/row-maps valid-setting-academic-years)
                                      (states/calculate-valid-year-settings-from-setting-academic-years))))
        projected-future-pop-by-year (->> projected-population
                                          (filter (fn [[k _]] (> k seed-year)))
                                          (sort-by key))
        iterations (inc (count projected-future-pop-by-year)) ;; include current year
        valid-states (->> (ds/row-maps valid-setting-academic-years)
                          (states/calculate-valid-states-from-setting-academic-years))
        inputs (assoc inputs :valid-year-settings (->> (ds/row-maps valid-setting-academic-years)
                                                       (states/calculate-valid-year-settings-from-setting-academic-years)))
        projections (->> (range simulations)
                         (partition-all (int (/ simulations 8)))
                         (pmap (fn [simulations]
                                 (->> (for [simulation simulations]
                                        (let [projection (reductions (partial run-model-iteration modify-transitions-from simulation inputs modified-inputs)
                                                                     {:model population-by-age-state
                                                                      :transitions {}}
                                                                     projected-future-pop-by-year)]
                                          (println (format "Created projection %d" simulation))
                                          projection))
                                      (doall))))
                         (doall))
        reduced (doall
                 (for [projection projections]
                   (do (println "Reducing...")
                       (transduce (map #(map :model %)) (reduce-rf iterations valid-states setting-cost-lookup) projection))))
        projection (apply concat projections)]
    (println "Combining...")
    {:projection (projection->transitions projection)
     :send-output (transduce identity (combine-rf iterations) reduced)
     :transition-matrix transition-matrix
     :valid-setting-academic-years valid-setting-academic-years
     :population population
     :modify-transition-by modify-transition-by
     :settings-to-change settings-to-change}))

(defn run-send-workflow
  "Run the send model, the function expects a map as seen in
  data/demo/config.edn (typically use `(config \"data/demo\")` to
  generate it)"
  ([config]
   (reset-send-report)
   (-> (build-input-datasets (:project-dir config) (:file-inputs config) (:schema-inputs config))
       (prepare-send-inputs (:transition-parameters config))
       (run-send-model (:run-parameters config)))))


(defn joiner-rate [joiners population ages years]
  (reduce (fn [coll academic-year]
            (reduce (fn [collection calendar-year]
                      (let [j (get-in joiners [calendar-year academic-year] 0)]
                        (assoc-in collection [academic-year calendar-year]
                                  {:alpha j
                                   :beta (- (get-in population [calendar-year academic-year]) j)})))
                    coll years)) {} ages))

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

(defn confidence-interval
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

(defn transition-present? [transition projection]
  (some #(= % transition) projection))

(defn pull-year
  [data pos]
  (->> (nth data pos)
       (map (fn [[a b c]] (if (zero? b) [a :NA :NA] [a b c])))
       (apply mapv vector)))

(defn create-keys [string year-count]
  (map (fn [n] (keyword (str string n))) (range year-count)))

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

(defn ribbon-data-rows [ribbon-data]
  (mapv (fn [x] (mapv #(nth % x) (map val ribbon-data))) (range (count (val (last ribbon-data))))))

(defn output-send-results
  "Groups the individual data from the loop to get a demand projection, and applies the cost profile
   to get the total cost."
  [{:keys [projection send-output transition-matrix valid-setting-academic-years
           population modify-transition-by settings-to-change]}
   {:keys [run-reports run-charts project-dir output-dir]}]
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
    (when run-reports
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
            joiner-rates-CI (map #(confidence-interval joiner-rates %) years)
            joiner-ribbon-data (prep-ribbon-plot-data joiner-rates-CI years n-colours)
            filter-leavers (remove (fn [{:keys [setting-1]}] (= setting-1 sc/non-send)) transitions-data)
            leaver-rates (leaver-rate filter-leavers)
            leaver-rates-CI (map #(confidence-interval leaver-rates %) years)
            leaver-ribbon-data (prep-ribbon-plot-data leaver-rates-CI years n-colours)
            filter-movers (remove (fn [{:keys [setting-1 setting-2]}]
                                    (or (= setting-1 sc/non-send)
                                        (= setting-2 sc/non-send))) transitions-data)
            mover-rates (mover-rate filter-movers)
            mover-rates-CI (map #(confidence-interval mover-rates %) years)
            mover-ribbon-data (prep-ribbon-plot-data mover-rates-CI years n-colours)]
        (report/info "First year of input data: " (report/bold (first years)))
        (report/info "Final year of input data: " (report/bold (inc (last years))))
        (report/info "Final year of projection: " (report/bold (+ (last years) (count (map :total-in-send send-output)))))
        (output-transitions (str dir "/transitions.edn") projection)
        (with-open [writer (io/writer (io/file (str dir "/Output_AY_State.csv")))]
          (let [columns [:calendar-year :academic-year :state :mean :std-dev :iqr :min :low-ci :q1 :median :q3 :high-ci :max]]
            (->> (mapcat (fn [output year]
                           (map (fn [[[academic-year state] stats]]
                                  (-> (medley/map-vals u/round stats)
                                      (assoc :academic-year academic-year :state state :calendar-year year))) (:by-state output))) send-output (range initial-projection-year 3000))
                 (map (apply juxt columns))
                 (concat [(map name columns)])
                 (csv/write-csv writer))))
        (with-open [writer (io/writer (io/file (str dir "/Output_AY.csv")))]
          (let [columns [:calendar-year :academic-year :mean :std-dev :iqr :min :low-ci :q1 :median :q3 :high-ci :max]]
            (->> (mapcat (fn [output year]
                           (map (fn [[academic-year stats]]
                                  (-> (medley/map-vals u/round stats)
                                      (assoc :academic-year academic-year :calendar-year year)))
                                (:total-in-send-by-ay output))) send-output (range initial-projection-year 3000))
                 (map (apply juxt columns))
                 (concat [(map name columns)])
                 (csv/write-csv writer))))
        (with-open [writer (io/writer (io/file (str dir "/Output_Need.csv")))]
          (let [columns [:calendar-year :need :mean :std-dev :iqr :min :low-ci :q1 :median :q3 :high-ci :max]]
            (->> (mapcat (fn [output year]
                           (map (fn [[need stats]]
                                  (-> (medley/map-vals u/round stats)
                                      (assoc :need (name need) :calendar-year year)))
                                (:total-in-send-by-need output))) send-output (range initial-projection-year 3000))
                 (map (apply juxt columns))
                 (concat [(map name columns)])
                 (csv/write-csv writer))))
        (with-open [writer (io/writer (io/file (str dir "/Output_Setting.csv")))]
          (let [columns [:calendar-year :setting :mean :std-dev :iqr :min :low-ci :q1 :median :q3 :high-ci :max]]
            (->> (mapcat (fn [output year]
                           (map (fn [[setting stats]]
                                  (-> (medley/map-vals u/round stats)
                                      (assoc :setting (name setting) :calendar-year year)))
                                (:total-in-send-by-setting output))) send-output (range initial-projection-year 3000))
                 (map (apply juxt columns))
                 (concat [(map name columns)])
                 (csv/write-csv writer))))
        (with-open [writer (io/writer (io/file (str dir "/Output_Count.csv")))]
          (let [columns [:calendar-year :mean :std-dev :iqr :min :low-ci :q1 :median :q3 :high-ci :max]]
            (->> (map (fn [stats year]
                        (-> (medley/map-vals u/round stats)
                            (assoc :calendar-year year)))
                      (map :total-in-send send-output) (range initial-projection-year 3000))
                 (map (apply juxt columns))
                 (concat [(map name columns)])
                 (csv/write-csv writer))))
        (with-open [writer (io/writer (io/file (str dir "/Output_Cost.csv")))]
          (let [columns [:calendar-year :mean :std-dev :iqr :min :low-ci :q1 :median :q3 :high-ci :max]]
            (->> (map (fn [stats year]
                        (-> (medley/map-vals u/round stats)
                            (assoc :calendar-year year)))
                      (map :total-cost send-output) (range initial-projection-year 3000))
                 (map (apply juxt columns))
                 (concat [(map name columns)])
                 (csv/write-csv writer))))
        (with-open [writer (io/writer (io/file (str dir "/Output_AY_Group.csv")))]
          (let [columns [:calendar-year :ay-group :mean :std-dev :iqr :min :low-ci :q1 :median :q3 :high-ci :max]]
            (->> (mapcat (fn [output year]
                           (map (fn [[ay-group stats]]
                                  (-> (medley/map-vals u/round stats)
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
          (sh/sh "Rscript" "--vanilla" "/tmp/send-charts.R" dir)
          (run! #(io/delete-file (str dir "/" %) :quiet)
                ["historic-data.csv" "valid-settings.csv" "joiner-rates.csv"
                 "leaver-rates.csv" "mover-rates.csv"]))))
    (report/write-send-report (str dir "/SEND_Log.md"))))
