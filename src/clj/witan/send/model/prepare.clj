(ns witan.send.model.prepare
  (:require [clojure.core.matrix.dataset :as ds]
            [clojure.string :as str]
            [witan.send.maths :as m]
            [witan.send.states :as states]
            [medley.core :as medley]
            [witan.send.params :as p]
            [witan.send.check-inputs :refer [run-input-checks]]
            [clojure.set :refer [rename-keys]]
            [clojure.walk :refer [postwalk]]
            [witan.send.report :as report]
            [schema.core :as s]
            [witan.send.schemas :as sc]))

(defn transitions-map
  [dataset]
  (->> dataset
       (reduce (fn [coll {:keys [setting-1 need-1 setting-2 need-2 academic-year-2]}]
                 (let [state-1 (states/state need-1 setting-1)
                       state-2 (states/state need-2 setting-2)]
                   (update coll [academic-year-2 state-1 state-2] m/some+ 1)))
               {})))

(defn full-transitions-map
  [dataset]
  (->> dataset
       (reduce (fn [coll {:keys [calendar-year setting-1 need-1 setting-2 need-2 academic-year-2]}]
                 (let [state-1 (states/state need-1 setting-1)
                       state-2 (states/state need-2 setting-2)]
                   (update coll [calendar-year academic-year-2 state-1 state-2] m/some+ 1)))
               {})))

(defn split-need-state [state pos]
  (keyword (pos (str/split (name state) #"-"))))

(defn back-to-transitions-matrix [k v]
  (let [[calendar-year academic-year-2 state-1 state-2] k
        total v]
    (repeat total (assoc {}
                         :calendar-year calendar-year
                         :academic-year-1 (- academic-year-2 1)
                         :academic-year-2 academic-year-2
                         :need-1 (split-need-state state-1 first)
                         :setting-1 (if (nil? (split-need-state state-1 second))
                                      (split-need-state state-1 first)
                                      (split-need-state state-1 second))
                         :need-2 (split-need-state state-2 first)
                         :setting-2 (if (nil? (split-need-state state-2 second))
                                      (split-need-state state-2 first)
                                      (split-need-state state-2 second))))))

(def total-by-academic-year
  "Given a sequence of {:academic-year year :population population}
  sums the total population for each year"
  (partial reduce (fn [coll {:keys [academic-year population]}]
                    (update coll academic-year m/some+ population))
           {}))

(defn int-ceil [n]
  (int (Math/ceil n)))

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
                                              (medley/map-vals #(total-by-academic-year %)))}]
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
                                                     (p/alpha-params-joiner-states valid-states (transitions-map transition-matrix))
                                                     (p/alpha-params-joiner-states valid-states (transitions-map transition-matrix-filtered)))

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
              :joiner-state-alphas (p/alpha-params-joiner-states valid-states (transitions-map transition-matrix))
              :mover-beta-params (p/beta-params-movers valid-states valid-transitions transition-matrix)
              :mover-state-alphas  (p/alpha-params-movers valid-states valid-transitions transition-matrix)}))))

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
         (remove #(nil? (first %)))
         distinct)))

(defn update-ifelse-assoc [m k arithmetic-fn v]
  (if (contains? m k)
    (update m k #(arithmetic-fn % v))
    (assoc m k v)))

(defn modify-transitions [transitions [first-state second-state] arithmetic-fn v]
  (if (contains? transitions first-state)
    (let [pop (get transitions first-state)
          mod-pop (int-ceil (arithmetic-fn pop v))
          diff (- pop mod-pop)
          assoc-first-state (assoc transitions first-state mod-pop)]
      (if (nil? second-state)
        assoc-first-state
        (update-ifelse-assoc assoc-first-state second-state + diff)))
    transitions))

(defn initialise-model [send-data]
  (reduce (fn [coll {:keys [academic-year need setting population]}]
            (assoc coll [academic-year (states/state need setting)] population))
          {} send-data))

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
        ages (distinct (map :academic-year (ds/row-maps population)))
        years (distinct (map :calendar-year (ds/row-maps population)))
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
                           (mapcat (fn [transition-type]
                                     (build-states-to-change settings-to-change
                                                             valid-needs valid-settings
                                                             ages years transition-type))
                                   which-transitions?))
        transition-matrix (ds/row-maps transition-matrix)
        modified-transition-matrix (when (not= 1 modify-transition-by)
                                     (let [convert (-> transition-matrix
                                                       full-transitions-map)
                                           result (reduce (fn [m k] (modify-transitions m k * modify-transition-by))
                                                          convert states-to-change)]
                                       (mapcat (fn [[k v]] (back-to-transitions-matrix k v)) result)))
        transitions (if modified-transition-matrix
                      (transitions-map modified-transition-matrix)
                      (transitions-map transition-matrix))
        transition-matrix-filtered (when filter-transitions-from
                                     (mapcat (fn [year] (filter #(= (:calendar-year %) year)
                                                                (or modified-transition-matrix transition-matrix)))
                                             [filter-transitions-from]))
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
