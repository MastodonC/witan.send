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
                 (let [need-setting-1 (states/join-need-setting need-1 setting-1)
                       need-setting-2 (states/join-need-setting need-2 setting-2)]
                   (update coll [academic-year-2 need-setting-1 need-setting-2] m/some+ 1)))
               {})))

(defn full-transitions-map
  [dataset]
  (->> dataset
       (reduce (fn [coll {:keys [calendar-year setting-1 need-1 setting-2 need-2 academic-year-2]}]
                 (let [need-setting-1 (states/join-need-setting need-1 setting-1)
                       need-setting-2 (states/join-need-setting need-2 setting-2)]
                   (update coll [calendar-year academic-year-2 need-setting-1 need-setting-2] m/some+ 1)))
               {})))

(defn split-need-state [state pos]
  (keyword (pos (str/split (name state) #"-"))))

(defn back-to-transitions [k v]
  (let [[calendar-year academic-year-2 need-setting-1 need-setting-2] k
        total v]
    (repeat total (assoc {}
                         :calendar-year calendar-year
                         :academic-year-1 (- academic-year-2 1)
                         :academic-year-2 academic-year-2
                         :need-1 (split-need-state need-setting-1 first)
                         :setting-1 (if (nil? (split-need-state need-setting-1 second))
                                      (split-need-state need-setting-1 first)
                                      (split-need-state need-setting-1 second))
                         :need-2 (split-need-state need-setting-2 first)
                         :setting-2 (if (nil? (split-need-state need-setting-2 second))
                                      (split-need-state need-setting-2 first)
                                      (split-need-state need-setting-2 second))))))

(def total-by-academic-year
  "Given a sequence of {:academic-year year :population population}
  sums the total population for each year"
  (partial reduce (fn [coll {:keys [academic-year population]}]
                    (update coll academic-year m/some+ population))
           {}))

(defn int-ceil [n]
  (int (Math/ceil n)))

(defn stitch-state-params
  [x a b]
  (reduce
   (fn [coll [[ay need-setting] v]]
     (cond-> coll
       (>= ay x)
       (assoc [ay need-setting] v)))
   (reduce (fn [coll [[ay need-setting] v]]
             (cond-> coll
               (< ay x)
               (assoc [ay need-setting] v)))
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

(defn prep-inputs [initial-send-pop validate-valid-states valid-transitions transitions
                   transitions-filtered population valid-states original-transitions costs]
  (let [start-map {:population-by-state initial-send-pop
                   :valid-states valid-states
                   :transitions original-transitions
                   :population population
                   :cost-lookup (->> (ds/row-maps costs)
                                     (map (juxt (juxt :need :setting) :cost))
                                     (into {}))
                   :projected-population (->> (ds/row-maps population)
                                              (group-by :calendar-year)
                                              (medley/map-vals #(total-by-academic-year %)))}]
    (if transitions-filtered
      (merge start-map
             {:joiner-beta-params (p/beta-params-joiners validate-valid-states
                                                         transitions-filtered
                                                         (ds/row-maps population))
              :leaver-beta-params (p/beta-params-leavers validate-valid-states transitions-filtered)
              :joiner-state-alphas (p/alpha-params-joiners validate-valid-states (transitions-map transitions-filtered))
              :mover-beta-params (p/beta-params-movers validate-valid-states valid-transitions transitions-filtered)
              :mover-state-alphas (p/alpha-params-movers validate-valid-states valid-transitions transitions-filtered)})
      (merge start-map
             {:joiner-beta-params (p/beta-params-joiners validate-valid-states
                                                         transitions
                                                         (ds/row-maps population))
              :leaver-beta-params (p/beta-params-leavers validate-valid-states transitions)
              :joiner-state-alphas (p/alpha-params-joiners validate-valid-states (transitions-map transitions))
              :mover-beta-params (p/beta-params-movers validate-valid-states valid-transitions transitions)
              :mover-state-alphas  (p/alpha-params-movers validate-valid-states valid-transitions transitions)}))))

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
            (assoc coll [academic-year (states/join-need-setting need setting)] population))
          {} send-data))

(def operators {:= =
                :< <
                :<= <=
                :> >
                :>= >=})

(defn remove-transitions-xf [remove-operation]
  "Expects a key-value pair like [:calendar-academic {:< 2017, :>= 12}].

   Each key much conform to :calendar-academic, :calendar-setting, :calendar-need, :academic-setting,
   :academic-need or :setting-need.

   The inner map must contain two key-value pairs, the key corresponding to an operator and the value
   corresponding to a value to filter on"
  (let [k (first remove-operation)
        v (second remove-operation)
        op1 ((first (keys v)) operators)
        op2 ((second (keys v)) operators)
        val1 (first (vals v))
        val2 (second (vals v))]
    (cond
      (= k :calendar-academic) (remove #(or (and (op1 (:calendar-year %) val1)
                                                 (op2 (:academic-year-1 %) val2))
                                            (and (op1 (+ 1 (:calendar-year %)) val1)
                                                 (op2 (:academic-year-2 %) val2))))
      (= k :calendar-setting) (remove #(or (and (op1 (:calendar-year %) val1)
                                                (op2 (:setting-1 %) val2))
                                           (and (op1 (+ 1 (:calendar-year %)) val1)
                                                (op2 (:setting-2 %) val2))))
      (= k :calendar-need) (remove #(or (and (op1 (:calendar-year %) val1)
                                             (op2 (:need-1 %) val2))
                                        (and (op1 (+ 1 (:calendar-year %)) val1)
                                             (op2 (:need-2 %) val2))))
      (= k :academic-setting) (remove #(or (and (op1 (:academic-year-1 %) val1)
                                                (op2 (:setting-1 %) val2))
                                           (and (op1 (:academic-year-2 %) val1)
                                                (op2 (:setting-2 %) val2))))
      (= k :academic-need) (remove #(or (and (op1 (:academic-year-1 %) val1)
                                             (op2 (:need-1 %) val2))
                                        (and (op1 (:academic-year-2 %) val1)
                                             (op2 (:need-2 %) val2))))
      (= k :setting-need) (remove #(or (and (op1 (:setting-1 %) val1)
                                            (op2 (:need-1 %) val2))
                                       (and (op1 (:setting-2 %) val1)
                                            (op2 (:need-2 %) val2)))))))

(defn test-predicates [data pred-map]
  (map (fn [[k v]] (= (k data) v)) pred-map))

(defn prepare-send-inputs
  "Outputs the population for the last year of historic data, with one
   row for each individual/year/simulation. Also includes age & state columns"
  [{:keys [transitions population
           costs valid-states]}
   {:keys [which-transitions? transitions-to-change
           filter-transitions-from]}]
  (run-input-checks (ds/row-maps transitions)
                    (ds/row-maps costs)
                    (ds/row-maps valid-states))
  (let  [original-transitions transitions
         ages (distinct (map :academic-year (ds/row-maps population)))
         years (distinct (map :calendar-year (ds/row-maps population)))
         initialise-validation (ds/row-maps valid-states)
         valid-transitions (states/calculate-valid-mover-transitions
                            initialise-validation)
         valid-needs (states/calculate-valid-needs-from-setting-academic-years
                      initialise-validation)
         valid-settings (states/calculate-valid-settings-from-setting-academic-years
                         initialise-validation)
         validate-valid-states (states/calculate-valid-states-from-setting-academic-years
                                initialise-validation)
         valid-year-settings (states/calculate-valid-year-settings-from-setting-academic-years
                              initialise-validation)
         transitions (ds/row-maps transitions)
         modified-transitions (when transitions-to-change
                                (println "Using modified transition rates")
                                (let [change (mapcat
                                              #(let [pred-map (dissoc % :modify-transition-by)]
                                                 (->> transitions
                                                      (filter (fn [t] (every? identity (test-predicates t pred-map))))
                                                      full-transitions-map
                                                      (map (fn [[k v]] [k (* v (:modify-transition-by %))]))))
                                              transitions-to-change)
                                      no-change (-> (reduce (fn [x s] (let [pred-map (dissoc s :modify-transition-by)]
                                                                        (remove (fn [t] (every? identity (test-predicates t pred-map))) x)))
                                                            transitions transitions-to-change)
                                                    full-transitions-map)]
                                  (mapcat (fn [[k v]] (back-to-transitions k v)) (concat change no-change))))
         map-of-transitions (if modified-transitions
                              (transitions-map modified-transitions)
                              (transitions-map transitions))
         transitions-filtered (when filter-transitions-from
                                (reduce #(sequence (remove-transitions-xf %2) %1) (or modified-transitions transitions) filter-transitions-from))
         max-transition-year (apply max (map :calendar-year transitions))
         initial-send-pop (->> (filter #(= (:calendar-year %) max-transition-year) transitions)
                               (filter #(not= (:setting-2 %) :NONSEND))
                               (postwalk #(if (map? %) (dissoc % :calendar-year :setting-1 :need-1 :academic-year-1) %))
                               (frequencies)
                               (map #(assoc (first %) :population (last %) :calendar-year (inc max-transition-year)))
                               (map #(rename-keys % {:setting-2 :setting, :need-2 :need :academic-year-2 :academic-year}))
                               (initialise-model))]
    (if modified-transitions
      (report/info "\nUsed " (report/bold "modified") " transitions matrix\n")
      (report/info "\nUsed " (report/bold "input") " transitions matrix\n"))
    (s/validate (sc/TransitionsMap+ valid-needs valid-settings) map-of-transitions)
    (s/validate (sc/NeedSettingCost+ valid-needs valid-settings) costs)
    {:standard-projection (prep-inputs initial-send-pop validate-valid-states
                                       valid-transitions transitions
                                       transitions-filtered population valid-states
                                       original-transitions costs)
     :scenario-projection (when modified-transitions
                            (prep-inputs initial-send-pop validate-valid-states
                                         valid-transitions modified-transitions
                                         transitions-filtered population valid-states
                                         original-transitions costs))
     :seed-year (inc max-transition-year)}))
