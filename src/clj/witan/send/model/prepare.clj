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

(defn prep-inputs [initial-send-pop validate-valid-states valid-transitions transitions
                   population valid-states original-transitions costs]
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
    (merge start-map
           {:joiner-beta-params  (p/beta-params-joiners validate-valid-states
                                                        transitions
                                                        (ds/row-maps population))
            :leaver-beta-params  (p/beta-params-leavers validate-valid-states transitions)
            :joiner-state-alphas (p/alpha-params-joiners validate-valid-states (transitions-map transitions))
            :mover-beta-params   (p/beta-params-movers validate-valid-states valid-transitions transitions)
            :mover-state-alphas  (p/alpha-params-movers validate-valid-states valid-transitions transitions)})))

(defn initialise-model [send-data]
  (reduce (fn [coll {:keys [academic-year need setting population]}]
            (assoc coll [academic-year (states/join-need-setting need setting)] population))
          {} send-data))

(defn prepare-send-inputs
  "Outputs the population for the last year of historic data, with one
   row for each individual/year/simulation. Also includes age & state columns"
  [{:keys [transitions population
           costs valid-states]}]
  (run-input-checks (ds/row-maps transitions)
                    (ds/row-maps costs)
                    (ds/row-maps valid-states))
  (let  [original-transitions transitions
         initialise-validation (ds/row-maps valid-states)
         valid-transitions (states/calculate-valid-mover-transitions
                            initialise-validation)
         valid-needs (states/calculate-valid-needs-from-setting-academic-years
                      initialise-validation)
         valid-settings (states/calculate-valid-settings-from-setting-academic-years
                         initialise-validation)
         validate-valid-states (states/calculate-valid-states-from-setting-academic-years
                                initialise-validation)
         transitions (ds/row-maps transitions)
         map-of-transitions (transitions-map transitions)
         max-transition-year (apply max (map :calendar-year transitions))
         initial-send-pop (->> (filter #(= (:calendar-year %) max-transition-year) transitions)
                               (filter #(not= (:setting-2 %) :NONSEND))
                               (postwalk #(if (map? %) (dissoc % :calendar-year :setting-1 :need-1 :academic-year-1) %))
                               (frequencies)
                               (map #(assoc (first %) :population (last %) :calendar-year (inc max-transition-year)))
                               (map #(rename-keys % {:setting-2 :setting, :need-2 :need :academic-year-2 :academic-year}))
                               (initialise-model))]
    (report/info "\nUsed " (report/bold "input") " transitions matrix\n")
    (s/validate (sc/TransitionsMap+ valid-needs valid-settings) map-of-transitions)
    (s/validate (sc/NeedSettingCost+ valid-needs valid-settings) costs)
    {:standard-projection (prep-inputs initial-send-pop
                                       validate-valid-states valid-transitions transitions
                                       population valid-states original-transitions costs)
     :seed-year (inc max-transition-year)}))
