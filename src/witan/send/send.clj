(ns witan.send.send
  (:require [witan.workspace-api :refer [defworkflowfn
                                         definput
                                         defworkflowpred
                                         defworkflowoutput]]
            [schema.core :as s]
            [witan.send.schemas :as sc]
            [clojure.core.matrix.dataset :as ds]
            [witan.datasets :as wds]
            [witan.datasets.stats :as wst]
            [witan.send.utils :as u]
            [markov-chains.core :as mc]))

;;Inputs
(definput historic-0-25-population-1-0-0
  {:witan/name :send/historic-0-25-population
   :witan/version "1.0.0"
   :witan/key :historic-0-25-population
   :witan/schema sc/PopulationSYA})

(definput historic-send-population-1-0-0
  {:witan/name :send/historic-send-population
   :witan/version "1.0.0"
   :witan/key :historic-send-population
   :witan/schema sc/SENDSchemaGrouped})

(definput population-projection-1-0-0
  {:witan/name :send/population-projection
   :witan/version "1.0.0"
   :witan/key :population-projection
   :witan/schema sc/PopulationSYA})

(definput cost-profile-1-0-0
  {:witan/name :send/cost-profile
   :witan/version "1.0.0"
   :witan/key :cost-profile
   :witan/schema sc/CostProfile})

(definput transition-matrix-1-0-0
  {:witan/name :send/transition-matrix
   :witan/version "1.0.0"
   :witan/key :transition-matrix
   :witan/schema sc/DataForMatrix})

;;Pre-loop functions


(defn add-state-to-send-population
  "Adds a 'state' column to a dataset with a need and placement column.
   Returns the dataset with the need and placement column removed."
  [historic-send-population]
  (-> historic-send-population
      (wds/add-derived-column :state [:need :placement]
                              (fn [n p] (keyword (str n "-" p))))
      (ds/remove-columns [:need :placement])))

(defn add-non-send-to-send-population
  "Given the SEND population by SYA & state, and the 0-25 population by SYA,
   calculates the number of Non-SEND pupils in the 0-25 population and adds
   them to the SEND population to make one dataset with the full 0-25 population
   grouped by age and state."
  [send-population-with-states historic-0-25-population]
  (let [renamed-send-popn (ds/rename-columns send-population-with-states {:population :count})
        send-totals (wds/rollup renamed-send-popn :sum :count [:age])
        num-rows (first (:shape historic-0-25-population))]
    (-> historic-0-25-population
        (wds/left-join send-totals [:age])
        (wds/add-derived-column :count [:population :count] -)
        (ds/add-column :state (repeat num-rows :Non-SEND))
        (ds/select-columns [:year :age :state :count])
        (ds/join-rows renamed-send-popn))))

(defn grps-to-indiv
  "Creates a data structure with one row per individual and per simulation."
  [repeat-data non-freq-col-names non-freq-col-data]
  (reduce merge (into [] (map (fn [colname colvalues]
                                {colname (repeat-data colvalues)})
                              non-freq-col-names non-freq-col-data))))

(defn add-simul-nbs
  "Add a column with simulation numbers to the data structure with individual rows."
  [indiv-data col-freqs num-sims]
  (merge indiv-data
         {:sim-num
          (into [] (take (* num-sims (apply + col-freqs)) (cycle (range 1 (inc num-sims)))))}))

(defn add-ids
  "Adds a column with an id per individual to the data structure with individual rows
   and simulation numbers."
  [data-with-sims num-sims range-individuals]
  (merge data-with-sims
         {:id (into [] (mapcat (fn [n] (into [] (repeat num-sims n))) range-individuals))}))

(defn select-cols
  "Analogue of select-keys for vectors - Reusing a fn by Henry Garner."
  [coll ids]
  (mapv (partial nth coll) ids))

(defn prepare-for-data-transformation
  "Performs steps ahead of the transformation (see fn below).
   It prepares the repeat of rows taking into account the number of individuals and simulations."
  [dataset frequency-column num-sims id-start-num]
  (let [groups-data (wds/select-from-ds dataset {frequency-column {:gt 0}})
        groups-matrix-data (:columns groups-data)
        index-col (.indexOf (:column-names dataset) frequency-column)
        col-freqs (nth groups-matrix-data index-col)
        other-cols (into [] (remove #{frequency-column} (:column-names groups-data)))
        index-other-cols (mapv #(.indexOf (:column-names groups-data) %) other-cols)
        matrix-other-cols (select-cols groups-matrix-data index-other-cols)
        counts-individs-with-sims (map #(* num-sims %) col-freqs)
        repeat-data (fn [col-data] (into [] (mapcat (fn [count val]
                                                      (into [] (repeat count val)))
                                                    counts-individs-with-sims col-data)))
        range-individuals (range id-start-num (+ id-start-num (apply + col-freqs)))]
    {:repeat-data repeat-data
     :other-cols other-cols
     :matrix-other-cols matrix-other-cols
     :col-freqs col-freqs
     :range-individuals range-individuals}))

(defn data-transformation
  "Transforms a dataset with groups to a dataset with individuals, ids and simulation numbers."
  ([dataset frequency-column num-sims]
   (data-transformation dataset frequency-column num-sims 1))
  ([dataset frequency-column num-sims id-start-num]
   (let [{:keys [repeat-data other-cols matrix-other-cols
                 col-freqs range-individuals]}
         (prepare-for-data-transformation
          dataset
          frequency-column
          num-sims
          id-start-num)]
     (-> (grps-to-indiv repeat-data other-cols matrix-other-cols)
         (add-simul-nbs col-freqs num-sims)
         (add-ids num-sims range-individuals)
         ds/dataset))))

(defworkflowfn get-historic-population-1-0-0
  "Outputs the population for the last year of historic data, with one
   row for each individual/year/simulation. Also includes age & state columns"
  {:witan/name :send/get-historic-population
   :witan/version "1.0.0"
   :witan/input-schema {:historic-0-25-population sc/PopulationSYA
                        :historic-send-population sc/SENDSchemaGrouped}
   :witan/param-schema {:projection-start-year sc/YearSchema
                        :number-of-simulations s/Int}
   :witan/output-schema {:historic-population sc/SENDSchemaIndividual}}
  [{:keys [historic-0-25-population historic-send-population]}
   {:keys [projection-start-year number-of-simulations]}]
  {:historic-population (let [send-with-states (add-state-to-send-population historic-send-population)
                              population-with-states (add-non-send-to-send-population
                                                      send-with-states
                                                      historic-0-25-population)]
                          (data-transformation population-with-states
                                               :count
                                               number-of-simulations))})

(defn lag
  "Dataset must be kept chronologically ordered in order to work correctly.
   Shifts the specified column down by the specified number of rows, filling
   in zeros at the top of the column and dropping the shifted values at the end."
  ([dataset col-key]
   (let [data (-> dataset
                  (ds/to-map)
                  (col-key))]
     (->> data
          (cons 0)
          (butlast))))
  ([dataset col-key lag-amount]
   (let [data  (-> dataset
                   (ds/to-map)
                   (col-key))]
     (->> data
          (concat (repeat lag-amount 0))
          (drop-last lag-amount)))))

(defn calc-population-difference
  "Given the historic 0-25 population and a population projection,
   calculates the difference in population between the aged-on population
   and the projected population each year to understand how the population changes."
  [historic-popn popn-projection projection-start-year projection-end-year]
  (let [lag-amount (- (inc projection-end-year) (dec projection-start-year))
        full-popn (-> historic-popn
                      ds/row-maps
                      (concat (ds/row-maps popn-projection))
                      ds/dataset
                      (u/order-ds [:age :year])
                      (as-> data (ds/add-column data :aged-on-population
                                                (lag data :population lag-amount)))
                      (ds/rename-columns {:year :previous-year})
                      (wds/add-derived-column :year [:previous-year] inc))]
    (-> (ds/rename-columns popn-projection {:population :population-from-proj})
        (wds/left-join full-popn [:age :year])
        (wds/add-derived-column :population-diff
                                [:population-from-proj :aged-on-population]
                                -))))

(defn transform-extra-population-data
  "Given the population change each year, returns a dataset that has
   one row per individual & simulation for each extra member of the population who
   needs to be added in the future."
  [popn-diff num-sims id-start-num]
  (let [number-rows (first (:shape popn-diff))]
    (-> popn-diff
        (wds/filter-dataset [:population-diff] (fn [p] (>= p 0)))
        (ds/add-column :state (repeat number-rows :Non-SEND))
        (ds/rename-columns {:population-diff :extra-population})
        (ds/select-columns [:year :age :state :extra-population])
        (data-transformation :extra-population num-sims id-start-num))))

(defworkflowfn population-change-1-0-0
  "Calculates how many extra Non-SEND must be added each year
   due to population growth. Returns a dataset with one row for each
   individual in each simulation"
  {:witan/name :send/population-change
   :witan/version "1.0.0"
   :witan/input-schema {:historic-0-25-population sc/PopulationSYA
                        :population-projection sc/PopulationSYA}
   :witan/param-schema {:projection-start-year sc/YearSchema
                        :projection-end-year sc/YearSchema
                        :number-of-simulations s/Int}
   :witan/output-schema {:extra-population sc/SENDSchemaIndividual}}
  [{:keys [historic-0-25-population population-projection]}
   {:keys [projection-start-year projection-end-year number-of-simulations]}]
  {:extra-population
   (let [hist-popn (wds/filter-dataset historic-0-25-population [:year]
                                       (fn [y] (= y (dec projection-start-year))))
         popn-proj (wds/filter-dataset population-projection [:year]
                                       (fn [y] (and (>= y projection-start-year)
                                                    (<= y projection-end-year))))
         id-start-num (inc (apply + (ds/column historic-0-25-population :population)))
         popn-diff (calc-population-difference hist-popn popn-proj
                                               projection-start-year
                                               projection-end-year)]
     (transform-extra-population-data popn-diff number-of-simulations id-start-num))})

(defworkflowfn add-extra-population-1-0-0
  "Combines the historic population with the extra population added in later years due to
   population change, to give the total population going into the loop."
  {:witan/name :send/add-extra-population
   :witan/version "1.0.0"
   :witan/input-schema {:historic-population sc/SENDSchemaIndividual
                        :extra-population sc/SENDSchemaIndividual}
   :witan/param-schema {:projection-start-year sc/YearSchema}
   :witan/output-schema {:total-population sc/SENDSchemaIndividual
                         :current-year-in-loop sc/YearSchema}}
  [{:keys [historic-population extra-population]} {:keys [projection-start-year]}]
  {:total-population (ds/join-rows historic-population extra-population)
   :current-year-in-loop projection-start-year})

;;Functions in loop
(defworkflowfn select-starting-population-1-0-0
  "Selects the rows from the total population that correspond to the individuals who
   were present in the year before the current year and who need to have their state
   assigned for the current year (e.g. for current year 2017, will select population
   from 2016)"
  {:witan/name :send/select-starting-population
   :witan/version "1.0.0"
   :witan/input-schema {:total-population sc/SENDSchemaIndividual
                        :current-year-in-loop sc/YearSchema}
   :witan/output-schema {:current-population sc/SENDSchemaIndividual
                         :total-population sc/SENDSchemaIndividual
                         :current-year-in-loop sc/YearSchema}}
  [{:keys [total-population current-year-in-loop]} _]
  {:current-population (wds/select-from-ds total-population {:year {:eq current-year-in-loop}})
   :total-population total-population
   :current-year-in-loop current-year-in-loop})

(defworkflowfn adjust-joiners-transition-1-0-0
  "Selects the desired transition matrix"
  {:witan/name :send/adjust-joiners-transition
   :witan/version "1.0.0"
   :witan/input-schema {:transition-matrix sc/DataForMatrix}
   :witan/param-schema {:age sc/AgeSchema :multiplier double}
   :witan/output-schema {:transition-matrix sc/DataForMatrix}}
  [{:keys [transition-matrix]} {:keys [age multiplier]}]
  (let [matrix-rows (ds/row-maps transition-matrix)
        from-non-send-and-age (fn [m] (and (= (:from-state m) :Non-SEND) (= (:age m) age)))
        bucket-fn (fn [row] (cond
                              (not (from-non-send-and-age row)) :non-selected-rows
                              (and (from-non-send-and-age row)
                                   (= (:to-state row) :Non-SEND)) :selected-rows-non-send
                              :else :selected-rows))
        {:keys [selected-rows selected-rows-non-send non-selected-rows]}
        (group-by bucket-fn matrix-rows)
        adjust-send-matrix (mapv (fn [m]
                                   (update m :probability #(* % multiplier))) selected-rows)
        total-send (transduce (map :probability) + adjust-send-matrix)
        adjust-non-send-matrix (mapv (fn [m]
                                       (assoc m :probability (- 1 total-send)))
                                     selected-rows-non-send)
        adjusted-matrix (concat adjust-send-matrix adjust-non-send-matrix non-selected-rows)]
    {:transition-matrix (ds/dataset adjusted-matrix)}))

(defworkflowfn apply-state-changes-1-0-0
  "Using the transition matrix, calculates the new state for each individual in the population.
   Also updates the age and year of the population dataset."
  {:witan/name :send/apply-state-changes
   :witan/version "1.0.0"
   :witan/input-schema {:current-population sc/SENDSchemaIndividual
                        :transition-matrix sc/DataForMatrix
                        :total-population sc/SENDSchemaIndividual
                        :current-year-in-loop sc/YearSchema}
   :witan/output-schema {:current-population sc/SENDSchemaIndividual
                         :total-population sc/SENDSchemaIndividual
                         :current-year-in-loop sc/YearSchema}}
  [{:keys [current-population transition-matrix total-population current-year-in-loop]} _]
  (let [adjusted-matrix (u/full-trans-mat sc/States [0 26] transition-matrix)
        new-states (wds/add-derived-column
                    current-population
                    :state [:age :state]
                    (fn [a s] (first (into [] (take 1 (mc/generate [s] (get adjusted-matrix a)))))))
        new-population (-> new-states
                           (wds/add-derived-column :year [:year] inc)
                           (wds/add-derived-column :age [:age] (fn [x]
                                                                 (if (< x 26)
                                                                   (inc x)
                                                                   x))))]
    {:current-population new-population
     :total-population total-population
     :current-year-in-loop current-year-in-loop}))

(defworkflowfn append-to-total-population-1-0-0
  "Adds the newly calculated states of the individuals for
   this year to the total population dataset"
  {:witan/name :send/append-to-total-population
   :witan/version "1.0.0"
   :witan/input-schema {:total-population sc/SENDSchemaIndividual
                        :current-population sc/SENDSchemaIndividual
                        :current-year-in-loop sc/YearSchema}
   :witan/output-schema {:total-population sc/SENDSchemaIndividual
                         :current-year-in-loop sc/YearSchema}}
  [{:keys [total-population current-population current-year-in-loop]} _]
  {:total-population (ds/join-rows total-population current-population)
   :current-year-in-loop (inc current-year-in-loop)})

(defworkflowpred finish-looping?-1-0-0
  "Predicate that returns true until the current year in the loop
   is equal to the projection end year"
  {:witan/name :send/send-loop-pred
   :witan/version "1.0.0"
   :witan/input-schema {:current-year-in-loop sc/YearSchema}
   :witan/param-schema {:projection-end-year sc/YearSchema}}
  [{:keys [current-year-in-loop]} {:keys [projection-end-year]}]
  (> current-year-in-loop projection-end-year))

;;Post-loop functions
(defn quantile ;; Re-used from https://gist.github.com/scottdw/2960070
  ([p vs]
   (let [svs (sort vs)]
     (quantile p (count vs) svs (first svs) (last svs))))
  ([p c svs mn mx]
   (let [pic (* p (inc c))
         k (int pic)
         d (- pic k)
         ndk (if (zero? k) mn (nth svs (dec k)))]
     (cond
       (zero? k) mn
       (= c (dec k)) mx
       (= c k) mx
       :else (+ ndk (* d (- (nth svs k) ndk)))))))

(defn add-avg-ci-to-projections
  "Takes in send projections grouped by year/age/state and outputs a dataset with year,
   age, need, placement, average number of children and confidence interval boundaries."
  [grouped-projection]
  (->> grouped-projection
       (pmap (fn [[k v]] (let [state (:state k)
                               sims-counts (frequencies (ds/column v :sim-num))
                               mean (/ (apply + (vals sims-counts)) (count sims-counts))
                               sorted-counts (sort (vals sims-counts))
                               low-ci (quantile 0.025 sorted-counts)
                               high-ci (quantile 0.975 sorted-counts)
                               split-keyword (fn [kw] (clojure.string/split (name kw) #"-"))
                               need (if (= state :Non-SEND) state
                                        (keyword (first (split-keyword state))))
                               placement (if (= state :Non-SEND) state
                                             (keyword (second (split-keyword state))))]
                           (assoc k :average-population mean :low-ci low-ci :high-ci high-ci
                                  :need need :placement placement))))
       ds/dataset))

(defn group-send-projection
  "Given the output from the loop which is a row for each individual and
   simulation and year, groups the data and converts it back to groups of
   individuals with the mean value from all the simulations as well as confidence intervals."
  [total-population]
  {:send-projection (->  total-population
                         (wds/group-ds [:year :age :state])
                         add-avg-ci-to-projections)})

(defn apply-costs
  "Multiplies the cost profile by the number of individuals to get the total cost"
  [{:keys [send-projection cost-profile]}]
  (let [safe-mul (fn [m c] (if c (* m c) 0.0))
        costs-with-states (add-state-to-send-population cost-profile)]
    {:send-costs (-> send-projection
                     (wds/left-join costs-with-states [:state])
                     (wds/add-derived-column :cost [:mean :cost-per-pupil] safe-mul)
                     (ds/remove-columns [:cost-per-pupil :state]))}))

(defworkflowoutput post-loop-steps-1-0-0
  "Groups the individual data from the loop to get a demand projection, and applies the cost profile
   to get the total cost."
  {:witan/name :send/post-loop-steps
   :witan/version "1.0.0"
   :witan/input-schema {:total-population sc/SENDSchemaIndividual
                        :cost-profile sc/CostProfile}}
  [{:keys [total-population]} _]
  (let [send-projection (group-send-projection total-population)
        send-costs (apply-costs send-projection)]
    (merge send-projection send-costs)))
