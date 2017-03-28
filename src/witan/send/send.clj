(ns witan.send.send
  (:require [witan.workspace-api :refer [defworkflowfn
                                         definput
                                         defworkflowpred
                                         defworkflowoutput]]
            [schema.core :as s]
            [witan.send.schemas :as sc]
            [clojure.core.matrix.dataset :as ds]
            [witan.datasets :as wds]))

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

(definput transitions-default-1-0-0
  {:witan/name :send/transitions-default
   :witan/version "1.0.0"
   :witan/key :transitions-default
   :witan/schema sc/TransitionMatrix})

(definput transitions-reduced-secondary-joiners-1-0-0
  {:witan/name :send/transitions-reduced-secondary-joiners
   :witan/version "1.0.0"
   :witan/key :transitions-reduced-secondary-joiners
   :witan/schema sc/TransitionMatrix})

;;Pre-loop functions
(defn add-state-to-send-population
  [historic-send-population]
  (-> historic-send-population
      (wds/add-derived-column :state [:need :placement]
                              (fn [n p] (keyword (str n "-" p))))
      (ds/select-columns [:year :age :state :population])
      (ds/rename-columns {:population :count})))

(defn add-non-send-to-send-population
  [send-population-with-states historic-0-25-population]
  (let [send-totals (wds/rollup send-population-with-states :sum :count [:age])
        num-rows (first (:shape historic-0-25-population))]
    (-> historic-0-25-population
        (wds/left-join send-totals [:age])
        (wds/add-derived-column :count [:population :count]
                                -)
        (ds/add-column :state (repeat num-rows :Non-SEND))
        (ds/select-columns [:year :age :state :count])
        (ds/join-rows send-population-with-states))))

(defn grps-to-indiv [repeat-data non-freq-col-names non-freq-col-data]
  (reduce merge (into [] (map (fn [colname colvalues]
                                {colname (repeat-data colvalues)})
                              non-freq-col-names non-freq-col-data))))

(defn add-simul-nbs [indiv-data col-freqs num-sims]
  (merge indiv-data
         {:sim-num
          (into [] (take (* num-sims (apply + col-freqs)) (cycle (range 1 (inc num-sims)))))}))

(defn add-ids [data-with-sims num-sims range-individuals]
  (merge data-with-sims
         {:id (into [] (mapcat (fn [n] (into [] (repeat num-sims n))) range-individuals))}))

(defn select-cols
  "Analogue of select-keys for vectors - thanks Henry!"
  [coll ids] (mapv (partial nth coll) ids))

(defn prepare-for-data-transformation
  [dataset frequency-column num-sims]
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
        range-individuals (range 1 (inc (apply + col-freqs)))]
    {:repeat-data repeat-data
     :other-cols other-cols
     :matrix-other-cols matrix-other-cols
     :col-freqs col-freqs
     :range-individuals range-individuals}))

(defn data-transformation
  [dataset frequency-column num-sims]
  (let [{:keys [repeat-data other-cols matrix-other-cols col-freqs range-individuals]}
        (prepare-for-data-transformation
         dataset
         frequency-column
         num-sims)]
    (-> (grps-to-indiv repeat-data other-cols matrix-other-cols)
        (add-simul-nbs col-freqs num-sims)
        (add-ids num-sims range-individuals)
        ds/dataset)))

(defworkflowfn get-historic-population-1-0-0
  {:witan/name :send/get-historic-population
   :witan/version "1.0.0"
   :witan/input-schema {:historic-0-25-population sc/PopulationSYA
                        :historic-send-population sc/SENDSchemaGrouped}
   :witan/param-schema {:projection-start-year sc/YearSchema
                        :number-of-simulations s/Int}
   :witan/output-schema {:historic-population sc/SENDSchemaIndividual}}
  [{:keys [historic-0-25-population historic-send-population]}
   {:keys [projection-start-year number-of-simulations]}]
  {:historic-population (let [send-with-states (add-state-to-send-population
                                                historic-send-population)
                              population-with-states
                              (add-non-send-to-send-population send-with-states
                                                               historic-0-25-population)]
                          (data-transformation population-with-states
                                               :count
                                               number-of-simulations))})

(defworkflowfn population-change-1-0-0
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
  {:extra-population {}})

(defworkflowfn add-extra-population-1-0-0
  {:witan/name :send/add-extra-population
   :witan/version "1.0.0"
   :witan/input-schema {:historic-population sc/SENDSchemaIndividual
                        :extra-population sc/SENDSchemaIndividual}
   :witan/param-schema {:projection-start-year sc/YearSchema}
   :witan/output-schema {:total-population sc/SENDSchemaIndividual
                         :current-year-in-loop sc/YearSchema}}
  [{:keys [historic-population extra-population]} {:keys [projection-start-year]}]
  {:total-population {}
   :current-year-in-loop projection-start-year})

;;Functions in loop
(defworkflowfn select-starting-population-1-0-0
  {:witan/name :send/select-starting-population
   :witan/version "1.0.0"
   :witan/input-schema {:total-population sc/SENDSchemaIndividual
                        :current-year-in-loop sc/YearSchema}
   :witan/output-schema {:current-population sc/SENDSchemaIndividual
                         :total-population sc/SENDSchemaIndividual
                         :current-year-in-loop sc/YearSchema}}
  [{:keys [total-population current-year-in-loop]} _]
  {:current-population {}
   :total-population total-population
   :current-year-in-loop current-year-in-loop})

(defworkflowfn get-transition-matrix-1-0-0
  {:witan/name :send/get-transition-matrix
   :witan/version "1.0.0"
   :witan/input-schema {:transitions-default sc/TransitionMatrix
                        :transitions-reduced-secondary-joiners sc/TransitionMatrix}
   :witan/param-schema {:scenario (s/enum :default :reduced-secondary-joiners)}
   :witan/output-schema {:transition-matrix sc/TransitionMatrix}}
  [{:keys [transitions-default transitions-reduced-secondary-joiners]} {:keys [scenario]}]
  {:transition-matrix {}})

(defworkflowfn apply-state-changes-1-0-0
  {:witan/name :send/apply-state-changes
   :witan/version "1.0.0"
   :witan/input-schema {:current-population sc/SENDSchemaIndividual
                        :transition-matrix sc/TransitionMatrix
                        :total-population sc/SENDSchemaIndividual
                        :current-year-in-loop sc/YearSchema}
   :witan/output-schema {:current-population sc/SENDSchemaIndividual
                         :total-population sc/SENDSchemaIndividual
                         :current-year-in-loop sc/YearSchema}}
  [{:keys [current-population transition-matrix total-population current-year-in-loop]} _]
  {:current-population {}
   :total-population total-population
   :current-year-in-loop current-year-in-loop})

(defworkflowfn append-to-total-population-1-0-0
  {:witan/name :send/append-to-total-population
   :witan/version "1.0.0"
   :witan/input-schema {:total-population sc/SENDSchemaIndividual
                        :current-population sc/SENDSchemaIndividual
                        :current-year-in-loop sc/YearSchema
                        :cost-profile sc/CostProfile}
   :witan/output-schema {:total-population sc/SENDSchemaIndividual
                         :current-year-in-loop sc/YearSchema
                         :cost-profile sc/CostProfile}}
  [{:keys [total-population current-population current-year-in-loop
           cost-profile]} _]
  {:total-population {}
   :current-year-in-loop (inc current-year-in-loop)
   :cost-profile {}})

(defworkflowpred finish-looping?-1-0-0
  {:witan/name :send/send-loop-pred
   :witan/version "1.0.0"
   :witan/input-schema {:current-year-in-loop sc/YearSchema}
   :witan/param-schema {:projection-end-year sc/YearSchema}}
  [{:keys [current-year-in-loop]} {:keys [projection-end-year]}]
  (> current-year-in-loop projection-end-year))

;;Post-loop functions
(defn group-send-projection
  [total-population]
  {:send-projection {}})

(defn apply-costs
  [{:keys [send-projection cost-profile]}]
  {:send-costs {}})

(defworkflowoutput post-loop-steps-1-0-0
  {:witan/name :send/post-loop-steps
   :witan/version "1.0.0"
   :witan/input-schema {:total-population sc/SENDSchemaIndividual
                        :cost-profile sc/CostProfile}}
  [{:keys [total-population]} _]
  (let [send-projection (group-send-projection total-population)
        send-costs (apply-costs send-projection)]
    (merge send-projection send-costs)))
