(ns witan.send.send
  (:require [witan.workspace-api :refer [defworkflowfn
                                         definput
                                         defworkflowpred
                                         defworkflowoutput]]
            [schema.core :as s]
            [witan.send.schemas :as sc]))

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
  (println "Got historic population")
  {:historic-population {}})

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
  [{:keys [historic-population extra-population]}
   {:keys [projection-start-year]}]
  (println "Got initial total population")
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
  (println "Starting Loop in year:" current-year-in-loop)
  {:current-population {}
   :total-population {}
   :current-year-in-loop current-year-in-loop})

(defworkflowfn get-transition-matrix-1-0-0
  {:witan/name :send/get-transition-matrix
   :witan/version "1.0.0"
   :witan/input-schema {:transitions-default sc/TransitionMatrix
                        :transitions-reduced-secondary-joiners sc/TransitionMatrix}
   :witan/param-schema {:scenario (s/enum :default :reduced-secondary-joiners)}
   :witan/output-schema {:transition-matrix sc/TransitionMatrix}}
  [{:keys [transitions-default transitions-reduced-secondary-joiners]} {:keys [scenario]}]
  (println "Got transition matrix for" scenario "scenario")
  {:transition-matrix {}})

(defworkflowfn apply-state-changes-1-0-0
  {:witan/name :send/apply-state-changes
   :witan/version "1.0.0"
   :witan/input-schema {:current-population sc/SENDSchemaIndividual
                        :transition-matrix sc/TransitionMatrix
                        :current-year-in-loop sc/YearSchema
                        :total-population sc/SENDSchemaIndividual}
   :witan/output-schema {:current-population sc/SENDSchemaIndividual
                         :current-year-in-loop sc/YearSchema
                         :total-population sc/SENDSchemaIndividual}}
  [{:keys [current-population transition-matrix current-year-in-loop total-population]} _]
  (println "Applied state changes")
   {:current-population {}
    :current-year-in-loop current-year-in-loop
    :total-population total-population})

(defworkflowfn append-to-total-population-1-0-0
  {:witan/name :send/append-to-total-population
   :witan/version "1.0.0"
   :witan/input-schema {:total-population sc/SENDSchemaIndividual
                        :current-population sc/SENDSchemaIndividual
                        :current-year-in-loop sc/YearSchema}
   :witan/output-schema {:total-population sc/SENDSchemaIndividual
                         :current-year-in-loop sc/YearSchema}}
  [{:keys [total-population current-population current-year-in-loop]} _]
  (println "Appended new states to total population")
   {:total-population {}
    :current-year-in-loop (inc current-year-in-loop)})

(defworkflowpred finish-looping?-1-0-0
  {:witan/name :send/send-loop-pred
   :witan/version "1.0.0"
   :witan/input-schema {:current-year-in-loop sc/YearSchema}
   :witan/param-schema {:projection-end-year sc/YearSchema}}
  [{:keys [current-year-in-loop]} {:keys [projection-end-year]}]
  (println "The year has been changed to" current-year-in-loop)
  (> current-year-in-loop projection-end-year))

;;Post-loop functions
(defworkflowfn group-send-projection-1-0-0
  {:witan/name :send/group-send-projection
   :witan/version "1.0.0"
   :witan/input-schema {:total-population sc/SENDSchemaIndividual}
   :witan/output-schema {:send-projection sc/SENDSchemaGroupedWithCI}}
  [{:keys [total-population]} _]
  (println "The loop has been exited. Grouping SEND projection.")
   {:send-projection {}})

(defworkflowfn apply-costs-1-0-0
  {:witan/name :send/apply-costs
   :witan/version "1.0.0"
   :witan/input-schema {:send-projection sc/SENDSchemaGroupedWithCI
                        :cost-profile sc/CostProfile}
   :witan/output-schema {:send-costs sc/YearlyCost}}
  [{:keys [send-projection]} _]
  (println "Applying costs")
   {:send-costs {}})

(defworkflowoutput model-outputs-1-0-0
  {:witan/name :send/model-outputs
   :witan/version "1.0.0"
   :witan/input-schema {:send-projection sc/SENDSchemaGroupedWithCI
                        :send-costs sc/YearlyCost}}
  [{:keys [send-projection send-costs]} _]
  (println "Model finished!")
  (println "Here is the SEND projection:" send-projection)
  (println "Here are the SEND costs:" send-costs)
   {:send-projection send-projection
    :send-costs send-costs})
