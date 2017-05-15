(ns witan.send.model
  (:require [witan.workspace-api :refer [defmodel]]
            [witan.workspace-api.protocols :as p]
            [witan.workspace-api.utils :refer [map-fn-meta
                                               map-model-meta]]
            [witan.send.send :as send]))

#_(def send-model-workflow
  "Defines each step of the model"
  [;;Inputs
   [:historic-0-25-population :get-historic-population]
   [:historic-send-population :get-historic-population]
   [:historic-0-25-population :population-change]
   [:population-projection :population-change]
   [:transition-matrix :adjust-joiners-transition]
   [:cost-profile :append-to-total-population]

   ;;Pre-loop
   [:get-historic-population :add-extra-population]
   [:population-change :add-extra-population]
   [:add-extra-population :select-starting-population]

   ;;Loop
   [:select-starting-population :apply-state-changes]
   [:adjust-joiners-transition :apply-state-changes]
   [:apply-state-changes :append-to-total-population]
   [:append-to-total-population [:finish-looping? :post-loop-steps
                                 :select-starting-population]]])

(def seed-year 2016)
(def projection-year 2019)

(def send-model-workflow
  "Defines each step of the model"
  [[:initial-population :prepare-send-inputs]
   [:initial-send-population :prepare-send-inputs]
   [:transition-matrix :prepare-send-inputs]
   [:projected-population :prepare-send-inputs]
   [:prepare-send-inputs :run-send-model]
   [:run-send-model :output-send-results]])

(def send-model-catalog
  "Provides metadata for each step in the model"
  [{:witan/name :initial-population
    :witan/version "1.0.0"
    :witan/type :input
    :witan/fn :send/initial-population
    :witan/params {:src ""}}
   {:witan/name :initial-send-population
    :witan/version "1.0.0"
    :witan/type :input
    :witan/fn :send/initial-send-population
    :witan/params {:src ""}}
   {:witan/name :transition-matrix
    :witan/version "1.0.0"
    :witan/type :input
    :witan/fn :send/transition-matrix
    :witan/params {:src ""}}
   {:witan/name :projected-population
    :witan/version "1.0.0"
    :witan/type :input
    :witan/fn :send/projected-population
    :witan/params {:src ""}}
   {:witan/name :prepare-send-inputs
    :witan/version "1.0.0"
    :witan/type :function
    :witan/fn :send/prepare-send-inputs
    :witan/params {}}
   {:witan/name :run-send-model
    :witan/version "1.0.0"
    :witan/type :function
    :witan/fn :send/run-send-model
    :witan/params {:seed-year seed-year
                   :projection-year projection-year}}
   {:witan/name :output-send-results
    :witan/version "1.0.0"
    :witan/type :output
    :witan/fn :send/output-send-results}])

#_(def send-model-catalog
  "Provides metadata for each step in the model"
  [;;Inputs
   {:witan/name :historic-0-25-population
    :witan/version "1.0.0"
    :witan/type :input
    :witan/fn :send/historic-0-25-population
    :witan/params {:src ""}}
   {:witan/name :historic-send-population
    :witan/version "1.0.0"
    :witan/type :input
    :witan/fn :send/historic-send-population
    :witan/params {:src ""}}
   {:witan/name :population-projection
    :witan/version "1.0.0"
    :witan/type :input
    :witan/fn :send/population-projection
    :witan/params {:src ""}}
   {:witan/name :cost-profile
    :witan/version "1.0.0"
    :witan/type :input
    :witan/fn :send/cost-profile
    :witan/params {:src ""}}

   ;;Inputs for scenarios
   {:witan/name :transition-matrix
    :witan/version "1.0.0"
    :witan/type :input
    :witan/fn :send/transition-matrix
    :witan/params {:src ""}}

   ;;Workflow functions pre-loop
   {:witan/name :get-historic-population
    :witan/version "1.0.0"
    :witan/type :function
    :witan/fn :send/get-historic-population
    :witan/params {:projection-start-year 2017
                   :number-of-simulations 2}}
   {:witan/name :population-change
    :witan/version "1.0.0"
    :witan/type :function
    :witan/fn :send/population-change
    :witan/params {:projection-start-year 2017
                   :projection-end-year 2019
                   :number-of-simulations 2}}
   {:witan/name :add-extra-population
    :witan/version "1.0.0"
    :witan/type :function
    :witan/fn :send/add-extra-population
    :witan/params {:projection-start-year 2017}}

   ;;Workflow functions in the loop
   {:witan/name :select-starting-population
    :witan/version "1.0.0"
    :witan/type :function
    :witan/fn :send/select-starting-population}
   {:witan/name :apply-state-changes
    :witan/version "1.0.0"
    :witan/type :function
    :witan/fn :send/apply-state-changes}
   {:witan/name :adjust-joiners-transition
    :witan/version "1.0.0"
    :witan/type :function
    :witan/fn :send/adjust-joiners-transition
    :witan/params {:age 11 :multiplier 0.1}}
   {:witan/name :append-to-total-population
    :witan/version "1.0.0"
    :witan/type :function
    :witan/fn :send/append-to-total-population}

   ;;Predicate to continue or exit loop
   {:witan/name :finish-looping?
    :witan/version "1.0.0"
    :witan/type :predicate
    :witan/fn :send/send-loop-pred
    :witan/params {:projection-end-year 2019}}

   ;;Workflow functions post-loop
   {:witan/name :post-loop-steps
    :witan/version "1.0.0"
    :witan/type :output
    :witan/fn :send/post-loop-steps}])

(defmodel send-model
  "Defines the model"
  {:witan/name :send/send-model
   :witan/version "1.0.0"}
  {:workflow send-model-workflow
   :catalog send-model-catalog})

(defn model-library
  "Lists all available functions to execute each step
   in the model"
  []
  (reify p/IModelLibrary
    (available-fns [_]
      (map-fn-meta send/initial-population-1-0-0
                   send/initial-send-population-1-0-0
                   send/transition-matrix-1-0-0
                   send/projected-population-1-0-0
                   send/prepare-send-inputs-1-0-0
                   send/run-send-model-1-0-0
                   send/output-send-results-1-0-0))
    (available-models [_]
      (map-model-meta send-model))))
