(ns witan.send.model
  (:require [witan.workspace-api :refer [defmodel]]
            [witan.workspace-api.protocols :as p]
            [witan.workspace-api.utils :refer [map-fn-meta
                                               map-model-meta]]
            [witan.send.send :as send]))

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
