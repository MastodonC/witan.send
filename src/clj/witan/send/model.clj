(ns witan.send.model
  (:require [witan.workspace-api :refer [defmodel]]
            [witan.workspace-api.protocols :as p]
            [witan.workspace-api.utils :refer [map-fn-meta
                                               map-model-meta]]
            [witan.send.send :as send]))

(def seed-year 2017)
(def random-seed 50)
(def simulations 1000)
(def target-growth 127.2)
(def target-variance 729.96)

(def send-model-workflow
  "Defines each step of the model"
  [[:initial-send-population :prepare-send-inputs]
   [:transition-matrix :prepare-send-inputs]
   [:population :prepare-send-inputs]
   [:setting-cost :prepare-send-inputs]
   [:valid-setting-academic-years :prepare-send-inputs]
   [:prepare-send-inputs :run-send-model]
   [:run-send-model :output-send-results]])

(def send-model-catalog
  "Provides metadata for each step in the model"
  [{:witan/name :initial-send-population
    :witan/version "1.0.0"
    :witan/type :input
    :witan/fn :send/initial-send-population
    :witan/params {:src ""}}
   {:witan/name :transition-matrix
    :witan/version "1.0.0"
    :witan/type :input
    :witan/fn :send/transition-matrix
    :witan/params {:src ""}}
   {:witan/name :population
    :witan/version "1.0.0"
    :witan/type :input
    :witan/fn :send/population
    :witan/params {:src ""}}
   {:witan/name :setting-cost
    :witan/version "1.0.0"
    :witan/type :input
    :witan/fn :send/setting-cost
    :witan/params {:src ""}}
   {:witan/name :valid-setting-academic-years
    :witan/version "1.0.0"
    :witan/type :input
    :witan/fn :send/valid-setting-academic-years
    :witan/params {:src ""}}
   {:witan/name :prepare-send-inputs
    :witan/version "1.0.0"
    :witan/type :function
    :witan/fn :send/prepare-send-inputs
    :witan/params {:seed-year seed-year}}
   {:witan/name :run-send-model
    :witan/version "1.0.0"
    :witan/type :function
    :witan/fn :send/run-send-model
    :witan/params {:seed-year seed-year
                   :random-seed random-seed
                   :simulations simulations
                   :target-growth target-growth
                   :target-variance target-variance}}
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
      (map-fn-meta send/initial-send-population-1-0-0
                   send/transition-matrix-1-0-0
                   send/population-1-0-0
                   send/setting-cost-1-0-0
                   send/valid-setting-academic-years-1-0-0
                   send/prepare-send-inputs-1-0-0
                   send/run-send-model-1-0-0
                   send/output-send-results-1-0-0))
    (available-models [_]
      (map-model-meta send-model))))
