(ns witan.send.acceptance.workspace-test
  (:require [clojure.core.matrix.dataset :as ds]
            [clojure.test :refer :all]
            [schema.core :as s]
            [witan.send.model :as m]
            [witan.send.schemas :as sc]
            [witan.send.states :as states]
            [witan.send.test-utils :as tu]
            [witan.workspace-api.protocols :as p]
            [witan.workspace-executor.core :as wex]
            [witan.send.utils :as u]))

(def inputs-path "demo/")

(defn test-inputs []
  {:settings-to-change ["data/demo/modify-settings.csv" sc/SettingsToChange]
   :initial-send-population [(str "data/" inputs-path "send-population.csv") sc/SENDPopulation]
   :transition-matrix [(str "data/" inputs-path "transitions.csv") sc/TransitionCounts]
   :population [(str "data/" inputs-path "population.csv") sc/PopulationDataset]
   :setting-cost [(str "data/" inputs-path "need-setting-costs.csv") sc/NeedSettingCost]
   :valid-setting-academic-years [(str "data/" inputs-path "valid-setting-academic-years.csv") sc/ValidSettingAcademicYears]})

(defn add-input-params
  [file-map input]
  (assoc-in input [:witan/params :fn] (fn [a b]
                                        (tu/read-inputs file-map input a b))))

(witan.workspace-api/set-api-logging! println)

(deftest send-workspace-test
  (testing "The default model is run on the workspace and returns the outputs expected"
    (let [fixed-catalog (->> (:catalog m/send-model)
                             (mapv #(if (= (:witan/type %) :input)
                                      (add-input-params (test-inputs) %)
                                      (assoc-in % [:witan/params :simulations] 10)))
                             (map #(assoc-in % [:witan/params :output] false)))
          workspace     {:workflow  (:workflow m/send-model)
                         :catalog   fixed-catalog
                         :contracts (p/available-fns (m/model-library))}
          workspace'    (s/with-fn-validation (wex/build! workspace))
          result        (apply merge (wex/run!! workspace' {}))]
      (is (= #{:total-in-send-by-ay :total-in-send-by-ay-group :by-state :total-cost
               :total-in-send :total-in-send-by-need :total-in-send-by-setting}
             (-> result first keys set))))))

(defn run-model
  "function expects a map of the following keys

  :iterations - number of simulations/iterations (min 8)

  :output? - true/false to output data and charts

  :transition-modifier - value to modify transition rate of :transitions-file contents by (optional)

  :transitions-file - csv containing list of settings to modify by :transition-modifier (optional)

  :modify-transitions-from - set a year to start modifying transitions from when :transition-modifier & :transitions-file are set (optional)

  :filter-transitions-from - sets a year or year range as a vector to filter historic transitions by for :splice-ncy (optional)

  :splice-ncy - sets a national curriculum year to ignore transitions of prior to :filter-transitions-from year (optional)"
  [{:keys [iterations output? transition-modifier transitions-file modify-transitions-from filter-transitions-from splice-ncy]}]
  (u/reset-log)
  (u/log-info (str "Input Data: " (clojure.string/replace inputs-path #"/" " ")))
  (u/log-info (str "Number of iterations: " iterations))
  (u/log-info (str "Transitions modifier: " (if (nil? transition-modifier) "None" transition-modifier)))
  (u/log-info (str "Transitions file: " (if (nil? transitions-file) "None" transitions-file)))
  (u/log-info (str "Modify transitions from: " (if (nil? modify-transitions-from) "None" modify-transitions-from)))
  (u/log-info (str "Filter transitions from: " (if (nil? filter-transitions-from) "None" filter-transitions-from)))
  (u/log-info (str "Splice NCY: " (if (nil? splice-ncy) "None" splice-ncy) "\n"))
  (let [file-input    (if (nil? transitions-file)
                        (test-inputs)
                        (assoc (test-inputs) :settings-to-change [(str "data/" inputs-path transitions-file) sc/SettingsToChange]))
        fixed-catalog (let [prep-catalog1 (->> (:catalog m/send-model)
                                               (mapv #(if (= (:witan/type %) :input)
                                                        (add-input-params file-input %)
                                                        (assoc-in % [:witan/params :simulations] iterations)))
                                               (map #(assoc-in % [:witan/params :output] output?)))
                            prep-catalog2 (if (nil? transition-modifier)
                                            prep-catalog1
                                            (map #(assoc-in % [:witan/params :modify-transition-by] transition-modifier) prep-catalog1))
                            prep-catalog3 (if (nil? modify-transitions-from)
                                            prep-catalog2
                                            (map #(assoc-in % [:witan/params :modify-transitions-from] modify-transitions-from) prep-catalog2))]
                        (if (nil? filter-transitions-from)
                          prep-catalog3
                          (->> prep-catalog3
                               (map #(assoc-in % [:witan/params :filter-transitions-from] filter-transitions-from))
                               (map #(assoc-in % [:witan/params :splice-ncy] splice-ncy)))))
        workspace     {:workflow  (:workflow m/send-model)
                       :catalog   fixed-catalog
                       :contracts (p/available-fns (m/model-library))}
        workspace'    (s/with-fn-validation (wex/build! workspace))
        result        (apply merge (wex/run!! workspace' {}))]))
