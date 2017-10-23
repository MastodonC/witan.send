(ns witan.send.acceptance.workspace-test
  (:require [clojure.core.matrix.dataset :as ds]
            [clojure.test :refer :all]
            [schema.core :as s]
            [witan.send.model :as m]
            [witan.send.schemas :as sc]
            [witan.send.states :as states]
            [witan.send.test-utils :as tu]
            [witan.workspace-api.protocols :as p]
            [witan.workspace-executor.core :as wex]))

(def test-inputs
  {:initial-population ["data/test_data/camden/initial-population.csv" sc/PopulationDataset]
   :initial-send-population ["data/test_data/towerhamlets/send-population.csv" sc/SENDPopulation]
   :transition-matrix ["data/test_data/towerhamlets/transitions.csv" sc/TransitionCounts]
   :projected-population ["data/test_data/towerhamlets/projected-population.csv" sc/PopulationDataset]
   :setting-cost ["data/test_data/towerhamlets/setting-costs.csv" sc/SettingCost]
   :valid-setting-academic-years ["data/test_data/towerhamlets/valid-setting-academic-years.csv" sc/ValidSettingAcademicYears]})

(defn add-input-params
  [input]
  (assoc-in input [:witan/params :fn] (fn [a b]
                                        (tu/read-inputs test-inputs input a b))))

(witan.workspace-api/set-api-logging! println)

(deftest send-workspace-test
  (testing "The default model is run on the workspace and returns the outputs expected"
    (let [fixed-catalog (mapv #(if (= (:witan/type %) :input) (add-input-params %) %)
                              (:catalog m/send-model))
          workspace     {:workflow  (:workflow m/send-model)
                         :catalog   fixed-catalog
                         :contracts (p/available-fns (m/model-library))}
          workspace'    (s/with-fn-validation (wex/build! workspace))
          result        (apply merge (wex/run!! workspace' {}))]
      (is (first result))
      (is (= #{:total-in-send-by-ay :total-in-send-by-ay-group :by-state :total-cost :total-in-send :total-in-send-by-need :total-in-send-by-setting}
             (-> result first keys set))))))

(defn run! []
  (let [fixed-catalog (mapv #(if (= (:witan/type %) :input) (add-input-params %) %)
                            (:catalog m/send-model))
        workspace     {:workflow  (:workflow m/send-model)
                       :catalog   fixed-catalog
                       :contracts (p/available-fns (m/model-library))}
        workspace'    (s/with-fn-validation (wex/build! workspace))
        result        (apply merge (wex/run!! workspace' {}))]
    nil))

(defn transitions [dataset]
  (->> (ds/row-maps dataset)
       (reduce (fn [coll {:keys [academic-year-1 setting-1 need-1 setting-2 need-2]}]
                 (update coll [academic-year-1
                               (states/state need-1 setting-1)
                               (states/state need-2 setting-2)] (fnil + 0) 1)) {})))

(defn sankey [f transitions]
  (let [settings (-> (reduce (fn [coll [ay setting-1 setting-2]]
                               (into coll [setting-1 setting-2]))
                             #{}
                             (keys transitions))
                     (vec))
        settings-count (count settings)
        setting-index (into {} (map vector settings (range)))
        links (->> (filter (fn [[[ay s1 s2] n]]
                             (f ay)) transitions)
                   (reduce (fn [coll [[ay s1 s2] n]]
                             (update coll [s1 s2] (fnil + 0) n))
                           {})
                   (map (fn [[[s1 s2] n]]
                          {:source (setting-index s1) :target (+ settings-count (setting-index s2)) :value (float n)})))]
    (-> {:links links :nodes (map #(hash-map :name (name %)) (concat settings settings))})))
