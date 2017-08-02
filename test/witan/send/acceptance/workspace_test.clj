(ns witan.send.acceptance.workspace-test
  (:require [clojure.test :refer :all]
            [witan.send.send :refer :all]
            [schema.core :as s]
            [witan.send.schemas :as sc]
            [witan.send.model :as m]
            [witan.workspace-api.protocols :as p]
            [witan.workspace-executor.core :as wex]
            [witan.send.test-utils :as tu]

            [clojure.core.matrix.dataset :as ds]
            [cheshire.core :refer [generate-string]]))

(def test-inputs
  {:initial-population ["data/demo/initial-population.csv" sc/PopulationSYA]
   :initial-send-population ["data/demo/send-population.csv" sc/SENDPopulation]
   :transition-matrix ["data/demo/transitions.csv" sc/TransitionCounts]
   :projected-population ["data/demo/projected-population.csv" sc/PopulationSYA]
   :setting-cost ["data/demo/setting-costs.csv" sc/SettingCost]})

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
    result))

(defn transitions [dataset]
  (->> (ds/row-maps dataset)
       (reduce (fn [coll {:keys [academic-year-2 setting-1 setting-2]}]
                 (update coll [academic-year-2 setting-1 setting-2] (fnil + 0) 1)) {})))

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
