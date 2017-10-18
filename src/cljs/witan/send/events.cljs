(ns witan.send.events
  (:require [re-frame.core :as re-frame]
            [witan.send.db :as db]
            [witan.send.states :as s]
            [witan.send.step :as step]
            [witan.send.params :as p]
            [clojure.string :as str]))

(re-frame/reg-event-db
 :initialize-db
 (fn  [_ _]
   db/default-db))

(defn sankey [f transitions]
  (js/console.log (pr-str transitions))
  (let [transitions (filter (fn [[[ay s1 s2] n]]
                              (and (f ay) (pos? n))) transitions)

        [from to] (->> transitions
                       (reduce (fn [[from to] [[ay setting-1 setting-2] n]]
                                 (if (pos? n)
                                   [(conj from setting-1) (conj to setting-2)]
                                   [from to]))
                               [#{} #{}]))
        from (vec (sort from))
        to (vec (sort to))
        
        from-index (into {} (map vector from (range)))
        to-index (into {} (map vector to (iterate inc (count from))))
        links (->> transitions
                   (reduce (fn [coll [[ay s1 s2] n]]
                             (update coll [s1 s2] (fnil + 0) n))
                           {})
                   (mapv (fn [[[s1 s2] n]]
                           {:source (from-index s1) :target (to-index s2) :value (float n)})))]
    (js/console.log (pr-str transitions))
    {:links links :nodes (mapv #(hash-map :name (some-> % name)) (concat from to))}))

(defn setting-transitions
  [transitions]
  (reduce (fn [coll [[academic-year-1 state-1 state-2] n]]
            (let [[need-1 setting-1] (s/need-setting state-1)
                  [need-2 setting-2] (s/need-setting state-2)]
              (update coll [academic-year-1 setting-1 setting-2] (fnil + 0) n)))
          {} transitions))

(defn update-sankey-fx
  [{:keys [academic-year transitions model-transitions leaver-weights mover-alpha-weights mover-beta-weights population joiner-alpha-weights joiner-beta-weights]}]
  (let [[w1 w2 w3] leaver-weights
        [ma1 ma2 ma3] mover-alpha-weights
        [mb1 mb2 mb3] mover-beta-weights
        [ja1 ja2] joiner-alpha-weights
        [jb1 jb2] joiner-beta-weights
        params (p/beta-params-leavers transitions w1 w2 w3)

        valid-states [] ;;TODO
        
        mover-beta (p/beta-params-movers valid-states transitions mb1 mb2 mb3)
        mover-alpha (p/alpha-params-movers valid-states transitions ma1 ma2 ma3)
        
        joiner-beta (p/beta-params-joiners valid-states transitions population 1)
        joiner-alpha-ages (p/alpha-params-joiner-ages transitions)
        joiner-alpha-states (p/alpha-params-joiner-states valid-states transitions ja1 ja2 1)
        
        state (s/transitions->initial-state transitions)
        transitions' {}
        [state transitions'] (step/step-leavers state transitions' params step/binomial-mean)
        [state transitions'] (step/step-movers state transitions' mover-beta step/binomial-mean mover-alpha step/dirichlet-mean)

        [state transitions'] (step/step-joiners state transitions' population joiner-beta step/binomial-mean
                                                joiner-alpha-ages step/dirichlet-mean
                                                joiner-alpha-states step/dirichlet-mean)]
    {:update-default-sankey! (sankey #(= % academic-year) (setting-transitions transitions))
     ;; :update-sankey! (sankey #(= % academic-year) (setting-transitions transitions'))
     :update-model-sankey! (sankey #(= % academic-year) model-transitions)
     }))

(re-frame/reg-event-fx
 :set-academic-year
 (fn [{:keys [db]} [_ academic-year]]
   (let [db (assoc db :academic-year academic-year)]
     (merge {:db db} (update-sankey-fx db)))))

(re-frame/reg-event-fx
 :set-leaver-weights
 (fn [{:keys [db]} [_ leaver-weights]]
   (let [db (assoc db :leaver-weights leaver-weights)]
     (merge {:db db} (update-sankey-fx db)))))

(re-frame/reg-event-fx
 :set-mover-alpha-weights
 (fn [{:keys [db]} [_ mover-weights]]
   (let [db (assoc db :mover-alpha-weights mover-weights)]
     (merge {:db db} (update-sankey-fx db)))))

(re-frame/reg-event-fx
 :set-mover-beta-weights
 (fn [{:keys [db]} [_ mover-weights]]
   (let [db (assoc db :mover-beta-weights mover-weights)]
     (merge {:db db} (update-sankey-fx db)))))

(re-frame/reg-event-fx
 :set-joiner-alpha-weights
 (fn [{:keys [db]} [_ mover-weights]]
   (let [db (assoc db :joiner-alpha-weights mover-weights)]
     (merge {:db db} (update-sankey-fx db)))))

(re-frame/reg-event-fx
 :set-joiner-beta-weights
 (fn [{:keys [db]} [_ mover-weights]]
   (let [db (assoc db :joiner-beta-weights mover-weights)]
     (merge {:db db} (update-sankey-fx db)))))

(re-frame/reg-fx
 :update-default-sankey!
 (fn [sankey]
   (js/displaySankey (clj->js sankey) "a")))

(re-frame/reg-fx
 :update-sankey!
 (fn [sankey]
   (js/displaySankey (clj->js sankey) "b")))

(re-frame/reg-fx
 :update-model-sankey!
 (fn [sankey]
   (js/displaySankey (clj->js sankey) "c")))
