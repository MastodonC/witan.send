(ns witan.send.params
  (:require [witan.send.states :as s]
            [witan.send.constants :as const]))

(def some+ (fnil + 0))

(defn multimerge-alphas [total & weight-alphas]

  (let [weight-alpha-sums (->> (partition 2 weight-alphas)
                               (map (fn [[w as]]
                                      [w as (->> as vals (apply +))])))
        m (/ total (apply + (map first (remove (comp zero? last) weight-alpha-sums))))]
    (->> (remove (fn [[_ _ s]]
                   (zero? s)) weight-alpha-sums)
         (reduce (fn [coll [w as s]]
                   (reduce (fn [coll [x a]]
                             (update coll x some+ (* m w (/ a s))))
                           coll
                           as))
                 {}))))

(defn weighted-alphas [n coll]
  (let [f (/ n (->> coll vals (apply +)))]
    (reduce (fn [coll [k v]]
              (assoc coll k (* f v)))
            {}
            coll)))

(defn joiner?
  [[ay state-1 state-2]]
  (and (= state-1 s/non-send)
       (not= state-2 s/non-send)))

(defn leaver?
  [[ay state-1 state-2]]
  (and (not= state-1 s/non-send)
       (= state-2 s/non-send)))

(defn mover?
  [[ay state-1 state-2]]
  (and (not= state-1 s/non-send)
       (not= state-2 s/non-send)
       (not= state-1 state-2)))

(defn remove-transitions
  [transitions pred]
  (reduce (fn [coll [transition n]]
            (cond-> coll
              (not (pred transition))
              (assoc transition n)))
          {}
          transitions))

(defn select-transitions
  [transitions pred]
  (reduce (fn [coll [transition n]]
            (cond-> coll
              (pred transition)
              (assoc transition n)))
          {}
          transitions))

(defn beta-params-overall
  [transitions f]
  (reduce (fn [coll [transition n]]
            (if (f transition)
              (update coll :alpha some+ n)
              (update coll :beta some+ n)))
          {} transitions))

(defn alpha-params [transitions f]
  (reduce (fn [coll [transition n]]
            (update-in coll (f transition) some+ n))
          {} transitions))

(defn state-1-setting
  [[_ state-1 _]]
  (let [[_ setting] (s/need-setting state-1)]
    setting))

(defn state-2
  [[_ _ state-2]]
  state-2)

(defn state-2-setting
  [[_ _ state-2]]
  (let [[_ setting] (s/need-setting state-2)]
    setting))

(defn academic-year
  [[ay _ _]]
  ay)

(defn beta-params-academic-year
  [transitions f]
  (reduce (fn [coll [[ay _ _ :as transition] n]]
            (if (f transition)
              (update-in coll [ay :alpha] some+ n)
              (update-in coll [ay :beta] some+ n)))
          {} transitions))

(defn beta-params-academic-year-setting
  [transitions f]
  (reduce (fn [coll [[ay state-1 state-2 :as transition] n]]
            (let [[_ setting] (s/need-setting state-1)]
              (if (f transition)
                (update-in coll [[ay setting] :alpha] some+ n)
                (update-in coll [[ay setting] :beta] some+ n))))
          {} transitions))

(defn weighted-beta-params
  [transitions filter select w1 w2]
  (let [transitions (remove-transitions transitions filter)
        overall (->> (beta-params-overall transitions select)
                     (weighted-alphas w1))
        academic-year (->> (beta-params-academic-year transitions select)
                           (reduce (fn [coll [ay alphas]]
                                     (assoc coll ay (weighted-alphas w2 alphas)))
                                   {}))
        academic-year-setting (beta-params-academic-year-setting transitions select)]
    (reduce (fn [coll [ay state]]
              (let [[need setting] (s/need-setting state)
                    observed (get academic-year-setting [ay setting])
                    by-ay (get academic-year ay)
                    prior (merge-with + overall by-ay)]
                (assoc coll [ay state] (merge-with + prior observed))))
            {}
            s/valid-states)))

(defn map-keys [f coll]
  (reduce (fn [coll [k v]]
            (assoc coll (f k) v))
          (empty coll)
          coll))

(defn filter-vals [pred coll]
  (reduce (fn [coll [k v]]
            (cond-> coll
              (pred v)
              (assoc k v)))
          (empty coll)
         coll ))

(defn weighted-alpha-params
  [transitions select w1 w2]
  (let [transitions (select-transitions transitions select)
        overall (alpha-params transitions (juxt state-2-setting))
        by-ay (alpha-params transitions (juxt academic-year state-2-setting))
        by-ay-setting (alpha-params transitions (juxt (juxt academic-year state-1-setting) state-2-setting))]
    (reduce (fn [coll [ay state]]
              (let [[need setting] (s/need-setting state)
                    observed (-> (get by-ay-setting [ay setting])
                                 (dissoc setting))                    
                    overall (->> (dissoc overall setting)
                                 (weighted-alphas w1))
                    by-ay (->> (dissoc (get by-ay ay) setting)
                               (weighted-alphas w2))
                    prior (merge-with + overall by-ay)]
                (assoc coll [ay state] (->> (merge-with + prior observed)
                                            (map-keys #(s/state need %))
                                            (filter-vals pos?)))))
            {}
            s/valid-states)))

(defn weighted-joiner-state-alpha-params
  [transitions w1]
  (let [transitions (select-transitions transitions joiner?)
        overall (->> (alpha-params transitions (juxt state-2))
                     (weighted-alphas w1))
        by-ay (alpha-params transitions (juxt academic-year state-2))]
    (reduce (fn [coll ay]
              (assoc coll ay (merge-with + overall (get by-ay ay))))
            {}
            const/academic-years)))

(defn weighted-joiner-age-alpha-params
  [transitions]
  (let [transitions (select-transitions transitions joiner?)
        overall (alpha-params transitions (juxt academic-year))]
    (reduce (fn [coll ay]
              (assoc coll ay (get overall ay 0)))
            {}
            const/academic-years)))

(defn weighted-joiner-age-beta-params
  [transitions population]
  (let [joiners (->> (select-transitions transitions joiner?)
                     (reduce (fn [coll [[ay _ _] n]]
                               (update coll ay (fnil + 0) n))
                             {}))]
    (reduce (fn [coll ay]
              (let [alpha (get joiners ay 0)
                    beta (- (get population ay 0) alpha)]
                (assoc coll ay {:alpha alpha :beta beta})))
            {}
            const/academic-years)))

(defn beta-params-leavers [transitions w1 w2]
  (weighted-beta-params transitions joiner? leaver? w1 w2))

(defn beta-params-movers [transitions w1 w2]
  (weighted-beta-params transitions (some-fn joiner? leaver?) mover? w1 w2))

(defn alpha-params-movers [transitions w1 w2]
  (weighted-alpha-params transitions mover? w1 w2))

(defn alpha-params-joiner-ages [transitions]
  (weighted-joiner-age-alpha-params transitions))

(defn alpha-params-joiner-states [transitions w1]
  (weighted-joiner-state-alpha-params transitions w1))

(defn beta-params-joiners [transitions population]
  (weighted-joiner-age-beta-params transitions population))