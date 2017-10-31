(ns witan.send.params
  (:require [witan.send.states :as s]
            [witan.send.constants :as const]
            [witan.send.utils :as u]))

(def some+ (fnil + 0))

(def natural-prior 1/3)

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
  (let [d (->> coll vals (apply +))
        f (if (zero? d) 0 (/ n d))]
    (reduce (fn [coll [k v]]
              (assoc coll k (* f v)))
            {}
            coll)))

(defn joiner?
  [[ay state-1 state-2]]
  (and (= state-1 s/non-send)
       (not= state-2 s/non-send)))

(defn transitions-matrix-joiner?
  [{:keys [need-1]}]
  (= need-1 s/non-send))

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
  [valid-states transitions filter select]
  (let [transitions (remove-transitions transitions filter)
        academic-year (->> (beta-params-academic-year transitions select)
                           (reduce (fn [coll [ay alphas]]
                                     (assoc coll ay (weighted-alphas (* 2 natural-prior) alphas)))
                                   {}))
        academic-year-setting (beta-params-academic-year-setting transitions select)]
    (reduce (fn [coll [ay state]]
              (let [[need setting] (s/need-setting state)
                    observed (get academic-year-setting [ay setting] {})
                    by-ay (get academic-year ay {})]
                (assoc coll [ay state] (merge-with + {:alpha natural-prior :beta natural-prior} by-ay observed))))
            {}
            valid-states)))

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
          coll))

(defn weighted-alpha-params
  [valid-states valid-year-settings transitions select]
  (let [transitions (select-transitions transitions select)
        overall (alpha-params transitions (juxt state-2-setting))
        by-ay (alpha-params transitions (juxt academic-year state-2-setting))
        by-ay-setting (alpha-params transitions (juxt (juxt academic-year state-1-setting) state-2-setting))]
    (reduce (fn [coll [ay state]]
              (let [[need setting] (s/need-setting state)
                    valid-settings (-> (get valid-year-settings (inc ay))
                                       (disj setting))
                    
                    prior-alphas (reduce (fn [coll state]
                                           (assoc coll state natural-prior))
                                         {} valid-settings)
                    observed (-> (get by-ay-setting [ay setting])
                                 (select-keys valid-settings))
                    by-ay (->> (select-keys (get by-ay ay) valid-settings)
                               (weighted-alphas natural-prior))
                    prior (merge-with + prior-alphas overall by-ay)]
                (assoc coll [ay state] (->> (merge-with + prior-alphas by-ay observed)
                                            (map-keys #(s/state need %))
                                            (filter-vals pos?)))))
            {}
            valid-states)))

(defn weighted-joiner-state-alpha-params
  [valid-states transitions]
  (let [incumbents (->> (s/transitions->state transitions)
                        (reduce (fn [coll [[ay state] n]]
                                  (update-in coll [ay state] some+ n))
                                {}))
        transitions (select-transitions transitions joiner?)
        joiners (alpha-params transitions (juxt state-2))
        by-ay (alpha-params transitions (juxt academic-year state-2))
        academic-years (distinct (map first valid-states))]
    (reduce (fn [coll ay]
              (let [valid-states (s/valid-states-for-ay valid-states ay)
                    prior-alphas (reduce (fn [coll state]
                                           (assoc coll state natural-prior))
                                         {} valid-states)]
                (assoc coll ay (merge-with + prior-alphas
                                           (get by-ay ay)))))
            {}
            academic-years)))

(defn weighted-joiner-beta-params
  [joiners population]
  (let [population-academic-years (-> population vals first keys)
        joiner-calendar-years (keys joiners)
        n (count joiner-calendar-years)]
    (reduce
     (fn [coll ay]
       (reduce
        (fn [coll cy]
          (let [j (get-in joiners [cy ay] 0)
                p (get-in population [cy ay])]
            (prn {:joiners j :population p :academic-year ay :calendar-year cy})
            (-> coll
                (update-in [ay :alpha] u/some+ (/ j n))
                (update-in [ay :beta] u/some+ (/ (- p j) n)))))
        (assoc coll ay {:alpha natural-prior :beta natural-prior})
        joiner-calendar-years))
     {}
     population-academic-years)))

(defn beta-params-leavers [valid-states transitions]
  (weighted-beta-params valid-states transitions joiner? leaver?))

(defn beta-params-movers [valid-states transitions]
  (weighted-beta-params valid-states transitions (some-fn joiner? leaver?) mover?))

(defn alpha-params-movers [valid-states valid-year-settings transitions]
  (weighted-alpha-params valid-states valid-year-settings transitions mover?))

(defn alpha-params-joiner-states [valid-states transitions]
  (weighted-joiner-state-alpha-params valid-states transitions))

(defn calculate-joiners-per-calendar-year
  [transitions-matrix]
  (->> (filter transitions-matrix-joiner? transitions-matrix)
       (reduce (fn [coll {:keys [calendar-year academic-year-2]}]
                 (update-in coll [calendar-year academic-year-2] u/some+ 1))
               {})))

(defn calculate-population-per-calendar-year
  [population]
  (let [ay-population #(hash-map (:academic-year %) (:population %))]
    (reduce (fn [coll {:keys [calendar-year] :as row}]
              (update coll calendar-year merge (ay-population row)))
            {}
            population)))

(defn beta-params-joiners [transitions-matrix population]
  (let [joiners-per-calendar-year (calculate-joiners-per-calendar-year transitions-matrix)
        population-per-calendar-year (calculate-population-per-calendar-year population)]
    (weighted-joiner-beta-params joiners-per-calendar-year population-per-calendar-year)))
