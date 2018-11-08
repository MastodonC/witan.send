(ns witan.send.params
  (:require [witan.send.constants :as c]
            [witan.send.maths :as m]
            [witan.send.states :as s]
            [witan.send.utils :as u]))

(def natural-prior 1/3)

(defn weighted-alphas [n coll]
  (let [d (->> coll vals (apply +))
        f (if (zero? d) 0 (/ n d))]
    (reduce (fn [coll [k v]]
              (assoc coll k (* f v)))
            {}
            coll)))

(defn joiner?
  [[ay state-1 state-2]]
  (and (= state-1 c/non-send)
       (not= state-2 c/non-send)))

(defn transitions-matrix-joiner?
  [{:keys [need-1]}]
  (= need-1 c/non-send))

(defn leaver?
  [[ay state-1 state-2]]
  (and (not= state-1 c/non-send)
       (= state-2 c/non-send)))

(defn mover?
  [[ay state-1 state-2]]
  (and (not= state-1 c/non-send)
       (not= state-2 c/non-send)
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

(defn alpha-params [transitions f]
  (reduce (fn [coll [transition n]]
            (update-in coll (f transition) m/some+ n))
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
              (update-in coll [ay :alpha] m/some+ n)
              (update-in coll [ay :beta] m/some+ n)))
          {} transitions))

(defn beta-params-academic-year-setting
  [transitions f]
  (reduce (fn [coll [[ay state-1 :as transition] n]]
            (let [[_ setting] (s/need-setting state-1)]
              (if (f transition)
                (update-in coll [[ay setting] :alpha] m/some+ n)
                (update-in coll [[ay setting] :beta] m/some+ n))))
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
              (let [[_ setting] (s/need-setting state)
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
                               (weighted-alphas natural-prior))]
                (assoc coll [ay state] (->> (merge-with + prior-alphas by-ay observed)
                                            (map-keys #(s/state need %))
                                            (filter-vals pos?)))))
            {}
            valid-states)))

(defn continue-for-latter-ays [params academic-years]
  (reduce (fn [coll ay]
            (if-let [v (or (get coll ay)
                           nil)] ;; Replace with average of adjacent AY params
              (assoc coll ay v)
              coll))
          params (sort academic-years)))

(defn weighted-joiner-state-alpha-params
  [valid-states transitions]
  (let [transitions (select-transitions transitions joiner?)
        by-ay (alpha-params transitions (juxt academic-year state-2))
        academic-years (->> (map first valid-states) distinct sort)
        params (reduce (fn [coll ay]
                         (let [valid-states (s/valid-states-for-ay valid-states ay)
                               prior-alphas (zipmap valid-states (repeat (/ 1.0 (count valid-states))))]
                           (if-let [v (get by-ay ay)]
                             (assoc coll ay (merge-with + prior-alphas v))
                             coll)))
                       {}
                       academic-years)]
    (continue-for-latter-ays params academic-years)))

(defn weighted-joiner-beta-params
  [valid-states joiners population]
  (let [academic-years (->> (map first valid-states) distinct sort)
        joiner-calendar-years (keys joiners)
        n (count joiner-calendar-years)
        params (reduce
                (fn [coll ay]
                  (reduce
                   (fn [coll cy]
                     (let [j (get-in joiners [cy ay])
                           p (get-in population [cy ay])]
                       (if j
                         (-> coll
                             (update-in [ay :alpha] m/some+ (/ j n))
                             (update-in [ay :beta] m/some+ (/ (- p j) n)))
                         coll)))
                   coll
                   joiner-calendar-years))
                {}
                academic-years)]
    (continue-for-latter-ays params academic-years)))

(defn beta-params-leavers [valid-states transitions]
  #_(weighted-beta-params valid-states transitions joiner? leaver?)
  (let [academic-years (->> (map first valid-states)
                            (distinct)
                            (sort))
        observations (reduce (fn [coll {:keys [academic-year-1 need-1 setting-1 need-2 setting-2 :as row]}]
                               (if (= setting-1 c/non-send)
                                 coll
                                 (if (= setting-2 c/non-send)
                                   (update-in coll [academic-year-1 (s/state need-1 setting-1) :alpha] m/some+ 1)
                                   (update-in coll [academic-year-1 (s/state need-1 setting-1) :beta] m/some+ 1))))
                             {} transitions)
        prior-per-year (reduce (fn [coll [ay state-betas]]
                                 (let [betas (apply merge-with + (vals state-betas))
                                       alpha (+ (get betas :alpha 0) 0.5)
                                       beta (+ (get betas :beta 0) 0.5)
                                       total (+ alpha beta)]
                                   (assoc coll ay {:alpha (double (/ alpha total))
                                                   :beta (double (/ beta total))})))
                               {} observations)
        prior-per-year (continue-for-latter-ays prior-per-year academic-years)]
    (reduce (fn [coll [ay state]]
              (if-let [beta-params (merge-with +
                                               (get-in observations [ay state])
                                               (get prior-per-year ay))]
                (assoc coll [ay state] beta-params)
                coll))
            {} valid-states)))

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
                 (update-in coll [calendar-year academic-year-2] m/some+ 1))
               {})))

(defn calculate-population-per-calendar-year
  [population]
  (let [ay-population #(hash-map (:academic-year %) (:population %))]
    (reduce (fn [coll {:keys [calendar-year] :as row}]
              (update coll calendar-year merge (ay-population row)))
            {}
            population)))

(defn beta-params-joiners [valid-states transitions-matrix population]
  (let [joiners-per-calendar-year (calculate-joiners-per-calendar-year transitions-matrix)
        population-per-calendar-year (calculate-population-per-calendar-year population)]
    (weighted-joiner-beta-params valid-states joiners-per-calendar-year population-per-calendar-year)))

(defn any-valid-transitions? [state valid-transitions]
  (< 1 (count (get valid-transitions (second (s/need-setting state))))))

(defn beta-params-movers
  "calculates the rate of the likelihood of a state transitioning for an academic year"
  [valid-states valid-transitions transitions]
  (let [academic-years (->> (map first valid-states)
                            (distinct)
                            (sort))
        observations (reduce (fn [coll {:keys [academic-year-1 need-1 setting-1 need-2 setting-2]}]
                               (if (or (= setting-1 c/non-send)
                                       (= setting-2 c/non-send))
                                 coll
                                 (if (not= setting-1 setting-2)
                                   (update-in coll [academic-year-1 (s/state need-1 setting-1) :alpha] m/some+ 1)
                                   (update-in coll [academic-year-1 (s/state need-1 setting-1) :beta] m/some+ 1))))
                             {} transitions)

        prior-per-year (reduce (fn [coll [ay state-betas]]
                                 (let [betas (apply merge-with + (vals state-betas))
                                       alpha (+ (get betas :alpha 0) 0.5)
                                       beta (+ (get betas :beta 0) 0.5)
                                       total (+ alpha beta)]
                                   (assoc coll ay {:alpha (double (/ alpha total))
                                                   :beta (double (/ beta total))})))
                               {} observations)
        prior-per-year (continue-for-latter-ays prior-per-year academic-years)]
    (reduce (fn [coll [ay state]]
              (if (any-valid-transitions? state valid-transitions)
                (if-let [beta-params (merge-with +
                                                 (get-in observations [ay state])
                                                 (get prior-per-year ay))]
                  (assoc coll [ay state] beta-params)
                  coll)
                coll))
            {} valid-states)))

(defn alpha-params-movers
  "calculates the rate of transitions to a new state at academic year X for state Y"
  [valid-states valid-transitions transitions]
  (let [academic-years (->> (map first valid-states)
                            (distinct)
                            (sort))
        observations (reduce (fn [coll {:keys [academic-year-1 need-1 setting-1 need-2 setting-2]}]
                               (if (or (= setting-1 c/non-send)
                                       (= setting-2 c/non-send)
                                       (= setting-1 setting-2))
                                 coll
                                 (update-in coll [academic-year-1
                                                  (s/state need-1 setting-1)
                                                  (s/state need-2 setting-2)] m/some+ 1)))
                             {} transitions)

        observations-per-ay (reduce (fn [coll {:keys [academic-year-1 need-1 setting-1 need-2 setting-2]}]
                                      (if (or (= setting-1 c/non-send)
                                              (= setting-2 c/non-send)
                                              (= setting-1 setting-2))
                                        coll
                                        (update-in coll [academic-year-1 setting-2] m/some+ 1)))
                                    {} transitions)
        observations-per-ay (reduce (fn [coll ay]
                                      (if-let [v (or (get coll ay)
                                                     (get coll (dec ay)))]
                                        (assoc coll ay v)
                                        coll))
                                    observations-per-ay
                                    academic-years)
        valid-settings (s/calculate-valid-settings-for-need-ay valid-states)]
    (reduce (fn [coll [ay state]]
              (let [[need setting] (s/need-setting state)
                    valid-trans (get valid-transitions setting)
                    obs (get-in observations [ay state])
                    ay-obs (get observations-per-ay ay)
                    settings (->> (get valid-settings [ay need])
                                  vec
                                  (into valid-trans)
                                  u/keep-duplicates
                                  vec)
                    prior (->> (zipmap settings (repeat (/ 1.0 (count settings))))
                               (merge-with + ay-obs))
                    prior (dissoc prior setting)
                    total (->> (vals prior) (apply +))
                    prior (reduce (fn [coll [setting v]]
                                    (assoc coll (s/state need setting) (double (/ v total))))
                                  {}
                                  prior)]
                (assoc coll [ay state] (merge-with + prior obs))))
            {}
            valid-states)))
