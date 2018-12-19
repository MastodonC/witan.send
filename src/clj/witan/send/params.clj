(ns witan.send.params
  (:require [clojure.set :as set]
            [witan.send.constants :as c]
            [witan.send.maths :as m]
            [witan.send.states :as s]))

(defn joiner?
  [[ay state-1 state-2]]
  (and (= state-1 c/non-send)
       (not= state-2 c/non-send)))

(defn transitions-joiner?
  [{:keys [need-1]}]
  (= need-1 c/non-send))

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

(defn state-2
  [[_ _ state-2]]
  state-2)

(defn academic-year
  [[ay _ _]]
  ay)

(defn continue-for-latter-ays [params academic-years]
  (reduce (fn [coll ay]
            (if-let [v (or (get coll ay)
                           nil)] ;; Replace with average of adjacent AY params
              (assoc coll ay v)
              coll))
          params (sort academic-years)))

(defn calculate-joiners-per-calendar-year
  [transitions]
  (->> (filter transitions-joiner? transitions)
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

(defn any-valid-transitions? [state valid-transitions]
  (< 1 (count (get valid-transitions (second (s/split-need-setting state))))))
;; informal api
(defn beta-params-leavers [valid-states transitions]
  (let [academic-years (->> (map first valid-states)
                            (distinct)
                            (sort))
        observations (reduce (fn [coll {:keys [academic-year-1 need-1 setting-1 setting-2]}]
                               (if (= setting-1 c/non-send)
                                 coll
                                 (if (= setting-2 c/non-send)
                                   (update-in coll [academic-year-1 (s/join-need-setting need-1 setting-1) :alpha] m/some+ 1)
                                   (update-in coll [academic-year-1 (s/join-need-setting need-1 setting-1) :beta] m/some+ 1))))
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

(defn beta-params-joiners
  "Returns beta dist parameters for each academic year by apportioning
  its data equally across calendar years"
  [valid-states transitions population]
  (let [joiners (calculate-joiners-per-calendar-year transitions)
        joiner-calendar-years (keys joiners)
        population (calculate-population-per-calendar-year population)
        academic-years (->> (map first valid-states) distinct sort)
        n (count joiner-calendar-years)
        params (reduce
                (fn [coll ay]
                  (reduce
                   (fn [coll cy]
                     (let [j (get-in joiners [cy ay])
                           p (get-in population [cy ay])]
                       (if j
                         (if (> ay 12)
                           (-> coll
                               (update-in [ay :alpha] m/some+ (/ (/ j 2) n))
                               (update-in [ay :beta] m/some+ (/ (- p j) n)))
                           (-> coll
                               (update-in [ay :alpha] m/some+ (/ j n))
                               (update-in [ay :beta] m/some+ (/ (- p j) n))))
                         coll)))
                   coll
                   joiner-calendar-years))
                {}
                academic-years)]
    (continue-for-latter-ays params academic-years)))

(defn beta-params-movers
  "calculates the rate of the likelihood of a state transitioning for an academic year"
  [valid-states valid-transitions transitions]
  (let [academic-years (->> (map first valid-states)
                            (distinct)
                            (sort))
        observations (reduce (fn [coll {:keys [academic-year-1 need-1 setting-1 setting-2]}]
                               (if (or (= setting-1 c/non-send)
                                       (= setting-2 c/non-send))
                                 coll
                                 (if (not= setting-1 setting-2)
                                   (update-in coll [academic-year-1 (s/join-need-setting need-1 setting-1) :alpha] m/some+ 1)
                                   (update-in coll [academic-year-1 (s/join-need-setting need-1 setting-1) :beta] m/some+ 1))))
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

(defn alpha-params-joiners
  "The transition structure at this point is of the form:

  {[1 :NONSEND :SEMH-MSSIB] 1,
  [12 :NONSEND :SEMH-MMSIB] 1,
  [1 :SP-MSSOB :SP-MSSOB] 2,
  [14 :SP-NMSS :SP-NMSS]

  This structure has implict rules for interpretation.
  For understanding we can use the general form.

  [ay_n :need-setting ay_n+1 :new_need-new_setting]

  An :NONSEND-NONSEND can be shorted to :NONSEND
  The ay_n+1 can be ommitted and assumed."
  [valid-states transitions]
  (let [by-ay (-> transitions
                  (select-transitions joiner?)
                  (alpha-params (juxt academic-year state-2)))
        academic-years (->> valid-states (map first) distinct sort)
        params (reduce (fn [coll ay]
                         (let [valid-states (s/validate-states-for-ay valid-states ay)
                               prior-alphas (zipmap valid-states (repeat (/ 1.0 (count valid-states))))]
                           (if-let [v (get by-ay ay)]
                             (assoc coll ay (merge-with + prior-alphas v))
                             coll)))
                       {}
                       academic-years)]
    (continue-for-latter-ays params academic-years)))


(defn- mover-alpha-prior
  "Calculate prior by adding uniform distribution across allowed settings to observations and normalising."
  [need-setting valid-transitions valid-settings observations-per-ay ay]
  (let [[need setting] (s/split-need-setting need-setting)
        allowed-settings (set/intersection (set (get valid-settings [ay need]))
                                           (set (get valid-transitions setting)))
        prior (as-> allowed-settings s
                (zipmap s (repeat (/ 1.0 (count allowed-settings))))
                (merge-with + (get observations-per-ay ay) s)
                (dissoc s setting))
        total (reduce + (vals prior))]
    (reduce (fn [coll [setting v]]
              (assoc coll (s/join-need-setting need setting) (double (/ v total))))
            {}
            prior)))

(defn- mover-observations-per-ay
  [valid-states mover-transitions]
  (let [academic-years (->> (map first valid-states)
                            (distinct)
                            (sort))
        observations-per-ay (reduce (fn [coll {:keys [academic-year-1 setting-2]}]
                                      (update-in coll [academic-year-1 setting-2] m/some+ 1))
                                    {} mover-transitions)]
    (reduce (fn [coll ay]
              (if-let [v (or (get coll ay)
                             (get coll (dec ay)))]
                (assoc coll ay v)
                coll))
            observations-per-ay
            academic-years)))

(defn alpha-params-movers
  "calculates the rate of transitions to a new state at academic year X for state Y"
  [valid-states valid-transitions transitions]
  (let [mover-transitions (remove (fn [{:keys [setting-1 setting-2]}]
                                    (or (= setting-1 c/non-send)
                                        (= setting-2 c/non-send)
                                        (= setting-1 setting-2)))
                                  transitions)
        observations (reduce (fn [coll {:keys [academic-year-1 need-1 setting-1 need-2 setting-2]}]
                               (update-in coll [academic-year-1
                                                (s/join-need-setting need-1 setting-1)
                                                (s/join-need-setting need-2 setting-2)] m/some+ 1))
                             {} mover-transitions)
        valid-settings (s/calculate-valid-settings-for-need-ay valid-states)]
    (reduce (fn [coll [ay need-setting]]
              (let [obs-for-ay (get-in observations [ay need-setting])
                    prior (mover-alpha-prior need-setting
                                             valid-transitions
                                             valid-settings
                                             (mover-observations-per-ay valid-states mover-transitions)
                                             ay)]
                (assoc coll [ay need-setting] (merge-with + prior obs-for-ay))))
            {}
            valid-states)))
