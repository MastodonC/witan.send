(ns witan.send.states
  (:require [clojure.string :as str]
            [witan.send.constants :as const]))

(def non-send :NON-SEND)

(defn need-setting [state]
  (if (= state non-send)
    (vector non-send non-send)
    (mapv keyword (str/split (name state) #"-"))))

(defn state [need setting]
  (if (= setting non-send)
    non-send
    (keyword (str (name need) "-" (name setting)))))

(defn valid-year-setting? [year setting]
  (or
   (and (= setting :CC) (<= year 0))
   (and (= setting :EO) (<= -1 year 14))
   (and (= setting :FEC) (<= 12 year 20))
   (and (= setting :IMS) (<= -3 year 14))
   (and (= setting :IN) (<= year 0))
   (and (= setting :ISC) (<= 15 year 20))
   (and (= setting :ISS) (<= 0 year 14))
   (and (= setting :ISSR) (<= 0 year 14))
   (and (= setting :IT) (<= 2 year 15))
   (and (= setting :MMS) (<= -3 year 14))
   (and (= setting :MSS) (<= -3 year 14))
   (and (= setting :OOE) (<= 6 year 20))
   (and (= setting :PRU) (<= 2 year 14))
   (and (= setting :MU) (<= -1 year 14))))

(defn valid-state? [academic-year state]
  (or (= state non-send)
      (let [[need setting] (need-setting state)]
        (valid-year-setting? academic-year setting))))

(def valid-states
  (->> (concat (for [academic-year const/academic-years
                     setting const/settings
                     need const/needs]
                 [academic-year (state need setting)])
               (for [academic-year const/academic-years]
                 [academic-year non-send]))
       (filter #(apply valid-state? %))))

(defn valid-transition? [academic-year state-1 state-2]
  (and (valid-state? (dec academic-year) state-1)
       (valid-state? academic-year state-2)))

(def valid-transitions
  (->> (concat (for [academic-year const/academic-years
                     from-setting const/settings
                     to-setting const/settings
                     need const/needs]
                 [academic-year (state need from-setting) (state need to-setting)]))
       (filter #(apply valid-transition? %))))

(defn transitions->initial-state
  [transitions]
  (reduce (fn [coll [[ay state-1 state-2] n]]
            (cond-> coll
              (not= state-1 non-send)
              (update [ay state-1] (fnil + 0) n)))
          {}
          transitions))

(defn transitions->state
  [transitions]
  (reduce (fn [coll [[ay state-1 state-2] n]]
            (cond-> coll
              (not= state-2 non-send)
              (update [ay state-2] (fnil + 0) n)))
          {}
          transitions))
