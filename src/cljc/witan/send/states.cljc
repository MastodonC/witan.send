(ns witan.send.states
  (:require [clojure.string :as str]
            [witan.send.constants :as const]))

(def non-send :NON-SEND)

(defn need-setting [state]
  (if (= state non-send)
    (vector non-send non-send)
    (mapv keyword (str/split (name state) #"-"))))

(defn state [need setting]
  (if (or (= setting non-send)
          (= need non-send))
    non-send
    (keyword (str (name need) "-" (name setting)))))

#_(defn valid-year-setting? [year setting]
  (or
   #_(and (= setting :CC) (<= year 0))
   (and (= setting :EO) (<= -4 year 20))
   (and (= setting :EYS) (<= -4 year 0))
   (and (= setting :FEC) (<= 12 year 20))
   (and (= setting :IMS) (<= -2 year 14))
   #_(and (= setting :IN) (<= year 0))
   #_(and (= setting :ISC) (<= 15 year 20))
   (and (= setting :ISS) (<= -4 year 15))
   (and (= setting :ISSR) (<= -4 year 15))
   #_(and (= setting :IT) (<= 2 year 15))
   (and (= setting :MAP) (<= 7 year 20))
   (and (= setting :MMS) (<= -2 year 14))
   (and (= setting :MMSIB) (<= -2 year 14))
   (and (= setting :MMSOB) (<= -2 year 14))
   (and (= setting :MSS) (<= -4 year 20))
   (and (= setting :MSSIB) (<= -2 year 15))
   (and (= setting :MSSOB) (<= -2 year 15))
   (and (= setting :MSSOP) (<= -2 year 15))
   (and (= setting :MSSR) (<= 5 year 15))
   #_(and (= setting :PRU) (<= 2 year 14))
   (and (= setting :MU) (<= -2 year 14))
   (and (= setting :MUOB) (<= -2 year 14))
   (and (= setting :NMSS) (<= -2 year 15))
   (and (= setting :NMSSR) (<= -2 year 15))
   (and (= setting :OOE) (<= -4 year 21))))

#_(defn valid-state? [academic-year state]
  (or (= state non-send)
      (let [[need setting] (need-setting state)]
        (valid-year-setting? academic-year setting))))

#_(def valid-states
  (->> (concat (for [academic-year const/academic-years
                     setting const/settings
                     need const/needs]
                 [academic-year (state need setting)])
               (for [academic-year const/academic-years]
                 [academic-year non-send]))
       (filter #(apply valid-state? %))))

(defn calculate-valid-settings-from-setting-academic-years
  [valid-setting-academic-years]
  (->> (map :setting valid-setting-academic-years)
       (into #{})))

(defn calculate-valid-needs-from-setting-academic-years
  [valid-setting-academic-years]
  (->> valid-setting-academic-years
       (mapcat (comp #(str/split % #",") :needs))
       (distinct)
       (map keyword)))

(defn calculate-valid-states-from-setting-academic-years
  [valid-setting-academic-years]
  (for [{:keys [setting min-academic-year max-academic-year needs]} valid-setting-academic-years
        academic-year (range min-academic-year (inc max-academic-year))
        need (map keyword (str/split needs #","))]
    [academic-year (state need setting)]))

#_(defn valid-transition? [academic-year state-1 state-2]
  (and (valid-state? academic-year state-1)
       (valid-state? (inc academic-year) state-2)))

#_(def valid-transitions
  (->> (concat (for [academic-year const/academic-years
                     from-setting const/settings
                     to-setting const/settings
                     need const/needs]
                 [academic-year (state need from-setting) (state need to-setting)]))
       (filter #(apply valid-transition? %))))

#_(defn valid-settings [ay]
  (->> (filter #(valid-year-setting? ay %) const/settings)
       (into #{})))

(defn valid-states-for-ay [valid-states ay]
  (->> (filter (fn [[ay' state]] (= ay ay')) valid-states)
       (map (fn [[ay' state]] state))))

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

#_(def valid-year-settings
  (->> (for [year const/academic-years
             setting const/settings
             :when (valid-year-setting? year setting)]
         [year setting])
       (reduce (fn [coll [year setting]]
                 (update coll year (fnil conj #{}) setting))
               {})))

(defn calculate-academic-year-range
  [setting-academic-years]
  [(->> (map :min-academic-year setting-academic-years)
        (apply min))
   (->> (map :max-academic-year setting-academic-years)
        (apply max))])

(defn calculate-valid-year-settings-from-setting-academic-years
  [setting-academic-years]
  (let [[minimum-academic-year maximum-academic-year]
        (calculate-academic-year-range setting-academic-years)]
    (reduce (fn [coll academic-year]
              (->> (filter (fn [{:keys [min-academic-year max-academic-year]}]
                             (<= min-academic-year academic-year max-academic-year))
                           setting-academic-years)
                   (into #{} (map :setting))
                   (assoc coll academic-year)))
            {}
            (range minimum-academic-year (inc maximum-academic-year)))))

(defn can-move? [valid-year-settings academic-year state]
  (let [[need setting] (need-setting state)]
    (pos? (count (disj (get valid-year-settings (inc academic-year)) setting)))))
