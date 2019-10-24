(ns witan.send.states
  (:require [clojure.string :as str]
            [witan.send.constants :as c]))

(defn split-need-setting [need-setting]
  (if (= need-setting c/non-send)
    (vector c/non-send c/non-send)
    (mapv keyword (str/split (name need-setting) #"-"))))

(defn split-entity [entity]
  (let [ay (first entity)
        need-setting (second entity)]
    (into [ay] (map name (split-need-setting need-setting)))))

(defn join-need-setting [need setting]
  (if (or (= setting c/non-send)
          (= need c/non-send))
    c/non-send
    (keyword (str (name need) "-" (name setting)))))

(defn calculate-valid-settings-from-setting-academic-years
  [valid-states]
  (->> (map :setting valid-states)
       (into #{})))

(defn calculate-valid-needs-from-setting-academic-years
  [valid-states]
  (->> valid-states
       (mapcat (comp #(str/split % #",") :needs))
       (distinct)
       (map keyword)))

(defn calculate-valid-states-from-setting-academic-years
  [valid-states]
  (for [{:keys [setting min-academic-year max-academic-year needs]} valid-states
        need (map keyword (str/split needs #","))
        academic-year (range min-academic-year (inc max-academic-year))]
    [academic-year (join-need-setting need setting)]))

(defn validate-states-for-ay [valid-states ay]
  (->> (filter (fn [[ay' state]] (= ay ay')) valid-states)
       (map (fn [[ay' state]] state))))

(defn calculate-valid-settings-for-need-ay [valid-states]
  (reduce (fn [coll [ay state]]
            (let [[need setting] (split-need-setting state)]
              (update coll [ay need] (fnil conj #{}) setting)))
          {} valid-states))

(defn aggregate-setting->setting [setting setting->setting]
  (hash-map setting (mapv keyword (str/split setting->setting #","))))

(defn calculate-valid-mover-transitions [valid-states]
  (reduce into {} (map (fn [row] (aggregate-setting->setting (:setting row) (:setting->setting row)))
                       valid-states)))

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

(defn any-valid-transitions? [state valid-transitions]
  (< 1 (count (get valid-transitions (second (split-need-setting state))))))

(defn can-move? [valid-year-settings academic-year state valid-transitions]
  (let [[need setting] (split-need-setting state)]
    (and (any-valid-transitions? (join-need-setting need setting) valid-transitions)
         (pos? (count (disj (get valid-year-settings (inc academic-year)) setting))))))
