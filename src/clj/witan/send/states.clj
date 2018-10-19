(ns witan.send.states
  (:require [clojure.string :as str]
            [witan.send.constants :as const]))

(def non-send :NONSEND)

(defn need-setting [state]
  (if (= state non-send)
    (vector non-send non-send)
    (mapv keyword (str/split (name state) #"-"))))

(defn state [need setting]
  (if (or (= setting non-send)
          (= need non-send))
    non-send
    (keyword (str (name need) "-" (name setting)))))

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
  (for [{:keys [setting setting->group min-academic-year max-academic-year needs]} valid-setting-academic-years
        need (map keyword (str/split needs #","))
        academic-year (range min-academic-year (inc max-academic-year))]
    [academic-year (state need setting)]))

(defn valid-states-for-ay [valid-states ay]
  (->> (filter (fn [[ay' state]] (= ay ay')) valid-states)
       (map (fn [[ay' state]] state))))

(defn calculate-valid-settings-for-need-ay [valid-states]
  (reduce (fn [coll [ay state]]
            (let [[need setting] (need-setting state)]
              (update coll [ay need] (fnil conj #{}) setting)))
          {} valid-states))

(defn aggregate-setting->setting [setting setting->setting]
  (hash-map setting (mapv keyword (str/split setting->setting #","))))

(defn calculate-valid-mover-transitions [valid-setting-academic-years]
  (reduce into {} (map (fn [row] (aggregate-setting->setting (:setting row) (:setting->setting row)))
                       valid-setting-academic-years)))

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
