(ns witan.send.utils
  (:require [clojure.core.matrix.dataset :as ds]
            [clojure.data.avl :as avl]
            [clojure.set :refer [union]]
            [witan.workspace-api.utils :as utils]
            [witan.send.schemas :as sc]
            [witan.send.states :as states]
            [kixi.stats.core :as kixi]
            [kixi.stats.random :refer [multinomial binomial beta-binomial dirichlet-multinomial draw]]
            [redux.core :as r]
            [medley.core :as medley]
            [schema.core :as s]
            [clojure.string :as str]
            )
  (:import [org.HdrHistogram IntCountsHistogram DoubleHistogram]))

(def random-seed (atom 0))
(defn set-seed! [n]
  (reset! random-seed n))
(defn get-seed! []
  (swap! random-seed inc))

(defn round [x]
  (Double/parseDouble (format "%.02f" (double x))))

(def some+
  "x + y. Returns y if x is nil and x if y is nil."
  (fnil + 0))

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

(defn transitions-map
  [dataset]
  (->> dataset
       (reduce (fn [coll {:keys [setting-1 need-1 setting-2 need-2 academic-year-2]}]
                 (let [state-1 (states/state need-1 setting-1)
                       state-2 (states/state need-2 setting-2)]
                   (update coll [academic-year-2 state-1 state-2] some+ 1)))
               {})))

(defn full-transitions-map
  [dataset]
  (->> dataset
       (reduce (fn [coll {:keys [calendar-year setting-1 need-1 setting-2 need-2 academic-year-2]}]
                 (let [state-1 (states/state need-1 setting-1)
                       state-2 (states/state need-2 setting-2)]
                   (update coll [calendar-year academic-year-2 state-1 state-2] some+ 1)))
               {})))

(defn split-need-state [state pos]
  (keyword (pos (str/split (name state) #"-"))))

(defn back-to-transitions-matrix [k v]
  (let [[calendar-year academic-year-2 state-1 state-2] k
        total v]
    (repeat total (assoc {}
                         :calendar-year calendar-year
                         :academic-year-1 (- academic-year-2 1)
                         :academic-year-2 academic-year-2
                         :need-1 (split-need-state state-1 first)
                         :setting-1 (if (nil? (split-need-state state-1 second))
                                      (split-need-state state-1 first)
                                      (split-need-state state-1 second))
                         :need-2 (split-need-state state-2 first)
                         :setting-2 (if (nil? (split-need-state state-2 second))
                                      (split-need-state state-2 first)
                                      (split-need-state state-2 second))))))

(defn sq [x] (* x x))
(defn abs [x] (if (< x 0)
                (- x)
                x))

(defn adjust-beta-mean [mean n {:keys [alpha beta]}]
  (let [t (+ alpha beta)
        alpha (* (/ mean n) t)]
    {:alpha alpha
     :beta (- t alpha)}))

(defn adjust-beta-variance [v n {:keys [alpha beta] :as params}]
  (if (or (zero? alpha) (zero? beta))
    params
    (let [r (/ beta alpha)
          alpha (/ (- (* n n r)
                      (* (sq (inc r)) v))
                   (* (inc r)
                      (- (* (sq (inc r)) v)
                         (* n r))))]
      {:alpha alpha
       :beta (* r alpha)})))

(defn beta-binomial-variance [n {:keys [alpha beta]}]
  (/ (* n alpha beta (+ n alpha beta))
     (* (sq (+ alpha beta)) (+ alpha beta 1))))


(defn beta-binomial-mean [n {:keys [alpha beta]}]
  (/ (* n alpha) (+ alpha beta)))

(defn balance-joiners [joiners leavers]
  (let [join (->> (vals joiners)
                  (map :alpha)
                  (reduce +))
        leave (->> (vals leavers)
                   (map :alpha)
                   (reduce +))
        r (/ leave join) ;; Steady state
        ;; r (* r 1.3) ;; 30% growth
        ]
    ;; Adjust join rates to equal leave
    (reduce (fn [coll [academic-year params]]
              (update-in coll [academic-year :alpha] * r))
            joiners
            joiners)))

(defn sample-dirichlet-multinomial
  [n alphas]
  (try
    (let [[ks as] (apply mapv vector alphas)]
      (let [xs (if (pos? n)
                 (draw (dirichlet-multinomial n as) {:seed (get-seed!)})
                 (repeat 0))]
        (zipmap ks xs)))
    (catch Exception e
      (do (println n alphas)
          nil))))

(defn sample-beta-binomial
  [n params]
  (if (pos? n)
    (draw (beta-binomial n params) {:seed (get-seed!)})
    0))

(defn sample-binomial
  [n p]
  (if (pos? n)
    (draw (binomial {:n n :p p}) {:seed (get-seed!)})
    0))

(defn sample-send-transitions
  "Takes a total count and map of categories to probabilities and
  returns the count in each category at the next step."
  [state n probs mover-beta]
  (try
    (if (pos? n)
      (let [movers (sample-beta-binomial n mover-beta)
            non-movers (- n movers)]
        (-> (sample-dirichlet-multinomial movers probs)
            (assoc state non-movers)))
      {})
    (catch Exception e
      (do (println state n probs mover-beta)
          nil))))

(defn probability [p]
  (case (< p 0) 0.0
        (> p 1) 1.0
        :else p))

(def total-by-age
  "Given a sequence of {:age age :population population}
  sums the total population for each age"
  (partial reduce (fn [coll {:keys [age population]}]
                    (update coll age some+ population))
           {}))

(def total-by-academic-year
  "Given a sequence of {:academic-year year :population population}
  sums the total population for each year"
  (partial reduce (fn [coll {:keys [academic-year population]}]
                    (update coll academic-year some+ population))
           {}))

(defn model-population-by-ay
  [model]
  (reduce (fn [coll [[ay state] population]]
            (cond-> coll
              (not= state sc/non-send)
              (update ay some+ population)))
          {} model))

(defn model-population-by-need
  [model]
  (reduce (fn [coll [[ay state] population]]
            (let [[need setting] (states/need-setting state)]
              (cond-> coll
                (not= state sc/non-send)
                (update need some+ population))))
          {} model))

(defn model-population-by-setting
  [model]
  (reduce (fn [coll [[ay state] population]]
            (let [[need setting] (states/need-setting state)]
              (cond-> coll
                (not= state sc/non-send)
                (update setting some+ population))))
          {} model))

(defn model-population-by-need-setting
  [model]
  (reduce (fn [coll [[ay state] population]]
            (let [[need setting] (states/need-setting state)]
              (cond-> coll
                (not= state sc/non-send)
                (update [need setting] some+ population))))
          {} model))

(defn ay-groups [ay]
  (condp >= ay
    0 "NCY < 1"
    6 "NCY 1-6"
    11 "NCY 7-11"
    13 "NCY 12-13"
    "NCY 14+"))

(defn model-population-by-ay-group
  [model]
  (reduce (fn [coll [[ay state] population]]
            (let [ay-group (ay-groups ay)]
              (cond-> coll
                (not= state sc/non-send)
                (update ay-group some+ population))))
          {} model))

(defn total-setting-cost
  [setting-lookup population-by-setting]
  (-> (reduce (fn [cost [setting population]]
                (+ cost (* population (get setting-lookup setting 0))))
              0 population-by-setting)
      round))

(defn total-need-setting-cost
  [need-setting-lookup population-by-need-setting]
  (-> (reduce (fn [cost [need-setting population]]
                (+ cost (* population (get need-setting-lookup need-setting 0))))
              0 population-by-need-setting)
      round))

(defn model-send-population
  [model]
  (reduce (fn [n [[ay state] population]]
            (cond-> n
              (not= state sc/non-send)
              (+ population)))
          0 model))

;;;; Reducing functions for use with transduce

(def mean-or-zero-rf
  (r/post-complete
   kixi/mean
   (fn [mean]
     (or mean 0.0))))

(defn histogram-rf
  [number-of-significant-digits]
  (fn
    ([] (IntCountsHistogram. number-of-significant-digits))
    ([hist x]
     (doto hist (.recordValue (inc x))))
    ([hist] hist)))

(defn histogram-combiner-rf
  [number-of-significant-digits]
  (fn
    ([] (IntCountsHistogram. number-of-significant-digits))
    ([acc hist]
     (doto acc (.add hist)))
    ([hist]
     {:median (dec (.getValueAtPercentile hist 50.0))
      :mean (dec (.getMean hist))
      :std-dev (.getStdDeviation hist)
      :iqr (- (.getValueAtPercentile hist 75.0) (.getValueAtPercentile hist 25.0))
      :min (dec (.getValueAtPercentile hist 0.0))
      :max (dec (.getValueAtPercentile hist 100.0))
      :q1 (dec (.getValueAtPercentile hist 25.0))
      :q3 (dec (.getValueAtPercentile hist 75.0))
      :low-ci (dec (.getValueAtPercentile hist 2.5))
      :high-ci (dec (.getValueAtPercentile hist 97.5))})))

(defn merge-with-rf
  "Like (apply merge-with f) but for reducing functions"
  [rf]
  (fn
    ([] {})
    ([acc x]
     (reduce (fn [acc [k v]]
               (-> acc
                   (cond-> (not (contains? acc k))
                     (assoc k (rf)))
                   (update k rf v)))
             acc x))
    ([acc]
     (medley/map-vals rf acc))))

(defn model-states-rf
  [valid-states rf]
  (fn
    ([]
     (reduce (fn [coll k]
               (assoc coll k (rf)))
             {} valid-states))
    ([acc x]
     (medley/map-kv
      (fn [k v]
        [k (rf v (get x k 0))])
      acc))
    ([acc]
     (medley/map-vals rf acc))))

(defn with-keys-rf
  [rf keys]
  (fn
    ([]
     (reduce (fn [coll k]
               (assoc coll k (rf)))
             {} keys))
    ([acc x]
     (medley/map-kv
      (fn [k v]
        [k (rf v (get x k 0))])
      acc))
    ([acc]
     (medley/map-vals rf acc))))

(defn partition-rf
  "Executes the rf on partitions of length n, returning n results"
  [n rf]
  (fn
    ([]
     (println "Initialising...")
     (mapv #(%1) (repeat n rf)))
    ([acc xs]
     (println "Reduce rf...")
     (mapv #(rf %1 %2) acc xs))
    ([acc]
     (println "Complete rf...")
     (mapv #(rf %1) acc))))

(defn int-ceil [n]
  (int (Math/ceil n)))

(defn projection-state-to-map [[calendar-year-2 ay-2 state-1 state-2]]
  (let [[need-1 setting-1] (states/need-setting state-1)
        [need-2 setting-2] (states/need-setting state-2)]
    (hash-map :calendar-year calendar-year-2
              :setting-1 setting-1
              :need-1 need-1
              :academic-year-1 (dec ay-2)
              :setting-2 setting-2
              :need-2 need-2
              :academic-year-2 ay-2)))

(defn projection->transitions [[state sum]]
  (repeat sum (projection-state-to-map state)))

(defn keep-duplicates [seq]
  (for [[id freq] (frequencies seq)
        :when (> freq 1)]
    id))
