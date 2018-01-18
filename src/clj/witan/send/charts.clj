(ns witan.send.charts
  (:require [witan.send.validate :as v]
            [gg4clj.core :as gg4clj]
            [clojure.java.io :as io]
            [witan.send.utils :as u]
            [clojure.core.matrix.dataset :as ds]
            [witan.send.test-utils :as tu]
            [witan.send.schemas :as sc]))

(defn move-file [source-path dest-path]
  (io/copy (io/file source-path) (io/file dest-path))
  (io/delete-file source-path))

(defn seq-of-maps->data-frame
  [coll]
  (when-let [x (first coll)]
    (let [ks (keys x)
          f (apply juxt ks)]
      (zipmap ks (apply map vector (map f coll))))))

(defn gather-set-data [data]
  (->> (reduce (fn [coll {:keys [setting-1 setting-2]}]
                 (update coll [setting-1 setting-2] u/some+ 1))
               {}
               data)
       (mapcat (fn [i [[s1 s2] v]]
                 (vector {:id i :x "NCY 6" :y s1 :value v :setting s2}
                         {:id i :x "NCY 7" :y s2 :value v :setting s2}))
               (range))))

(defn r-combine [data]
  (vec (concat [:c] data)))

(def palette ["#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7" "#1b9e77" "#d95f02" "#7570b3" "#e7298a"])

(defn sankey [{:keys [title] :or {title ""}} df]
  (gg4clj/render
   [[:require "ggforce"]
    [:<- :foo (gg4clj/data-frame df)]
    [:<- :palette (r-combine palette)]
    (gg4clj/r+
     [:ggplot :foo [:aes :x {:id :id :split [:factor :y] :value :value}]]
     [:ggtitle title]
     [:geom_parallel_sets [:aes {:fill :setting}] {:alpha 0.5 :axis.width 0.1}]
     [:geom_parallel_sets_axes {:axis.width 0.2 :fill "#F6F6F6" :color "#DDDDDD"}]
     [:geom_parallel_sets_labels {:color "#444444" :angle 0 :size 2.5}]
     [:theme {:axis.title.x [:element_blank]}])]))

(defn sankey-transitions [data calendar-year settings]
  (->> data
       (remove v/joiner?)
       (remove v/leaver?)
       (filter #(= (:calendar-year %) calendar-year))
       (filter #(= (:academic-year-1 %) 6))
       (map #(-> (update % :setting-1 settings) (update :setting-2 settings)))
       (gather-set-data)
       (seq-of-maps->data-frame)
       (sankey {:title (str "Aggregate setting transitions: " calendar-year "/" (apply str (drop 2 (str (inc calendar-year)))))}))
  (move-file "Rplots.pdf" (str "target/historic-transitions_" calendar-year ".pdf")))

(defn pull-year
  [data pos]
  (->> (nth data pos)
       (map (fn [[a b c]] (if (zero? b) [a :NA :NA] [a b c])))
       (apply mapv vector)))

(defn create-keys [string year-count]
  (map (fn [n] (keyword (str string n))) (range year-count)))

(defn create-CI-map [string data pos year-count]
  (reduce into {}
          (map (fn [k v]
                 (assoc {} k v))
               (create-keys string year-count)
               (map #(nth (nth data %) pos) (range year-count)))))

(defn random-colour []
  (apply str "#" (repeatedly 6 #(format "%x" (rand-int 16)))))

(defn ribbon-vec [pos colour]
  [:geom_ribbon [:aes {:ymax (keyword (str "upper" pos)) :ymin (keyword (str "lower" pos)) :fill colour}] {:alpha 0.2}])

(defn valid-years-vector? [data]
  (not= (last data) 0))

(defn ribbon-plot
  [data title years colours]
  (let [n-years (count years)
        filter-data (map #(pull-year data %) (range n-years))
        ay (first (first filter-data))
        uppers (create-CI-map "upper" filter-data 1 n-years)
        lowers (create-CI-map "lower" filter-data 2 n-years)
        ribbon-all-years (map (fn [n c] (ribbon-vec n c)) (range n-years) colours)
        non-zero-data (filter valid-years-vector? (first data))
        min-year (first (first non-zero-data))
        max-year (first (last non-zero-data))]
    (gg4clj/render [[:<- :foo
                     (gg4clj/data-frame
                      (merge lowers uppers {:year ay}))]
                    (apply gg4clj/r+ (concat [[:ggplot :foo [:aes :year :upper0]]]
                                             ribbon-all-years
                                             [[:ggtitle (str title " probability by academic year")]]
                                             [[:xlab "NCY"]]
                                             [[:ylab "95% probability interval"]]
                                             [[:scale_fill_manual {:name "Years"
                                                                   :labels (r-combine (map str years))
                                                                   :values (r-combine colours)}]]
                                             [[:scale_x_continuous {:breaks (r-combine (filter even? (range min-year max-year)))
                                                                    :limit [:c min-year max-year]}]]))]))
  (move-file "Rplots.pdf" (str "target/" title "_probability.pdf")))
