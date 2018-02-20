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

(defn gather-set-data [to-or-from? x-from-str x-to-str data]
  (->> (reduce (fn [coll {:keys [setting-1 setting-2]}]
                 (update coll [setting-1 setting-2] u/some+ 1))
               {}
               data)
       (mapcat (fn [i [[s1 s2] v]]
                 (vector {:id i :x x-from-str :y s1 :value v :setting (if (= to-or-from? :from)
                                                                        s2
                                                                        s1)}
                         {:id i :x x-to-str :y s2 :value v :setting (if (= to-or-from? :from)
                                                                      s2
                                                                      s1)}))
               (range))))

(defn r-combine [data]
  (vec (concat [:c] data)))

(def palette ["#1b9e77" "#d95f02" "#7570b3" "#e7298a" "#D55E00" "#CC79A7"])

(defn sankey [{:keys [title] :or {title ""}} df]
  (gg4clj/render
   [[:require "ggforce"]
    [:<- :data (gg4clj/data-frame df)]
    [:<- :palette (r-combine palette)]
    (gg4clj/r+
     [:ggplot :data [:aes :x {:id :id :split [:factor :y] :value :value}]]
     [:ggtitle title]
     [:geom_parallel_sets [:aes {:fill :setting}] {:alpha 0.5 :axis.width 0.1}]
     [:geom_parallel_sets_axes {:axis.width 0.2 :fill "#F6F6F6" :color "#DDDDDD"}]
     [:geom_parallel_sets_labels {:color "#444444" :angle 0 :size 2.5}]
     [:theme {:axis.title.x [:element_blank]
              :axis.text.y [:element_blank]}])]))

(defn sankey-transitions [data calendar-year settings]
  (->> data
       (remove v/joiner?)
       (remove v/leaver?)
       (filter #(= (:calendar-year %) calendar-year))
       (filter #(= (:academic-year-1 %) 6))
       (map #(-> (update % :setting-1 settings) (update :setting-2 settings)))
       (gather-set-data :from "NCY 6" "NCY 7")
       (seq-of-maps->data-frame)
       (sankey {:title (str "Aggregate setting transitions: " calendar-year "/" (apply str (drop 2 (str (inc calendar-year)))))}))
  (move-file "Rplots.pdf" (str "target/Historic-Transitions_" calendar-year ".pdf")))

(defn sankey-joiners [data]
  (->> data
       (remove v/leaver?)
       (filter #(= (:setting-1 %) :NONSEND))
       (map #(-> (update % :setting-1 name) (update :setting-2 name)))
       (gather-set-data :from "From" "To")
       (seq-of-maps->data-frame)
       (sankey {:title "Joiner transitions"}))
  (move-file "Rplots.pdf" (str "target/Joiner-Transitions.pdf")))

(defn sankey-setting-specific [data setting-to]
  (->> data
       (remove v/joiner?)
       (remove v/leaver?)
       (filter #(= (:setting-2 %) setting-to))
       (map #(-> (update % :setting-1 name) (update :setting-2 name)))
       (gather-set-data :to "From" "To")
       (seq-of-maps->data-frame)
       (sankey {:title (str "Joiner transitions")}))
  (move-file "Rplots.pdf" (str "target/" (name setting-to) "-mover-transitions.pdf")))

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
    (gg4clj/render [[:<- :data
                     (gg4clj/data-frame
                      (merge lowers uppers {:year ay}))]
                    (apply gg4clj/r+ (concat [[:ggplot :data [:aes :year :upper0]]]
                                             ribbon-all-years
                                             [[:ggtitle (str title " probability by academic year")]]
                                             [[:xlab "NCY"]]
                                             [[:ylab "95% probability interval"]]
                                             [[:scale_fill_manual {:name "Years"
                                                                   :labels (r-combine (map str years))
                                                                   :values (r-combine colours)}]]
                                             [[:scale_x_continuous {:breaks (r-combine (filter even? (range min-year max-year)))
                                                                    :limit [:c min-year max-year]}]]))]))
  (move-file "Rplots.pdf" (str "target/" title "_Probability.pdf")))

(defn get-total-historical-send-pop [year transitions-data]
  (count (map :academic-year-1 (filter #(= (:calendar-year %) year) (filter #(not= (:setting-1 %) :NONSEND) transitions-data)))))

(defn is-send-and-is-year? [row year]
  (and (not= (:setting-1 row) :NONSEND)
       (= (:calendar-year row) year)))

(defn population-line-plot
  [historic-data future-projections-data]
  (let [historic-years (vec (sort (distinct (map :calendar-year historic-data))))
        projected-years (vec (#(range % (+ (count future-projections-data) %))
                              (inc (last historic-years))))
        historic-pops (map #(get-total-historical-send-pop % historic-data) historic-years)
        years (into [] (concat historic-years projected-years))
        means (into [] (concat historic-pops (map int (map :mean future-projections-data))))
        low-cis (into [] (concat (repeat (count historic-years) :NA) (map :low-ci future-projections-data)))
        high-cis (into [] (concat (repeat (count historic-years) :NA) (map :high-ci future-projections-data)))
        gg4clj-data-frame {:year years :mean means :min low-cis :max high-cis}
        min-year (first historic-years)
        max-year (last projected-years)
        highest-val (apply max (map :max future-projections-data))]
    (gg4clj/render [[:<- :df
                     (gg4clj/data-frame gg4clj-data-frame)]
                    (gg4clj/r+ [:ggplot {:data :df} [:aes {:x :year :y :mean}]]
                               [:ggtitle "SEND Population Projection"]
                               [:geom_line [:aes {:colour "mean" :linetype "mean"}]]
                               [:geom_point {:colour "darkcyan"}]
                               [:geom_line {:data :df} [:aes {:x :year :y :max :colour "conf" :linetype "conf"}]]
                               [:geom_line {:data :df} [:aes {:x :year :y :min :colour "conf" :linetype "conf"}]]
                               [:scale_y_continuous {:name "Total SEND population" :limits [:c 0 highest-val]}]
                               [:scale_x_continuous {:name "Year" :breaks [:seq min-year max-year {:by 1}] :limits [:c min-year max-year]}]
                               [:theme_bw]
                               [:theme {:axis.text.x [:element_text {:size 8}]}]
                               [:scale_linetype_manual {:name "" :values [:c {:mean "solid" :conf "dashed"}] :labels [:c {:mean "Mean" :conf "95% Confidence"}]}]
                               [:scale_colour_manual {:name "" :values [:c {:mean "darkcyan" :conf "grey38"}] :labels [:c {:mean "Mean" :conf "95% Confidence"}]}])]))
  (move-file "Rplots.pdf" "target/Total_Population.pdf"))

(defn to-millions [n]
  (/ n 1000000))

(defn send-cost-plot
  [cost-data years]
  (let [proj-years (vec (#(range % (+ (count cost-data) %)) (inc (last years))))
        low-ci (vec  (map to-millions (map :low-ci cost-data)))
        q1 (vec (map to-millions (map :q1 cost-data)))
        median  (vec (map to-millions (map :median cost-data)))
        q3 (vec (map to-millions (map :q3 cost-data)))
        high-ci (vec (map to-millions (map :high-ci cost-data)))
        gg2clj-data-frame {:year proj-years :ymin low-ci :lower q1 :middle median :upper q3 :ymax high-ci}
        highest-val (apply max high-ci)
        min-year (first proj-years)
        max-year (last proj-years)]
    (gg4clj/render [[:<- :df
                     (gg4clj/data-frame gg2clj-data-frame)]
                    (gg4clj/r+ [:ggplot {:data :df} [:aes {:x :year}]]
                               [:ggtitle "SEND Cost Projection"]
                               [:geom_boxplot {:stat "identity"} {:fill "snow"} {:colour "darkcyan"} [:aes {:ymin :ymin :lower :lower :middle :middle :upper :upper :ymax :ymax}]]
                               [:scale_y_continuous {:name "Total Projected SEND Cost  /  £ million" :limits [:c 0 highest-val]}]
                               [:scale_x_continuous {:name "Year" :breaks [:seq min-year max-year {:by 1}] :limits [:c (- min-year 0.5) (+ max-year 0.5)]}]
                               [:theme_bw] )]))
  (move-file "Rplots.pdf" "target/Total_Cost.pdf"))
