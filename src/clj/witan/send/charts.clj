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

(defn sankey [{:keys [title] :or {title ""}} df]
  (gg4clj/render
   [[:require "ggforce"]
    [:<- :foo (gg4clj/data-frame df)]
    [:<- :palette [:c "#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7" "#1b9e77" "#d95f02" "#7570b3" "#e7298a"]]
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

(defn ribbon-plot
  [data title years]
  (let [
        year1 (pull-year data 0)
        year2 (pull-year data 1)
        year3 (pull-year data 2)
        year4 (pull-year data 3)
      	[ay upper1 lower1] year1
        [_ upper2 lower2] year2
        [_ upper3 lower3] year3
        [_ upper4 lower4] year4]
    (map #(pull-year data %) years)
    #_(gg4clj/render [[:<- :foo (gg4clj/data-frame
                                 {:year ay :upper1 upper1 :lower1 lower1 :upper2 upper2 :lower2 lower2 :upper3 upper3 :lower3 lower3 :upper4 upper4 :lower4 lower4})]
                      (gg4clj/r+
                       [:ggplot :foo [:aes :year :upper1]]
                       [:geom_ribbon [:aes {:ymax :upper1 :ymin :lower1}] {:fill "#1b9e77" :alpha 0.2}]
                       [:geom_ribbon [:aes {:ymax :upper2 :ymin :lower2}] {:fill "#d95f02" :alpha 0.2}]
                       [:geom_ribbon [:aes {:ymax :upper3 :ymin :lower3}] {:fill "#7570b3" :alpha 0.2}]
                       [:geom_ribbon [:aes {:ymax :upper4 :ymin :lower4}] {:fill "#e7298a" :alpha 0.2}]
                       [:ggtitle title]
                       [:xlab "NCY"]
                       [:ylab "95% probability interval"]
                       [:scale_x_continuous {:breaks [:seq -5 15 {:by 2}]}]
                       [:scale_fill_manual {:name "Years" :values [:c "red" "yellow" "grey" "blue"] :labels [:c "2013" "2014" "2015" "2016"]}]
                       [:scale_colour_manual "" {:values "blue"}]
                       [:theme])])))
