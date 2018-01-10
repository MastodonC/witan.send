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

(defn load-transitions [path]
  (-> (tu/csv-to-dataset path sc/TransitionCounts) ds/row-maps))

(def TH-setting->group
  (hash-map :OOE "Other"
            :MMSIB "Mainstream"
            :MU "Other"
            :MMSOB "Mainstream"
            :MSSOB "Special"
            :EO "Other"
            :EYS "Mainstream"
            :MMSIB "Mainstream"
            :MMSOB "Mainstream"
            :MAP "Other"
            :MSS "Special"
            :FEC "Further education"
            :IMS "Mainstream"
            :ISS "Special"
            :ISSR "Special"
            :MMSIB "Mainstream"
            :MU "Other"
            :MMSOB "Mainstream"
            :MUOB "Other"
            :MMS "Mainstream"
            :MSSIB "Special"
            :MSSOP "Special"
            :MSSR "Special"
            :MSSOB "Special"
            :NMSS "Special"
            :NMSSR "Special"
            :MAP "Other"
            :FEC "Further education"
            :FEC "Further education"
            :EO "Other"
            :NON-SEND "Other"))

(def Camden-setting->group
  (hash-map :MSS "Special"
            :IN "Mainstream"
            :EO "Other"
            :CC "Other"
            :MU "Other"
            :MMS "Mainstream"
            :IMS "Mainstream"
            :ISS "Special"
            :ISSR "Special"
            :PRU "Other"
            :OOE "Other"
            :FEC "Further education"
            :ISC "Special"
            :ISCR "Special"
            :IT "Other"
            :NON-SEND "Other"))

(defn sankey [{:keys [title] :or {title ""}} df]
  (gg4clj/render
   [[:require "ggforce"]
    [:<- :foo (gg4clj/data-frame df)]
    [:<- :palette [:c "#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7" "#1b9e77" "#d95f02" "#7570b3" "#e7298a"]]
    [:<- :palette [:c "#4ab276" "#4ab276" "#4ab276"  "#4ab276"  "#4ab276" ]]
    (gg4clj/r+
     [:ggplot :foo [:aes :x {:id :id :split [:factor :y] :value :value}]]
     [:scale_fill_manual {:values :palette}]
     [:geom_parallel_sets [:aes {:fill :setting}] {:alpha 0.5 :axis.width 0.1}]
     [:geom_parallel_sets_axes {:axis.width 0.2 :fill "#F6F6F6" :color "#DDDDDD"}]
     [:geom_parallel_sets_labels {:color "#444444" :angle 0 :size 2.5}]
     [:ggtitle title]
     [:theme {:axis.title.x [:element_blank]
              :legend.position "none"}])]))

(defn sankey-transitions [data calendar-year settings]
  (->> (remove v/joiner? data)
       (remove v/leaver?)
       (filter #(= (:calendar-year %) calendar-year))
       (filter #(= (:academic-year-1 %) 6))
       (map #(-> (update % :setting-1 settings) (update :setting-2 settings)))
       (gather-set-data)
       (seq-of-maps->data-frame)
       (sankey {:title (str "Aggregate setting transitions: " calendar-year "/" (apply str (drop 2 (str (inc calendar-year)))))}))
  (move-file "Rplots.pdf" (str "target/historic-transitions_" calendar-year ".pdf")))
