(ns witan.send.subs
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [re-frame.core :as re-frame]))

(re-frame/reg-sub
 :name
 (fn [db]
   (:name db)))

(re-frame/reg-sub
 :academic-year
 (fn [db]
   (:academic-year db)))

(re-frame/reg-sub
 :leaver-weights
 (fn [db]
   (:leaver-weights db)))

(re-frame/reg-sub
 :mover-alpha-weights
 (fn [db]
   (:mover-alpha-weights db)))

(re-frame/reg-sub
 :mover-beta-weights
 (fn [db]
   (:mover-beta-weights db)))

(re-frame/reg-sub
 :joiner-alpha-weights
 (fn [db]
   (:joiner-alpha-weights db)))

(re-frame/reg-sub
 :joiner-beta-weights
 (fn [db]
   (:joiner-beta-weights db)))
