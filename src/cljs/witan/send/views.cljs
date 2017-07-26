(ns witan.send.views
  (:require [re-frame.core :as re-frame]))

(defn control-panel []
  (let [academic-year (re-frame/subscribe [:academic-year])
        leaver-weights (re-frame/subscribe [:leaver-weights])
        mover-alpha-weights (re-frame/subscribe [:mover-alpha-weights])
        mover-beta-weights (re-frame/subscribe [:mover-beta-weights])]
    (fn []
      (let [[w1 w2 w3] @leaver-weights
            [ma1 ma2] @mover-alpha-weights
            [mb1 mb2] @mover-beta-weights]
        [:table
         [:tbody
          [:tr
           [:td
            [:div "Academic year: " @academic-year]
            [:input {:type :range  :min -5 :max 25 :step 1
                     :on-change #(re-frame/dispatch [:set-academic-year (js/parseInt (.. % -target -value ))])}]

            [:div "Leaver Prior: " w1]
            [:input {:type :range  :min 0 :max 100 :step 1
                     :on-change #(re-frame/dispatch [:set-leaver-weights [(js/parseInt (.. % -target -value)) w2 w3]])}]

            [:div "Leaver Prior: " w2]
            [:input {:type :range  :min 0 :max 100 :step 1
                     :on-change #(re-frame/dispatch [:set-leaver-weights [w1 (js/parseInt (.. % -target -value)) w3]])}]
            ]
           [:td
            [:div "Mover alpha: " ma1]
            [:input {:type :range  :min 0 :max 100 :step 1
                     :on-change #(re-frame/dispatch [:set-mover-alpha-weights [(js/parseInt (.. % -target -value)) ma2]])}]

            [:div "Mover alpha: " ma2]
            [:input {:type :range  :min 0 :max 100 :step 1
                     :on-change #(re-frame/dispatch [:set-mover-alpha-weights [ma1 (js/parseInt (.. % -target -value))]])}]]

           [:td
            [:div "Mover beta: " mb1]
            [:input {:type :range  :min 0 :max 100 :step 1
                     :on-change #(re-frame/dispatch [:set-mover-beta-weights [(js/parseInt (.. % -target -value)) mb2]])}]

            [:div "Mover beta: " mb2]
            [:input {:type :range  :min 0 :max 100 :step 1
                     :on-change #(re-frame/dispatch [:set-mover-beta-weights [mb1 (js/parseInt (.. % -target -value))]])}]]

           [:td
            [:div "Joiner alpha: " ja1]
            [:input {:type :range  :min 0 :max 100 :step 1
                     :on-change #(re-frame/dispatch [:set-joiner-alpha-weights [(js/parseInt (.. % -target -value)) ja2]])}]

            [:div "Joiner alpha: " ja2]
            [:input {:type :range  :min 0 :max 100 :step 1
                     :on-change #(re-frame/dispatch [:set-joiner-alpha-weights [ja1 (js/parseInt (.. % -target -value))]])}]]

           [:td
            [:div "Joiner beta: " jb1]
            [:input {:type :range  :min 0 :max 100 :step 1
                     :on-change #(re-frame/dispatch [:set-joiner-beta-weights [(js/parseInt (.. % -target -value)) jb2]])}]

            [:div "Joiner beta: " jb2]
            [:input {:type :range  :min 0 :max 100 :step 1
                     :on-change #(re-frame/dispatch [:set-joiner-beta-weights [jb1 (js/parseInt (.. % -target -value))]])}]]]]]))))

(defn main-panel []
  (let [name (re-frame/subscribe [:name])]
    (fn []
      [:div
       [control-panel]])))



