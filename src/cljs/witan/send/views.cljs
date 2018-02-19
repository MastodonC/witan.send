(ns witan.send.views
  (:require [re-frame.core :as re-frame]))

(defn control-panel []
  (let [academic-year (re-frame/subscribe [:academic-year])
        leaver-weights (re-frame/subscribe [:leaver-weights])
        mover-alpha-weights (re-frame/subscribe [:mover-alpha-weights])
        mover-beta-weights (re-frame/subscribe [:mover-beta-weights])
        joiner-alpha-weights (re-frame/subscribe [:joiner-alpha-weights])
        joiner-beta-weights (re-frame/subscribe [:joiner-beta-weights])]
    (fn []
      (let [[w1 w2 w3] @leaver-weights
            [ma1 ma2] @mover-alpha-weights
            [mb1 mb2] @mover-beta-weights
            [ja1 ja2] @joiner-alpha-weights
            [jb1 jb2] @joiner-beta-weights]
        [:table {:styles {:border "1px solid black"}}
         [:tbody
          [:tr
           [:td
            [:div "Academic year: " @academic-year]
            [:input {:type :range  :min -5 :max 25 :step 1
                     :value @academic-year
                     :on-change #(re-frame/dispatch [:set-academic-year (js/parseInt (.. % -target -value ))])}]

            [:div "Leaver Prior (Overall): " w1]
            [:input {:type :range  :min 0 :max 100 :step 1
                     :value w1
                     :on-change #(re-frame/dispatch [:set-leaver-weights [(js/parseInt (.. % -target -value)) w2 w3]])}]

            [:div "Leaver Prior (By NCY): " w2]
            [:input {:type :range  :min 0 :max 100 :step 1
                     :value w2
                     :on-change #(re-frame/dispatch [:set-leaver-weights [w1 (js/parseInt (.. % -target -value)) w3]])}]
            
            [:div "Leaver Prior (Uniform): " w3]
            [:input {:type :range  :min 0 :max 100 :step 1
                     :value w3
                     :on-change #(re-frame/dispatch [:set-leaver-weights [w1 w2 (js/parseInt (.. % -target -value))]])}]
            ]
           [:td
            [:div "Mover alpha (Overall): " ma1]
            [:input {:type :range  :min 0 :max 100 :step 1
                     :value ma1
                     :on-change #(re-frame/dispatch [:set-mover-alpha-weights [(js/parseInt (.. % -target -value)) ma2]])}]

            [:div "Mover alpha (By NCY): " ma2]
            [:input {:type :range  :min 0 :max 100 :step 1
                     :value ma2
                     :on-change #(re-frame/dispatch [:set-mover-alpha-weights [ma1 (js/parseInt (.. % -target -value))]])}]]

           [:td
            [:div "Mover beta (Overall): " mb1]
            [:input {:type :range  :min 0 :max 100 :step 1
                     :value mb1
                     :on-change #(re-frame/dispatch [:set-mover-beta-weights [(js/parseInt (.. % -target -value)) mb2]])}]

            [:div "Mover beta (By NCY): " mb2]
            [:input {:type :range  :min 0 :max 100 :step 1
                     :value mb2
                     :on-change #(re-frame/dispatch [:set-mover-beta-weights [mb1 (js/parseInt (.. % -target -value))]])}]]

           #_[:td
            [:div "Joiner alpha (Overall): " ja1]
            [:input {:type :range  :min 0 :max 100 :step 1
                     :value ja1
                     :on-change #(re-frame/dispatch [:set-joiner-alpha-weights [(js/parseInt (.. % -target -value)) ja2]])}]

            [:div "Joiner alpha (By NCY): " ja2]
            [:input {:type :range  :min 0 :max 100 :step 1
                     :value ja2
                     :on-change #(re-frame/dispatch [:set-joiner-alpha-weights [ja1 (js/parseInt (.. % -target -value))]])}]]

           #_[:td
            [:div "Joiner beta (Overall): " jb1]
            [:input {:type :range  :min 0 :max 100 :step 1
                     :value jb1
                     :on-change #(re-frame/dispatch [:set-joiner-beta-weights [(js/parseInt (.. % -target -value)) jb2]])}]

            [:div "Joiner beta (By NCY): " jb2]
            [:input {:type :range  :min 0 :max 100 :step 1
                     :value jb2
                     :on-change #(re-frame/dispatch [:set-joiner-beta-weights [jb1 (js/parseInt (.. % -target -value))]])}]]]]]))))

(defn main-panel []
  (let [name (re-frame/subscribe [:name])]
    (fn []
      [:div
       [control-panel]])))



