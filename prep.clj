;; gorilla-repl.fileformat = 1

;; **
;;; # 40K View of Input Preparation
;; **

;; @@
(ns witan.send.explore-model.gorilla
  (:require [witan.send.model.input :as i]
            [witan.send.model.run :as r]
            [witan.send.report :refer [reset-send-report]]
            [witan.send.main :as m]
            [clojure.pprint :refer [pprint]]
            [clojure.core.matrix.impl.pprint :as pp]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; Some helper functions for viewing data
;; **

;; @@
(defn types [m]
  "Build a map replacing the values with there types"
  (into {} (for [[k v] m]
             [k (type v)])))

(defn ppds [ds]
  "Pretty print a datastore"
  (str (pp/pm (:column-names ds)) "\n" (pp/pm ds)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;witan.send.explore-model.gorilla/ppds</span>","value":"#'witan.send.explore-model.gorilla/ppds"}
;; <=

;; **
;;; ##Loading The Data
;;; 
;;; Load our config
;; **

;; @@
(def ss (m/config "/home/matt/src/github/mastodonc/witan.send-runs/mastodonc/simple-examples/two-states.edn"))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;witan.send.explore-model.gorilla/ss</span>","value":"#'witan.send.explore-model.gorilla/ss"}
;; <=

;; **
;;; Explore the config file a little
;; **

;; @@
(pprint (keys ss))
;; @@
;; ->
;;; (:file-inputs
;;;  :transition-parameters
;;;  :run-parameters
;;;  :output-parameters
;;;  :schema-inputs
;;;  :project-dir)
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(pprint (:file-inputs ss))
;; @@
;; ->
;;; {:transition-matrix
;;;  &quot;data/two-states/transitions-regular-movement-two-states.csv&quot;,
;;;  :population &quot;data/population-static.csv&quot;,
;;;  :setting-cost &quot;data/two-states/need-setting-costs-two-states.csv&quot;,
;;;  :valid-setting-academic-years
;;;  &quot;data/two-states/valid-setting-two-states.csv&quot;}
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; The first step is to build the input-datasets
;; **

;; @@
(def input-datasets (i/build-input-datasets (:project-dir ss) (:file-inputs ss) (:schema-inputs ss)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;witan.send.explore-model.gorilla/input-datasets</span>","value":"#'witan.send.explore-model.gorilla/input-datasets"}
;; <=

;; **
;;; What do these look like?
;; **

;; @@
(pprint (types input-datasets))
;; @@
;; ->
;;; {:transition-matrix clojure.core.matrix.impl.dataset.DataSet,
;;;  :population clojure.core.matrix.impl.dataset.DataSet,
;;;  :setting-cost clojure.core.matrix.impl.dataset.DataSet,
;;;  :valid-setting-academic-years clojure.core.matrix.impl.dataset.DataSet}
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; ### The Transition Matrix
;;; 
;; **

;; @@
(print (ppds (:transition-matrix input-datasets)))
;; @@
;; ->
;;; [:calendar-year :setting-1 :need-1 :academic-year-1 :setting-2 :need-2 :academic-year-2]
;;; [[2014.000  :NONSEND :NONSEND 0.000 :settingA   :needB 1.000]
;;;  [2014.000  :NONSEND :NONSEND 0.000 :settingA   :needB 1.000]
;;;  [2014.000  :NONSEND :NONSEND 0.000 :settingA   :needB 1.000]
;;;  [2014.000  :NONSEND :NONSEND 0.000 :settingC   :needD 1.000]
;;;  [2014.000  :NONSEND :NONSEND 0.000 :settingC   :needD 1.000]
;;;  [2014.000 :settingA   :needB 1.000 :settingA   :needB 2.000]
;;;  [2014.000 :settingA   :needB 1.000 :settingA   :needB 2.000]
;;;  [2014.000 :settingA   :needB 1.000 :settingC   :needD 2.000]
;;;  [2014.000 :settingC   :needD 1.000 :settingC   :needD 2.000]
;;;  [2014.000 :settingC   :needD 1.000 :settingC   :needD 2.000]
;;;  [2014.000 :settingA   :needB 2.000 :settingA   :needB 3.000]
;;;  [2014.000 :settingA   :needB 2.000 :settingA   :needB 3.000]
;;;  [2014.000 :settingC   :needD 2.000 :settingC   :needD 3.000]
;;;  [2014.000 :settingC   :needD 2.000 :settingC   :needD 3.000]
;;;  [2014.000 :settingC   :needD 2.000 :settingC   :needD 3.000]
;;;  [2014.000 :settingA   :needB 3.000 :settingA   :needB 4.000]
;;;  [2014.000 :settingA   :needB 3.000 :settingA   :needB 4.000]
;;;  [2014.000 :settingC   :needD 3.000 :settingC   :needD 4.000]
;;;  [2014.000 :settingC   :needD 3.000 :settingC   :needD 4.000]
;;;  [2014.000 :settingC   :needD 3.000 :settingA   :needB 4.000]
;;;  [2014.000 :settingA   :needB 4.000  :NONSEND :NONSEND 5.000]
;;;  [2014.000 :settingA   :needB 4.000  :NONSEND :NONSEND 5.000]
;;;  [2014.000 :settingC   :needD 4.000  :NONSEND :NONSEND 5.000]
;;;  [2014.000 :settingC   :needD 4.000  :NONSEND :NONSEND 5.000]
;;;  [2014.000 :settingA   :needB 4.000  :NONSEND :NONSEND 5.000]
;;;  [2015.000  :NONSEND :NONSEND 0.000 :settingA   :needB 1.000]
;;;  [2015.000  :NONSEND :NONSEND 0.000 :settingA   :needB 1.000]
;;;  [2015.000  :NONSEND :NONSEND 0.000 :settingA   :needB 1.000]
;;;  [2015.000  :NONSEND :NONSEND 0.000 :settingC   :needD 1.000]
;;;  [2015.000  :NONSEND :NONSEND 0.000 :settingC   :needD 1.000]
;;;  [2015.000 :settingA   :needB 1.000 :settingA   :needB 2.000]
;;;  [2015.000 :settingA   :needB 1.000 :settingA   :needB 2.000]
;;;  [2015.000 :settingA   :needB 1.000 :settingC   :needD 2.000]
;;;  [2015.000 :settingC   :needD 1.000 :settingC   :needD 2.000]
;;;  [2015.000 :settingC   :needD 1.000 :settingC   :needD 2.000]
;;;  [2015.000 :settingA   :needB 2.000 :settingA   :needB 3.000]
;;;  [2015.000 :settingA   :needB 2.000 :settingA   :needB 3.000]
;;;  [2015.000 :settingC   :needD 2.000 :settingC   :needD 3.000]
;;;  [2015.000 :settingC   :needD 2.000 :settingC   :needD 3.000]
;;;  [2015.000 :settingC   :needD 2.000 :settingC   :needD 3.000]
;;;  [2015.000 :settingA   :needB 3.000 :settingA   :needB 4.000]
;;;  [2015.000 :settingA   :needB 3.000 :settingA   :needB 4.000]
;;;  [2015.000 :settingC   :needD 3.000 :settingC   :needD 4.000]
;;;  [2015.000 :settingC   :needD 3.000 :settingC   :needD 4.000]
;;;  [2015.000 :settingC   :needD 3.000 :settingA   :needB 4.000]
;;;  [2015.000 :settingA   :needB 4.000  :NONSEND :NONSEND 5.000]
;;;  [2015.000 :settingA   :needB 4.000  :NONSEND :NONSEND 5.000]
;;;  [2015.000 :settingC   :needD 4.000  :NONSEND :NONSEND 5.000]
;;;  [2015.000 :settingC   :needD 4.000  :NONSEND :NONSEND 5.000]
;;;  [2015.000 :settingA   :needB 4.000  :NONSEND :NONSEND 5.000]
;;;  [2016.000  :NONSEND :NONSEND 0.000 :settingA   :needB 1.000]
;;;  [2016.000  :NONSEND :NONSEND 0.000 :settingA   :needB 1.000]
;;;  [2016.000  :NONSEND :NONSEND 0.000 :settingA   :needB 1.000]
;;;  [2016.000  :NONSEND :NONSEND 0.000 :settingC   :needD 1.000]
;;;  [2016.000  :NONSEND :NONSEND 0.000 :settingC   :needD 1.000]
;;;  [2016.000 :settingA   :needB 1.000 :settingA   :needB 2.000]
;;;  [2016.000 :settingA   :needB 1.000 :settingA   :needB 2.000]
;;;  [2016.000 :settingA   :needB 1.000 :settingC   :needD 2.000]
;;;  [2016.000 :settingC   :needD 1.000 :settingC   :needD 2.000]
;;;  [2016.000 :settingC   :needD 1.000 :settingC   :needD 2.000]
;;;  [2016.000 :settingA   :needB 2.000 :settingA   :needB 3.000]
;;;  [2016.000 :settingA   :needB 2.000 :settingA   :needB 3.000]
;;;  [2016.000 :settingC   :needD 2.000 :settingC   :needD 3.000]
;;;  [2016.000 :settingC   :needD 2.000 :settingC   :needD 3.000]
;;;  [2016.000 :settingC   :needD 2.000 :settingC   :needD 3.000]
;;;  [2016.000 :settingA   :needB 3.000 :settingA   :needB 4.000]
;;;  [2016.000 :settingA   :needB 3.000 :settingA   :needB 4.000]
;;;  [2016.000 :settingC   :needD 3.000 :settingC   :needD 4.000]
;;;  [2016.000 :settingC   :needD 3.000 :settingC   :needD 4.000]
;;;  [2016.000 :settingC   :needD 3.000 :settingA   :needB 4.000]
;;;  [2016.000 :settingA   :needB 4.000  :NONSEND :NONSEND 5.000]
;;;  [2016.000 :settingA   :needB 4.000  :NONSEND :NONSEND 5.000]
;;;  [2016.000 :settingC   :needD 4.000  :NONSEND :NONSEND 5.000]
;;;  [2016.000 :settingC   :needD 4.000  :NONSEND :NONSEND 5.000]
;;;  [2016.000 :settingA   :needB 4.000  :NONSEND :NONSEND 5.000]]
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; The transition matrix, above, models how students move between years.  Columns 2,3 and 4 can be merged into one like so `[0 :settingA-needB]`.  Doing the same for columns 5,6 and 7 gives us `[1 :settingA-needB]`.  This row therefore represents the transition:
;;; 
;;;     [0 :settingA-needB] -> [1 :settingA-needB]
;;;     
;;; This tells us one student in academic year 0 transitioned to academic year 1 and kept the setting and need the same.
;;; 
;;; ### Population Matrix
;; **

;; @@
(print (ppds (:population input-datasets)))
;; @@
;; ->
;;; [:calendar-year :academic-year :population]
;;; [[2014.000 0.000  7000.000]
;;;  [2014.000 1.000  7000.000]
;;;  [2014.000 2.000  7000.000]
;;;  [2014.000 3.000  7000.000]
;;;  [2014.000 4.000  7000.000]
;;;  [2014.000 5.000  7000.000]
;;;  [2015.000 0.000 70000.000]
;;;  [2015.000 1.000 70000.000]
;;;  [2015.000 2.000 70000.000]
;;;  [2015.000 3.000 70000.000]
;;;  [2015.000 4.000 70000.000]
;;;  [2015.000 5.000 70000.000]
;;;  [2016.000 0.000 80000.000]
;;;  [2016.000 1.000 80000.000]
;;;  [2016.000 2.000 80000.000]
;;;  [2016.000 3.000 80000.000]
;;;  [2016.000 4.000 80000.000]
;;;  [2016.000 5.000  8000.000]
;;;  [2017.000 0.000  7000.000]
;;;  [2017.000 1.000  7000.000]
;;;  [2017.000 2.000  7000.000]
;;;  [2017.000 3.000  7000.000]
;;;  [2017.000 4.000  7000.000]
;;;  [2017.000 5.000  7000.000]
;;;  [2018.000 0.000  7000.000]
;;;  [2018.000 1.000  7000.000]
;;;  [2018.000 2.000  7000.000]
;;;  [2018.000 3.000  7000.000]
;;;  [2018.000 4.000  7000.000]
;;;  [2018.000 5.000  7000.000]
;;;  [2019.000 0.000  7000.000]
;;;  [2019.000 1.000  7000.000]
;;;  [2019.000 2.000  7000.000]
;;;  [2019.000 3.000  7000.000]
;;;  [2019.000 4.000  7000.000]
;;;  [2019.000 5.000  7000.000]
;;;  [2020.000 0.000 70000.000]
;;;  [2020.000 1.000 70000.000]
;;;  [2020.000 2.000 70000.000]
;;;  [2020.000 3.000 70000.000]
;;;  [2020.000 4.000 70000.000]
;;;  [2020.000 5.000 70000.000]
;;;  [2021.000 0.000 70000.000]
;;;  [2021.000 1.000 70000.000]
;;;  [2021.000 2.000 70000.000]
;;;  [2021.000 3.000 70000.000]
;;;  [2021.000 4.000 70000.000]
;;;  [2021.000 5.000 70000.000]
;;;  [2022.000 0.000 70000.000]
;;;  [2022.000 1.000 70000.000]
;;;  [2022.000 2.000 70000.000]
;;;  [2022.000 3.000 70000.000]
;;;  [2022.000 4.000 70000.000]
;;;  [2022.000 5.000 70000.000]]
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(print (ppds (:settings-cost input-datasets)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(print (ppds (:valid-setting-academic-years input-datasets)))
;; @@
;; ->
;;; [:setting :setting-group :min-academic-year :max-academic-year :needs :setting-&gt;setting]
;;; [[:settingA Other 0.000 5.000 needB settingA,settingC]
;;;  [:settingC Other 0.000 5.000 needD settingA,settingC]]
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; This matrix limits the state space and is particular important to get right.
;;; 
;;; Omiting year 0 (lowest AY) will cause the joiner rate distribution to be skewed (and others).  The final column is not a mapping (as the header suggests) but rather a set of all settings the current setting can map to.  For example, in this case A can go to itself and to C.
;;; 
;;; The DSL used here to implement restrictions is not powerful enough to fully restrict state spaces as it doesn't take into account needs. In this case the states allowed by this file are
;;; 
;;;     [AB] [CA] [CD] [AD]
;;;     
;;; Whereas our observation data only has `[AB]` and `[CD]`
;;; 
;;; By not being able to fix needs we have to states `[CA]` adn `[AD]` in our model we've no real interest in.  These will actually come up in probabiltiy distributions used to model transitions.  However interestingly they don't come up in the final result graphs and outputs but we do see them in transitions - something strange is happening here and needs investigating. 
;; **

;; **
;;; ## Preparing the Inputs
;; **

;; @@
(def prepared-inputs (i/prepare-send-inputs input-datasets (:transition-parameters ss)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;witan.send.explore-model.gorilla/prepared-inputs</span>","value":"#'witan.send.explore-model.gorilla/prepared-inputs"}
;; <=

;; **
;;; Where
;; **

;; @@
(pprint (:transition-parameters ss))
;; @@
;; ->
;;; {:filter-transitions-from nil,
;;;  :which-transitions? nil,
;;;  :splice-ncy nil,
;;;  :modify-transition-by 1}
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; The results are
;; **

;; @@
(type prepared-inputs)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-class'>clojure.lang.PersistentArrayMap</span>","value":"clojure.lang.PersistentArrayMap"}
;; <=

;; @@
(pprint (types prepared-inputs))
;; @@
;; ->
;;; {:standard-projection clojure.lang.PersistentHashMap,
;;;  :scenario-projection nil,
;;;  :modify-transition-by java.lang.Long,
;;;  :settings-to-change nil}
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; What is `:standard-projection`?
;; **

;; @@
(pprint (types (:standard-projection prepared-inputs)))
;; @@
;; ->
;;; {:mover-state-alphas clojure.lang.PersistentHashMap,
;;;  :population-by-age-state clojure.lang.PersistentArrayMap,
;;;  :leaver-beta-params clojure.lang.PersistentHashMap,
;;;  :joiner-state-alphas clojure.lang.PersistentArrayMap,
;;;  :setting-cost-lookup clojure.lang.PersistentArrayMap,
;;;  :transition-matrix clojure.core.matrix.impl.dataset.DataSet,
;;;  :projected-population clojure.lang.PersistentHashMap,
;;;  :valid-setting-academic-years
;;;  clojure.core.matrix.impl.dataset.DataSet,
;;;  :population clojure.core.matrix.impl.dataset.DataSet,
;;;  :mover-beta-params clojure.lang.PersistentHashMap,
;;;  :joiner-beta-params clojure.lang.PersistentArrayMap}
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; This is much more interesting!
;;; 
;;; First we see that the datasets are all unchanged from the initial creation
;; **

;; @@
(= (:transition-matrix (:standard-projection prepared-inputs))
   (:transition-matrix input-datasets))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"}
;; <=

;; @@
(= (:valid-setting-academic-years (:standard-projection prepared-inputs))
   (:valid-setting-academic-years input-datasets))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"}
;; <=

;; @@
(= (:population (:standard-projection prepared-inputs))
   (:population input-datasets))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"}
;; <=

;; **
;;; What are the new keys introduced?
;; **

;; @@
(def added-sp-keys (clojure.set/difference
                     (set (keys (:standard-projection prepared-inputs)))
                     (set (keys input-datasets))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;witan.send.explore-model.gorilla/added-sp-keys</span>","value":"#'witan.send.explore-model.gorilla/added-sp-keys"}
;; <=

;; @@
(pprint added-sp-keys)
;; @@
;; ->
;;; #{:mover-state-alphas :population-by-age-state :leaver-beta-params
;;;   :joiner-state-alphas :setting-cost-lookup :projected-population
;;;   :mover-beta-params :joiner-beta-params}
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(pprint (types (select-keys (:standard-projection prepared-inputs) added-sp-keys)))
;; @@
;; ->
;;; {:mover-state-alphas clojure.lang.PersistentHashMap,
;;;  :population-by-age-state clojure.lang.PersistentArrayMap,
;;;  :leaver-beta-params clojure.lang.PersistentHashMap,
;;;  :joiner-state-alphas clojure.lang.PersistentArrayMap,
;;;  :setting-cost-lookup clojure.lang.PersistentArrayMap,
;;;  :projected-population clojure.lang.PersistentHashMap,
;;;  :mover-beta-params clojure.lang.PersistentHashMap,
;;;  :joiner-beta-params clojure.lang.PersistentArrayMap}
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; At a 40k view we are not so much interested in how these maps are calculated but rather what they represent.
;;; 
;;; ### Population by Age State
;; **

;; **
;;; 
;; **

;; @@
(pprint (:population-by-age-state (:standard-projection prepared-inputs)))
;; @@
;; ->
;;; {[1 :needB-settingA] 3,
;;;  [1 :needD-settingC] 2,
;;;  [2 :needB-settingA] 2,
;;;  [2 :needD-settingC] 3,
;;;  [3 :needB-settingA] 2,
;;;  [3 :needD-settingC] 3,
;;;  [4 :needB-settingA] 3,
;;;  [4 :needD-settingC] 2}
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; This matrix represents a graph of the population. 
;;; 
;;; ![Graph](https://github.com/MastodonC/witan.send/blob/gorilla-prep/gorilla/transition-pop-graph.png?raw=true)
;;; 
;;; It does not contain any information about where the increases in population for one state came from.  However in this simple graph a solution can easily be inferred (not the only solution).  In general they can not (i dont' think).  The transition matrix would be needed for this.
;; **

;; **
;;; ### Leaver Beta Parameters
;;; 
;; **

;; @@
(pprint  (sort (:leaver-beta-params (:standard-projection prepared-inputs))))
;; @@
;; ->
;;; ([[1 :needB-settingA] {:beta 9.96875, :alpha 0.03125}]
;;;  [[1 :needD-settingC] {:beta 6.96875, :alpha 0.03125}]
;;;  [[2 :needB-settingA] {:beta 6.96875, :alpha 0.03125}]
;;;  [[2 :needD-settingC] {:beta 9.96875, :alpha 0.03125}]
;;;  [[3 :needB-settingA] {:beta 6.96875, :alpha 0.03125}]
;;;  [[3 :needD-settingC] {:beta 9.96875, :alpha 0.03125}]
;;;  [[4 :needB-settingA] {:alpha 9.96875, :beta 0.03125}]
;;;  [[4 :needD-settingC] {:alpha 6.96875, :beta 0.03125}]
;;;  [[5 :needB-settingA] {:alpha 0.96875, :beta 0.03125}]
;;;  [[5 :needD-settingC] {:alpha 0.96875, :beta 0.03125}])
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; To model people leaving SEND, with very limited data and sometimes no data, we have to introduce some expectation. For example if no one leaves SEND in year 2 it's unreasonable to conclude the probability of leaving is 0.  The beta distribution is used to provide the probability of the probablity-of-leaving.
;;; 
;;; Looking at `[1 :BA]`
;;; 
;;; ![1ba beta dist](https://github.com/MastodonC/witan.send/blob/gorilla-prep/gorilla/1ba-leaver-beta.png?raw=true)
;;; 
;;; The graph shows the probability of someone successfully leaving is likely between 0 and 0.1 - so very low but not zero as the actual data shows.
;;; 
;;; Looking at `[5 :BA]`
;;; 
;;; ![5ba beta dist](https://github.com/MastodonC/witan.send/blob/gorilla-prep/gorilla/5ba-leaver-beta.png?raw=true)
;;; 
;;; The graph shows the probability of someone successfully leaving is likely between 0.9 and 1.0 - so very high but not zero as the actual data shows.
;;; 
;;; The remaining state probabilities take the form of very likely or very unlikely for this sample set.
;;; 
;;; 
;;; 
;;; 
;;; 
;; **

;; **
;;; ### Mover Beta Parameters
;; **

;; @@
(pprint  (sort (:mover-beta-params (:standard-projection prepared-inputs))))
;; @@
;; ->
;;; ([[1 :needB-settingA] {:beta 6.78125, :alpha 3.21875}]
;;;  [[1 :needD-settingC] {:beta 6.78125, :alpha 0.21875}]
;;;  [[2 :needB-settingA] {:beta 6.96875, :alpha 0.03125}]
;;;  [[2 :needD-settingC] {:beta 9.96875, :alpha 0.03125}]
;;;  [[3 :needB-settingA] {:beta 6.78125, :alpha 0.21875}]
;;;  [[3 :needD-settingC] {:beta 6.78125, :alpha 3.21875}]
;;;  [[4 :needB-settingA] {:alpha 0.21875, :beta 0.78125}]
;;;  [[4 :needD-settingC] {:alpha 0.21875, :beta 0.78125}]
;;;  [[5 :needB-settingA] {:alpha 0.21875, :beta 0.78125}]
;;;  [[5 :needD-settingC] {:alpha 0.21875, :beta 0.78125}])
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; The probability of someone moving placements is done in the same way as the leavers using a beta distribution.
;;; 
;;; Looking at `[1 :ba]`
;;; 
;;; ![1ba-movers-beta](https://github.com/MastodonC/witan.send/blob/gorilla-prep/gorilla/1ba-mover-beta.png?raw=true)
;;; 
;;; We see that the probability that someone moves is between 0.25 and 0.42 and the expected value is 0.33.  This matches the fact that population count directed graph shows that 1 out of the 3 students in `[1 :ba]` moved.
;;; 
;;; Looking at `[1 :dc]` 
;;; 
;;; ![1dc-movers-beta](https://github.com/MastodonC/witan.send/blob/gorilla-prep/gorilla/1dc-mover-beta.png?raw=true)
;;; 
;;; We see a probabilty graph that represents we saw 0 movers but has used the prior expectation to say it can't actually be 0.
;;; 
;;; 
;;; We also need to know the probabilities where someone will actually move to.
;; **

;; **
;;; ### Mover Alpha Parameters
;; **

;; @@
(pprint  (sort (:mover-state-alphas (:standard-projection prepared-inputs))))
;; @@
;; ->
;;; ([[0 :needB-settingA] {}]
;;;  [[0 :needD-settingC] {}]
;;;  [[1 :needB-settingA] {:needB-settingC 1.0, :needD-settingC 3}]
;;;  [[1 :needD-settingC] {}]
;;;  [[2 :needB-settingA] {:needB-settingC 1.0}]
;;;  [[2 :needD-settingC] {}]
;;;  [[3 :needB-settingA] {}]
;;;  [[3 :needD-settingC] {:needD-settingA 1.0, :needB-settingA 3}]
;;;  [[4 :needB-settingA] {}]
;;;  [[4 :needD-settingC] {:needD-settingA 1.0}]
;;;  [[5 :needB-settingA] {}]
;;;  [[5 :needD-settingC] {:needD-settingA 1.0}])
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; The Alpha parameters are inputs into the Dirichlet distribution - which is the multinomial equivalent to the Beta distribution.  i.e, it works on multiple outcomes.
;;; 
;;; Like the Beta distribution it allows us to sample the probabilities a mover will go to a partiuclar state.
;;; 
;;; For `[1 :BA]` we can write the alphas as `4 * (0.25, 0.75)`.  This reads as the mean probability of going from `[1 :BA] -> [BC]` is 0.25 and `[1 :BA] -> [DC]` is 0.75.
;;; 
;;; 
;;; For `[1 :DC]` we have `4 * (0.25, 0.75)`, so `[1 :DC] -> [:DA]` is 0.25 and `[1 :DC] -> [BA]` is 0.75.
;;; 
;;; I'm unable to properly explain why the `[2: BA] -> BC` has a probabilty of 1.  This is a state that doesn't exist in the transitions but does exist as an allowed state (see the section input data discussing this).  Perhaps the probabilty of going to the other states from the observed data is 0 and so it's mass is all dumped here?
;;; 
;;; 
;;; 
;; **

;; **
;;; ### Joiner Beta Parameters
;; **

;; @@
(pprint  (sort (:joiner-beta-params (:standard-projection prepared-inputs))))
;; @@
;; ->
;;; ([1 {:alpha 5N, :beta 6995N}]
;;;  [2 {:alpha 5N, :beta 6995N}]
;;;  [3 {:alpha 5N, :beta 6995N}]
;;;  [4 {:alpha 5N, :beta 6995N}]
;;;  [5 {:alpha 5N, :beta 6995N}])
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; Note: It's pretty important that valid settings allows for year 0 elsewise these results don't make sense.
;;; 
;;; ![joiner-beta-dist](https://github.com/MastodonC/witan.send/blob/gorilla-prep/gorilla/joiner-beta-ay-1.png?raw=true)
;;; 
;;; The data shows currently 5 joiners per year.  With a static population of 7000 and the expected probabilty of 0.0007 we have `7000 * 0.0007 = 5`.
;;; 
;;; The repeated beta parameters here are perculiar.  Each years distribution is the same.  This is due to the Academic Year 1 being copied to Year 2 when no data is present.  This happens for all 5 Academic years.  In reality the Joiner rates are much higher for year 1 and much lower for the subsequent years - the copy of the distrubtion at least for Joiners does not feel like the correct thing to do.  Also the function to copy the distribution is used in movers and leavers also - these should be visited also.
;;; 
;;; A first pass at a fix has been put in place.
;; **

;; **
;;; ### Joiner Alpha Parameters
;; **

;; @@
(pprint  (sort (:joiner-state-alphas (:standard-projection prepared-inputs))))
;; @@
;; ->
;;; ([1 {:needB-settingA 9.5, :needD-settingC 6.5}]
;;;  [2 {:needB-settingA 9.5, :needD-settingC 6.5}]
;;;  [3 {:needB-settingA 9.5, :needD-settingC 6.5}]
;;;  [4 {:needB-settingA 9.5, :needD-settingC 6.5}]
;;;  [5 {:needB-settingA 9.5, :needD-settingC 6.5}])
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; This distribution says that on average 60% of the joiners will go to BA and 40% to DC.  However as before it doesn't seem reasonable to copy these distributions to subseqent academic years.
;; **
