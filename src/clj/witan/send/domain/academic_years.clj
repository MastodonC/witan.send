(ns witan.send.domain.academic-years)


(defn ncy->age [ncy]
  (+ ncy 5))

(defn age->ncy [age]
  (- age 5))

(defn age-group [age]
  (cond
    (< age 5) "Age 0 to 5"
    (<= 5 age 10) "Age 05 to 10"
    (<= 11 age 15) "Age 11 to 15"
    (<= 16 age 19) "Age 16 to 19"
    (<= 20 age 25) "Age 20 to 25"
    (< 25 age) "Over 25"))

(def ay-lookup
  (sorted-map -5 -5
              -4 -4
              -3 -3
              -2 -2
              -1 -1
              0 0
              1 1
              2 2
              3 3
              4 4
              5 5
              6 6
              7 7
              8 8
              9 9
              10 10
              11 11
              12 12
              13 13
              14 14
              15 15
              16 16
              17 17
              18 18
              19 19
              20 20
              21 21))

(defn inclusive-range [beginning end]
  (range beginning (inc end)))

(def early-years
  (into (sorted-set) (inclusive-range -5 0)))

(def key-stage-1
  (into (sorted-set) (inclusive-range 1 2)))

(def key-stage-2
  (into (sorted-set) (inclusive-range 3 6)))

(def key-stage-3
  (into (sorted-set) (inclusive-range 7 9)))

(def key-stage-4
  (into (sorted-set) (inclusive-range 10 11)))

(def key-stage-5
  (into (sorted-set) (inclusive-range 12 14)))

(def ncy-15+
  (into (sorted-set) (inclusive-range 15 20)))

(def outside-of-send-age
  (into (sorted-set) (inclusive-range 21 99)))

(defn national-curriculum-stage [y]
  (cond
    (early-years y) :early-years
    (key-stage-1 y) :ks-1
    (key-stage-2 y) :ks-2
    (key-stage-3 y) :ks-3
    (key-stage-4 y) :ks-4
    (key-stage-5 y) :ks-5
    (ncy-15+ y) :ncy-15+
    :else :outside-of-send-age))

(def key-stage-names
  {:early-years "Early Years"
   :ks-1 "Key Stage 1"
   :ks-2 "Key Stage 2"
   :ks-3 "Key Stage 3"
   :ks-4 "Key Stage 4"
   :ks-5 "Key Stage 5"
   :ncy-15+ "NCY 15+"
   :outside-of-send-age "Outside of SEND age"})

;; Nursery - anything less than Reception/NCY 0
(def nursery
  (disj early-years 0))

;; Primary school - Reception and Key Stages 1 + 2 - Reception + Years 1 to 6
(def primary-school
  (into (sorted-set 0) (into key-stage-1 key-stage-2)))

;; Infant school - Reception and Key Stage 1 - Reception + Years 1 to 2
(def infant-school (into (sorted-set) (inclusive-range 0 2)))

;; Junior school - Key Stage 2 - Years 3 to 6
(def junior-school (into (sorted-set) (inclusive-range 3 6)))

;; First school - Reception + Years 1 to 4
(def first-school (into (sorted-set) (inclusive-range 0 4)))

;; Secondary school - Key Stages 3 + 4 - Years 7 to 11
(def secondary-school
  (into key-stage-3 key-stage-4))

;; Post 16 - Key Stage 5 - Years 12 to 14
(def post-16 key-stage-5)

;; Post 19 - Post Key Stage 5 up to 25 years of age - Years 15 to 20
(def post-19 ncy-15+)

;; School age - Reception + Key Stages 1 to 5
(def school-age
  (into (sorted-set) cat [primary-school secondary-school key-stage-5]))

;; School phase
(defn ncy->school-phase
  "National Curriculum Year to School Phase keyword"
  [ncy]
  (cond
    (nursery ncy)          :nursery
    (primary-school ncy)   :primary
    (secondary-school ncy) :secondary
    (post-16 ncy)          :post-16
    (post-19 ncy)          :post-19
    :else                  :outside-of-send-age
    ))

(def school-phase-names
  {:nursery   "Nursery"
   :primary   "Primary"
   :secondary "Secondary"
   :post-16   "Post 16"
   :post-19   "Post 19"})

(defn ncy->school-phase-name
  "National Curriculum Year to School Phase name"
  [ncy]
  (-> ncy ncy->school-phase school-phase-names))

;; School phase but using :early-years as keyword for NCYs -5 to -1
;; NOTE: Discordant with definition of early-years which includes Reception
(defn primary-secondary-post16-ncy15+ [ncy]
  (cond
    (nursery ncy) :early-years
    (primary-school ncy) :primary
    (secondary-school ncy) :secondary
    (key-stage-5 ncy) :post-16
    (ncy-15+ ncy) :post-19
    :else :outside-of-send-age))

(defn primary-secondary-postsecondary
  "Sometimes we don't care about whether someone is post 16 or post 19."
  [ncy]
  (cond
    (nursery ncy) :early-years
    (primary-school ncy) :primary
    (secondary-school ncy) :secondary
    ((into key-stage-5 ncy-15+) ncy) :post-secondary
    :else :outside-of-send-age))
