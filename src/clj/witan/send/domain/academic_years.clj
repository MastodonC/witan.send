(ns witan.send.domain.academic-years)

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
    (ncy-15+ y) :further-education))
