(ns witan.send.constants)

(def non-send :NONSEND)

(def academic-years
  (range -4 (inc 21)))

(def min-academic-year (apply min academic-years))

(def max-academic-year (apply max academic-years))
