(ns witan.send.maths)

(def some+
  "x + y. Returns y if x is nil and x if y is nil."
  (fnil + 0))

(defn round [x]
  (Double/parseDouble (format "%.02f" (double x))))
