(ns witan.send.distributions
  (:require [kixi.stats.distribution :as d]))

(def random-seed (atom 0))
(defn set-seed! [n]
  (reset! random-seed n))
(defn get-seed! []
  (swap! random-seed inc))

(defn sample-dirichlet-multinomial
  [n alphas]
  (let [[ks as] (apply mapv vector alphas)
        xs (if (pos? n)
             (d/draw (d/dirichlet-multinomial {:n n :alphas as}) {:seed (get-seed!)})
             (repeat 0))]
    (zipmap ks xs)))

(defn sample-beta-binomial
  [n params]
  (if (pos? n)
    (d/draw (d/beta-binomial (assoc params :n n)) {:seed (get-seed!)})
    0))
