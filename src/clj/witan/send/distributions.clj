(ns witan.send.distributions
  (:require [kixi.stats.distribution :as d]))

(def random-seed (atom 0))
(defn set-seed! [n]
  (reset! random-seed n))
(defn get-seed! []
  (swap! random-seed inc))

(defn sample-dirichlet-multinomial
  [n alphas]
  (when-not (seq alphas) (throw (ex-info "Missing alphas" {:n n :alphas alphas})))
  (let [[ks as] (apply mapv vector alphas)
        xs (if (pos? n)
             (d/draw (d/dirichlet-multinomial {:n n :alphas as}) #_(d/dirichlet-multinomial n as) {:seed (get-seed!)})
             (repeat 0))]
    (zipmap ks xs)))

(defn sample-beta-binomial
  [n {:keys [alpha beta]} #_params]
  (if (pos? n)
    (d/draw (d/beta-binomial {:n n :alpha alpha :beta beta}) #_(d/beta-binomial n params) {:seed (get-seed!)})
    0))
