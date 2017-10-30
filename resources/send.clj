;; gorilla-repl.fileformat = 1

;; **
;;; # Gorilla REPL
;;; 
;;; Welcome to gorilla :-)
;;; 
;;; Shift + enter evaluates code. Hit ctrl+g twice in quick succession or click the menu icon (upper-right corner) for more commands ...
;;; 
;;; It's a good habit to run each worksheet in its own namespace: feel free to use the declaration we've provided below if you'd like.
;; **

;; @@
(ns witan-send
  (:require [gorilla-plot.core :as plot]
            [kixi.stats.core :as kixi]
            [kixi.stats.random :as r]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
;; This is the variance we want to duplicate

(double (transduce identity kixi/mean [-5 21 54])) ;; Avg growth: 23

(double (transduce identity kixi/variance [-5 21 54])) ;; Avg growth: 23


;; Joiners is one beta-binomial

;; Leavers is another beta-binomial

;; mean = na / (a + b)
;; variance = nab(n + a + b) / (a + b)^2(a + b + 1)

;; avg growth = mean joiners - mean leavers
;; variance = variance joiners + variance leavers

;; 1 : joiners, 2 : leavers
;; beta1 = n1 - alpha1
;; beta2 = n2 - alpha1

;; n1a1 / (a1 + b1) = n2a2 / (a2 + b2) + G
;; n1a1 / (a1 + n1 - a1) = n2a2 / (a2 + n2 - a2) + G
;; n1a1 / n1 = n2a2 / n2 + G
;; a1 = a2 + G
;; a2 = a1 - G


;; n1a1b1(n1 + a1 + b1) / (a1 + b1)^2(a1 + b1 + 1) + n2a2b2(n2 + a2 + b2) / (a2 + b2)^2(a2 + b2 + 1) = 874
;; n1a1(n1-a1)(2n1) / n1^2(n1+1) + n2(a1-G)(n2-a1-G)(2n2) / n2^2(n2+1) = 874
;; n1^2a1(2n1) - a1^2n1(2n1) / (n1^3+n1^2) + (n2a1 - n2G)(n2 - a1 - G)(2n2) / (n2^3 + n2^2) = 874

;; n1 = n
;; n2 = m
;; a1 = a


;; na(n-a)(2n) / n^2(n+1) + m(a-G)(m-a-G)(2m) / m^2(m+1) = 874


(defn f []
  (- (r/draw (r/beta-binomial 55665 {:alpha 100 :beta 55565}))
     (r/draw (r/beta-binomial 1039 {:alpha 78 :beta 961}))))

(defn f []
  (- (r/draw (r/beta-binomial 55665 {:alpha (/ 100 4) :beta (/ 55565 4)}))
     (r/draw (r/beta-binomial 1039 {:alpha (/ 78 4) :beta (/ 961 4)}))))

(defn sq [x] (*' x x))


(defn g []
  (r/draw (r/beta-binomial 55665 {:alpha (/ 100 4) :beta (/ 55565 4)})))

(def xs (take 10000 (repeatedly f)))
  
(def xs' (take 10000 (repeatedly g)))

(double (transduce identity kixi/mean xs))
(double (transduce identity kixi/variance xs))

(plot/histogram xs)
  
(plot/histogram xs')
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"46bc8540-35b4-4195-87c2-42130fa5c014","values":[{"x":36.0,"y":0},{"x":47.06666666666667,"y":22.0},{"x":58.13333333333334,"y":143.0},{"x":69.2,"y":508.0},{"x":80.26666666666667,"y":1216.0},{"x":91.33333333333333,"y":1780.0},{"x":102.39999999999999,"y":1954.0},{"x":113.46666666666665,"y":1756.0},{"x":124.53333333333332,"y":1246.0},{"x":135.6,"y":724.0},{"x":146.66666666666666,"y":381.0},{"x":157.73333333333332,"y":160.0},{"x":168.79999999999998,"y":70.0},{"x":179.86666666666665,"y":30.0},{"x":190.9333333333333,"y":7.0},{"x":201.99999999999997,"y":2.0},{"x":213.06666666666663,"y":1.0},{"x":224.1333333333333,"y":0}]}],"marks":[{"type":"line","from":{"data":"46bc8540-35b4-4195-87c2-42130fa5c014"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"46bc8540-35b4-4195-87c2-42130fa5c014","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"46bc8540-35b4-4195-87c2-42130fa5c014","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"46bc8540-35b4-4195-87c2-42130fa5c014\", :values ({:x 36.0, :y 0} {:x 47.06666666666667, :y 22.0} {:x 58.13333333333334, :y 143.0} {:x 69.2, :y 508.0} {:x 80.26666666666667, :y 1216.0} {:x 91.33333333333333, :y 1780.0} {:x 102.39999999999999, :y 1954.0} {:x 113.46666666666665, :y 1756.0} {:x 124.53333333333332, :y 1246.0} {:x 135.6, :y 724.0} {:x 146.66666666666666, :y 381.0} {:x 157.73333333333332, :y 160.0} {:x 168.79999999999998, :y 70.0} {:x 179.86666666666665, :y 30.0} {:x 190.9333333333333, :y 7.0} {:x 201.99999999999997, :y 2.0} {:x 213.06666666666663, :y 1.0} {:x 224.1333333333333, :y 0})}], :marks [{:type \"line\", :from {:data \"46bc8540-35b4-4195-87c2-42130fa5c014\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"46bc8540-35b4-4195-87c2-42130fa5c014\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"46bc8540-35b4-4195-87c2-42130fa5c014\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; @@

(defn beta-binomial-variance [n a b]
  (/ (* n a b (+ n a b))
     (* (sq (+ a b)) (+ a b 1))))

(defn beta-binomial-mean [n a b]
  (/ (* n a) (+ a b)))

(def n 1000)
(def alpha 5)
(def beta 3000)

;; Let's make variance about 8

(/ 8.0 (beta-binomial-variance n alpha beta))

;; (n*m*a*b(n + m*a + m*b)) / ((m*a + m*b)^2*(m*a + m*b + 1)) = (f*n*a*b(n + a + b)) / ((a + b)^2*(a + b + 1))

(defn sqrt [x]
  (Math/sqrt x))

(defn m [f n a b]
  (* (/ 1 (* 2 f (+ a b) (+ a b n)))
     (+ (sqrt (- (sq (+ (- (sq a))
                  (* -2 a b)
                  (* a f)
                  (- a)
                  (- (sq b))
                  (* b f)
                  (- b)
                  (* f n)))
              (* 4 (- (- (* a n)) (* b n) n)
                 (+ (* (sq a) f)
                    (* 2 a b f)
                    (* a f n)
                    (* b b f)
                    (* b f n)))))
        (sq a)
        (* 2 a b)
        (- (* a f))
        a
        (sq b)
        (- (* b f))
        b
        (- (* f n)))))

(m 3.610 n alpha beta)


(def xs (take 10000 (r/beta-binomial n {:alpha (* alpha ) :beta (/ beta 10.32)})))


(double (transduce identity kixi/mean xs))

(* 1 (double (beta-binomial-mean n alpha beta)))

(double (transduce identity kixi/variance xs))

(* 1 (double (beta-binomial-variance n alpha beta)))


;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-double'>2.213175482916134</span>","value":"2.213175482916134"}
;; <=

;; @@


(defn m [f n a b]
  (* (/ 1 (* 2 f (+ a b) (+ a b n)))
     (+ (sqrt (- (sq (+ (- (sq a))
                  (* -2 a b)
                  (* a f)
                  (- a)
                  (- (sq b))
                  (* b f)
                  (- b)
                  (* f n)))
              (* 4 (- (- (* a n)) (* b n) n)
                 (+ (* (sq a) f)
                    (* 2 a b f)
                    (* a f n)
                    (* b b f)
                    (* b f n)))))
        (sq a)
        (* 2 a b)
        (- (* a f))
        a
        (sq b)
        (- (* b f))
        b
        (- (* f n)))))



;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;witan-send/m</span>","value":"#'witan-send/m"}
;; <=

;; @@
(def n 3000)
(def alpha 10)
(def beta 3000)

(double (beta-binomial-variance n alpha beta))
(double (beta-binomial-mean n alpha beta))

(defn alpha-beta-variance [v n alpha beta]
  (let [r (/ beta alpha)
        alpha (/ (- (* n n r)
                    (* (sq (inc r)) v))
                 (* (inc r)
                    (- (* (sq (inc r)) v)
                       (* n r))))]
    [alpha (* r alpha)]))

(alpha-beta-variance 100 n alpha beta)

;; n*a / a+b
;; m = n*a/a+b
;; m/n = 1/1+b
;; m/n(1+b) = 1


(defn alpha-beta-mean-variance [n m v]
  (let [q (- (/ n m) 1)
        alpha (/ (- (* n n q)
                    (* (sq (inc q)) v))
                 (* (inc q)
                    (- (* (sq (inc q)) v)
                       (* n q))))]
    [alpha (* q alpha)]))

(alpha-beta-mean-variance 3000 1 100)


;; b = r


(def new-variance 200.0)

(let [n 300
      [alpha beta] (alpha-beta-mean-variance n 30 100)]
  [(double (beta-binomial-mean n alpha beta))
   (double (beta-binomial-variance n alpha beta))])

;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>30.0</span>","value":"30.0"},{"type":"html","content":"<span class='clj-double'>100.0</span>","value":"100.0"}],"value":"[30.0 100.0]"}
;; <=

;; @@
((n*a*b)*(n+a+b))/
;; @@
