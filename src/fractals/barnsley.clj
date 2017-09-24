(ns fractals.barnsley)

(def transforms {:f1 [0 0 0 0.16 0 0]
                 :f2 [0.85 0.04 -0.04 0.85 0 1.6]
                 :f3 [0.2 -0.26 0.23 0.22 0 1.6]
                 :f4 [-0.15 0.28 0.26 0.24 0 0.44]})


(defn- transform-probs[i]
  (cond
    (<= i 0.01) :f1
    (<= i (+ 0.01 0.85)) :f2
    (<= i (+ 0.01 0.85 0.07)) :f3
    :true :f4))

(defn- apply-transform [point coeffs]
  (if (empty? coeffs)
    [0 0]
    (let [[x y] point
          [a b c d e f] coeffs
          xx  (+ (* a x) (* b y) e)
          yy  (+ (* c x) (* d y) f )]
      [xx yy])))

(defn- barnsley-fern* [num-points]
  (let [probs (take num-points (repeatedly rand))
        coeflist (map (comp transforms transform-probs) probs)]
    (reductions apply-transform [0 0] coeflist)))

(defn- scale-bf [points]
  (map (fn [pt]
         (let [[x y] pt]
           [(* 100 x) (* 100 y)])) points))

(defn barnsley-fern [num-points]
  (scale-bf (barnsley-fern* num-points)))
