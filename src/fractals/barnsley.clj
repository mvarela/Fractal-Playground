(ns fractals.barnsley
  (:require [fractals.util :as util]
            [hiccup.core :as hiccup]))

(def transforms {:f1 [0 0 0 0.16 0 0]
                 :f2 [0.85 0.04 -0.04 0.85 0 1.6]
                 :f3 [0.2 -0.26 0.23 0.22 0 1.6]
                 :f4 [-0.15 0.28 0.26 0.24 0 0.44]})

(defn- transform-probs [i]
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
          yy  (+ (* c x) (* d y) f)]
      [xx yy])))

(defn barnsley-fern [num-points]
  (let [probs (repeatedly num-points rand)
        coeflist (map (comp transforms transform-probs) probs)]
    (reductions apply-transform [0 0] coeflist)))

;; (defn barnsley-fern [num-points]
;;   (comp scale-bf barnsley-fern*) num-points)

(defn- render-svg-points [plot-points size]
  (let [xmlns "http://www.w3.org/2000/svg"
        style "stroke:#5f7f5fBB; fill:#5f7f5fBB;"
        scale-bf (fn [[x y]] [(* 100 x) (* 100 y)])
        points (util/fix-coords (map scale-bf plot-points) size)
        do-circle (fn [pt] (let [[x y] pt]
                             [:circle {:cx (str x)
                                       :cy (str y)
                                       :r "0.2"
                                       :style style}]))
        circles (vec (conj (map do-circle  points) :g))]
    (hiccup/html [:html
                  [:div {:padding 25}
                   [:svg {:width size
                          :height size
                          :xmlns xmlns}
                    circles]]])))

(defn do-BF [num-points]
  (spit "barnsley-fern.html" (render-svg-points (barnsley-fern num-points) 500)))

(ns Marcos)

(def sheet "1001010101011000101011001110001001100111011100011011101010100011011000111101001101001")

(let [letters (partition 7 (seq sheet))]
  (map (comp char read-string (partial apply str)) (repeat "2r") letters))
