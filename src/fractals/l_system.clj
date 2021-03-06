(ns fractals.l-system
  (:require [fractals.util :as util]
            [hiccup.core :as hiccup]
            [clojure.string :refer [join]]))

(defrecord L-system [axiom rewrite-rules drawing-rules phi])

(defn- l-pop [stack]
  [(peek stack) (pop stack)])

(defn create-LS [axiom rewrite-rules drawing-rules phi]
  (->L-system axiom rewrite-rules drawing-rules phi))

(defn- rewrite [ls i]
  (get-in ls [:rewrite-rules i] [i]))

(defn- expand* [ls segments iter]
  (if (zero? iter)
    segments
    (let [segs (vec (mapcat (partial rewrite ls) segments))]
      (recur ls segs (dec iter)))))

(defn expand [ls iter]
  (expand* ls (:axiom ls) iter))

(defn- draw* [ls segments theta acc x y step stack]
  (if (empty? segments)
    acc
    (let [s (first segments)
          xs (rest segments)
          phi (get-in ls [:phi])
          skip-set (get-in ls [:drawing-rules :skip])
          draw-set (get-in ls [:drawing-rules :draw])
          move-set (get-in ls [:drawing-rules :move])]
      (cond
        (contains? skip-set s) (recur ls xs theta acc x y step stack)
        (contains? draw-set s) (let [xx (+ x (* step (Math/cos theta)))
                                     yy (+ y (* step (Math/sin theta)))
                                     new-acc (conj acc (vector xx yy))]
                                 (recur ls xs theta new-acc xx yy step stack))
        (contains? move-set s) (let [xx (+ x (* step (Math/cos theta)))
                                     yy (+ y (* step (Math/sin theta)))]
                                 (recur ls xs theta acc xx yy step stack))
        (= :+ s) (recur ls xs (+ theta phi) acc x y step stack)
        (= :- s) (recur ls xs (- theta phi) acc x y step stack)
        (= :push s) (recur ls xs theta acc x y step (conj stack [x y]))
        (= :pop s) (let [[[xx yy] new-stack] (l-pop stack)]
                     (recur ls xs theta acc xx yy step new-stack))))))

(defn draw [ls iter theta x y step]
  (draw* ls (expand ls iter) theta [] x y step []))

(defn- render-svg-path [plot-points size]
  (let [xmlns "http://www.w3.org/2000/svg"
        style "stroke:#474674; fill:white;"
        points (join (map #(str (first %) "," (second %) " ")
                               (util/fix-coords plot-points size)))]
    (hiccup/html [:html
                  [:div {:padding 25}
                   [:svg {:width size
                          :height size
                          :xmlns xmlns}
                    [:polyline {:points points
                                :style style}]]]])))

(defn do-LS [ls name rotation i]
  (let [pts (draw ls i rotation 0 0 10)]
    (spit (str name "-" (if (< i 10) (str 0 i) i) ".html") (render-svg-path pts 1000))))

(defn parse [in]
  (let [stringify (fn [c] (cond
                           (= \[ c) "push"
                           (= \] c) "pop"
                           :true (str c)))]
    (vec (map (comp keyword stringify) (seq in)))))
