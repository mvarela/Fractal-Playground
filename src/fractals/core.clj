(ns fractals.core
  (:require [hiccup.core :as hiccup]
            [fractals.l-system :as l]))


(def L-dragon (l/create-LS [:F :X]
                          {:X [:X :+ :Y :F :+]
                           :Y [:- :F :X :- :Y]}
                          {:draw #{:F}
                           :skip #{:X :Y}
                           :move #{}}
                          (/ Math/PI 2)))

(defn fix-coords [points, canvas-max]
  (let [[xmax, xmin] ((juxt (partial apply max) (partial apply min)) (map first points))
        [ymax, ymin] ((juxt (partial apply max) (partial apply min)) (map second points))
        translated-points (map #(vector (- (first %) xmin)
                                        (- (second %) ymin)) points)
        x-span (- xmax, xmin)
        y-span (- ymax ymin)
        span (max x-span y-span)
        scale? (> span canvas-max)]
    (if scale?
      (let [multiplier (/ canvas-max span)
            do-scale (fn [i] (* multiplier i))]
        (map #(map do-scale %) translated-points))
      translated-points)))

(defn render-svg [plot-points size]
  (let [side size
        xmlns "http://www.w3.org/2000/svg"
        style "stroke:#474674; fill:white;"
        points (apply str (map #(str (first %) "," (second %) " ")
                               (fix-coords plot-points size)))]
    (hiccup/html [:html
                  [:div {:padding 25}
                   [:svg {:width size
                          :height size
                          :xmlns xmlns}
                    [:polyline {:points points
                                :style style}]]]])))

(defn do-LS [ls name rotation i]
  (let [pts (l/draw ls i rotation 0 0 10)]
    (spit (str name "-" (if (< i 10) (str 0 i) i) ".html") (render-svg pts 1000))))

(def L-dragon (l/create-LS [:F :X]
                           {:X [:X :+ :Y :F :+]
                            :Y [:- :F :X :- :Y]}
                           {:draw #{:F}
                            :skip #{:X :Y}
                            :move #{}}
                           (/ Math/PI 2)))

(map (partial do-LS L-dragon "dragon" 0) (range 1 16))

(def L-snowflake (l/create-LS [:F :- :- :F :- :- :F]
                              {:F [:F :+ :F :- :- :F :+ :F]}
                              {:draw #{:F}
                               :skip #{}
                               :move #{}}
                              (/ Math/PI 3)))

(map (partial do-LS L-snowflake "koch-snowflake" 0) (range 1 10))

(def L-sierpinski (l/create-LS [:F :- :G :- :G]
                               {:F [:F :- :G :+ :F :+ :G :- :F]
                                :G [:G :G]}
                               {:draw #{:F :G}
                                :skip #{}
                                :move #{}}
                               (* 2 (/ Math/PI 3))))

(map (partial do-LS L-sierpinski "sierpinski-triangle" 0) (range 1 10))

(def L-crystal (l/create-LS [:F :+ :F :+ :F :+ :F]
                           {:F [:F :F :+ :F :+ :+ :F :+ :F]}
                           {:draw #{:F}
                            :skip #{}
                            :move #{}}
                           (/ Math/PI 2)))

(map (partial do-LS L-crystal "crystal" 0) (range 1 7))

(def L-square (l/create-LS [:F :+ :F :+ :F :+ :F]
                           {:F [:F :+ :F :- :F :- :F :F :+ :F :+ :F :- :F]}
                            {:draw #{:F}
                             :skip #{}
                             :move #{}}
                            (/ Math/PI 2)))

(map (partial do-LS L-square "koch-curve" 0) (range 1 5))

(def L-box (l/create-LS [:F :+ :F :+ :F :+ :F]
                        {:F [:F :F :+ :F :+ :F :+ :F :+ :F :F]}
                           {:draw #{:F}
                            :skip #{}
                            :move #{}}
                           (/ Math/PI 2)))

(map (partial do-LS L-box "box" 0) (range 1 6))


(def L-rings (l/create-LS [:F :+ :F :+ :F :+ :F]
                          {:F [:F :F :+ :F :+ :F :+ :F :+ :F :+ :F :- :F]}
                        {:draw #{:F}
                         :skip #{}
                         :move #{}}
                        (/ Math/PI 2)))

(map (partial do-LS L-rings "rings" 0) (range 1 7))

(def L-hilbert (l/create-LS [:X]
                            {:X [:+ :Y :F :- :X :F :X :- :F :Y :+]
                             :Y [:- :X :F :+ :Y :F :Y :+ :F :X :-]}
                            {:draw #{:F}
                             :skip #{:X :Y}
                             :move #{}}
                            (/ Math/PI 2)))

(map (partial do-LS L-hilbert "hilbert" 0) (range 1 10))


(def L-seaweed (l/create-LS [:F]
                            {:F [:F :F :- :push :- :F :+ :F :+ :F :pop :+ :push :+ :F :- :F :- :F :pop]}
                            {:draw #{:F}
                             :skip #{}
                             :move #{}}
                            0.3490659))

(map (partial do-LS L-seaweed "seaweed" 0) (range 1 5))
