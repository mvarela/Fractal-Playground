(ns fractals.core
  (:require 
            [fractals.l-system :as l]
            [fractals.barnsley :as b]
            [fractals.mandelbrot :as m]))



(defn- main [&args]
  ;; Render the mandelbrot set, full, and zoomed in
(let [size [800 600]
      mb-set (m/mandelbrot size [[-2.5 -1][1 1]] 256)]
  (do (m/do-png mb-set size "full.png")
      (m/do-ppm mb-set size 256 "full.ppm")))

(let [size [800 600]
      mb-set (m/mandelbrot size [[0.1 0.9][0.2 1]] 256)]
  (do (m/do-png mb-set size "zoomed.png")
      (m/do-ppm mb-set size 256 "zoomed.ppm")))

  ;; Do a 40K points Barnsley fern
  (b/do-BF 40000)

  ;; Do a bunch of L-systems, at different depths
  (def L-dragon (l/create-LS [:F :X]
                             {:X [:X :+ :Y :F :+]
                              :Y [:- :F :X :- :Y]}
                             {:draw #{:F}
                              :skip #{:X :Y}
                              :move #{}}
                             (/ Math/PI 2)))

  (map (partial l/do-LS L-dragon "dragon" 0) (range 1 16))

  (def L-snowflake (l/create-LS [:F :- :- :F :- :- :F]
                                {:F [:F :+ :F :- :- :F :+ :F]}
                                {:draw #{:F}
                                 :skip #{}
                                 :move #{}}
                                (/ Math/PI 3)))

  (map (partial l/do-LS L-snowflake "koch-snowflake" 0) (range 1 10))

  (def L-sierpinski (l/create-LS [:F :- :G :- :G]
                                 {:F [:F :- :G :+ :F :+ :G :- :F]
                                  :G [:G :G]}
                                 {:draw #{:F :G}
                                  :skip #{}
                                  :move #{}}
                                 (* 2 (/ Math/PI 3))))

  (map (partial l/do-LS L-sierpinski "sierpinski-triangle" 0) (range 1 10))

  (def L-crystal (l/create-LS [:F :+ :F :+ :F :+ :F]
                              {:F [:F :F :+ :F :+ :+ :F :+ :F]}
                              {:draw #{:F}
                               :skip #{}
                               :move #{}}
                              (/ Math/PI 2)))

  (map (partial l/do-LS L-crystal "crystal" 0) (range 1 7))

  (def L-square (l/create-LS [:F :+ :F :+ :F :+ :F]
                             {:F [:F :+ :F :- :F :- :F :F :+ :F :+ :F :- :F]}
                             {:draw #{:F}
                              :skip #{}
                              :move #{}}
                             (/ Math/PI 2)))

  (map (partial l/do-LS L-square "koch-curve" 0) (range 1 5))

  (def L-box (l/create-LS [:F :+ :F :+ :F :+ :F]
                          {:F [:F :F :+ :F :+ :F :+ :F :+ :F :F]}
                          {:draw #{:F}
                           :skip #{}
                           :move #{}}
                          (/ Math/PI 2)))

  (map (partial l/do-LS L-box "box" 0) (range 1 6))

  (def L-rings (l/create-LS [:F :+ :F :+ :F :+ :F]
                            {:F [:F :F :+ :F :+ :F :+ :F :+ :F :+ :F :- :F]}
                            {:draw #{:F}
                             :skip #{}
                             :move #{}}
                            (/ Math/PI 2)))

  (map (partial l/do-LS L-rings "rings" 0) (range 1 7))

  (def L-hilbert (l/create-LS [:X]
                              {:X [:+ :Y :F :- :X :F :X :- :F :Y :+]
                               :Y [:- :X :F :+ :Y :F :Y :+ :F :X :-]}
                              {:draw #{:F}
                               :skip #{:X :Y}
                               :move #{}}
                              (/ Math/PI 2)))

  (map (partial l/do-LS L-hilbert "hilbert" 0) (range 1 10)))

