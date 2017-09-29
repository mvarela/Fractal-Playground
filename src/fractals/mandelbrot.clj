(ns fractals.mandelbrot
  (:require [com.evocomputing.colors :as c]
            [clojure.string :refer [join]])
  (:import [java.io File]
           [javax.imageio ImageIO]
           [java.awt Color]
           [java.awt.image BufferedImage]))

(defn- scale-to-bounds
"This function takes a point [x y] in an plot area [[0 0] [xmax ymax]],
  and maps it to a bounding box in the real plane where the plotting will be
  done. The bounding box is defined by its lower left and upper right corners,
  and in practice, should not be much larger than [[-2.5 -1] [1 1]], which is
  where the fun stuff happens"
  [point img-size bounding-box]
  (let [[x y] point
        [xmax ymax] img-size
        [[b-x-min b-y-min][b-x-max b-y-max]] bounding-box
        bx-span (- b-x-max b-x-min)
        by-span (- b-y-max b-y-min)
        xx (- (* bx-span (/ x xmax)) (Math/abs b-x-min))
        yy (- (* by-span (/ y ymax)) (Math/abs b-y-min))]
    [xx yy]))


(defn- square-z [r i]
  [(- (* r r) (* i i)) (* 2 r i)])

(defn- in-mandelbrot? [point iter]
  "This function will return 0 if the point is in the set, and otherwise, the
  number of remaining iterations remaining (out of the original maximum), if the
  point 'escapes' from the set"
  (let [[x y] point
        values (iterate #(map + point (square-z (first %) (second %))) [0 0])
        test  (fn[acc [r i]]
                (if (or (>= acc iter) (> (+ (* r r)(* i i)) 4))
                  (reduced acc)
                  (inc acc)))
        counted (reduce test 0 values)]
    (- iter counted)))

(defn mandelbrot [size bounding-box depth]
  (let [[xmax ymax] size
        points (for [y (range ymax)
                     x (range xmax)]
                 [x y])
        f (fn[i](-> i
                   (scale-to-bounds size bounding-box)
                   (in-mandelbrot? depth)))]
    (pmap f points)))

;; (defn do-color [i]
;;   (let [[r g b] (c/hsl-to-rgb 205 100 (/ i 2.56))]
;;     (bit-or (bit-shift-left r 16)
;;             (bit-shift-left g 8)
;;             b)))

(defn do-color
  "Maps a new color with the luminance based on the number of iterations it took
  for the point to escape the set. The plain mapping is rather dull, so here we
  focus on the points which are in the boundaries (intuitively, those that took
  long to escape, but not longest). We calculate the luminance as the distance
  to the middle value, which results in a nice 'electric' look around the edges
  of the set"
  [i depth]
  (let [ icorr (/ (* i 100.0) depth)
        l (- 100 (* 2 (Math/abs (- 50 icorr))))
        s (- 100 (* 1.3 (Math/abs (- 50 icorr))))
        [r g b] (c/hsl-to-rgb 205 s l)]
    (bit-or (bit-shift-left r 16)
            (bit-shift-left g 8)
            b)))

(defn do-ppm [points size depth name]
  (let [[x y] size
        contents (->> points
                      (partition x)
                      (reverse)
                      (map #(join " " (map str %)))
                      (interpose "\n")
                      (apply str)
                      )
        header (str "P2\n" (str x) " "
                    (str y) "\n"
                    (str depth) "\n")]
    (spit name (str header contents))))


(defn do-png [points size depth name]
  (ImageIO/write
   (let [width (first size)
         height (second size)
         pts (->> points
                  (partition width)
;                  (reverse)
                  (map vec)
                  (vec))
         out (new BufferedImage width height  BufferedImage/TYPE_3BYTE_BGR)]
     (doseq [x (range width)
             y (range height)]
       (.setRGB out x y (do-color (get-in pts [y x]) depth)))
     out)
   "png"
   (new File name )))

;; (defn colorppm [size]
;;   (let [[x y] size
;;         contents (->> mymandel
;;                       (map do-color)
;;                       (flatten)
;;                       (partition 21)
;;                       (map #(apply str (interpose " " (map str %))))
;;                       (interpose "\n")
;;                       (apply str))]
;;     (spit "color.ppm" (str header contents))))



