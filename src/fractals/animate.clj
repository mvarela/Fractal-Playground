(ns fractals.animate
  (:require [fractals.mandelbrot :as m]
            [clojure.math.numeric-tower :as nt]))

(defn interpolate-centers [start-point end-point nframes]
  (let [[xs ys] start-point
        [xe ye] end-point
        delta-x (- xe xs)
        delta-y (- ye ys)
        step (/ (Math/sqrt (+ (nt/expt delta-x 2)
                              (nt/expt delta-y 2)))
                nframes)
        theta (Math/atan2  delta-y delta-x)
        next-center (fn [[x y]] [(+ x (* step (Math/cos theta)))
                                 (+ y (* step (Math/sin theta)))])]
    (take (inc nframes) (iterate next-center start-point))))

(defn interpolate-areas [start-area ratio nframes]
  (let [step (nt/expt ratio (/ 1.0 nframes))]
    (take (inc nframes)
          (iterate #(/ % step) start-area))))

(defn bounding-box [aspect-ratio area center]
  (let [[x y] center
        dim-y (nt/sqrt (/ area aspect-ratio))
        dim-x (* dim-y aspect-ratio)
        delta-y (/ dim-y 2)
        delta-x (/ dim-x 2)]
    [[(- x delta-x) (- y delta-y)]
     [(+ x delta-x) (+ y delta-y)]]))

(defn animate-mandelbrot [img-width
                          img-height
                          depth
                          start-point
                          end-point
                          start-area
                          zoom-factor
                          nframes]
  (let [centers (interpolate-centers start-point end-point nframes)
        areas (interpolate-areas start-area zoom-factor nframes)
        aspect-ratio (/ img-width img-height)
        size [img-width img-height]
        b-boxes (map (partial bounding-box aspect-ratio) areas centers)
        names (map (partial format "mb-frame-%04d.png") (range))
        fracs (map #(m/mandelbrot size % depth) b-boxes)]
    (dorun (pmap #(m/do-png %1 size depth %2) fracs names))))

;; Zoom into the seahorse valley
;;(time (animate-mandelbrot 300 200 255 [-1.75 0] [-0.8 0.11] 7 60 24))
;;(time (animate-mandelbrot 150 100 100 [-1.75 1] [-0.75 0.13] 7 1400 24))
;;ffmpeg -framerate 12 -start_number 0 -i mb-frame-%4d.png out.mp4
;; (time (animate-mandelbrot 192 108 256 [-1.75 1] [-0.78 0.17] 7 2400 72))

