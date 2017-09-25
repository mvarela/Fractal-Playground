(ns fractals.util)

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
