(ns fractals.l-system)

(defrecord L-system [axiom rewrite-rules drawing-rules phi])

(defn- l-pop [stack]
  [(last stack) (pop stack)])

(defn create-LS [axiom rewrite-rules drawing-rules phi]
  (->L-system axiom rewrite-rules drawing-rules phi))

(defn- rewrite [ls i]
  (get-in ls [:rewrite-rules i] [i]))

(defn- expand* [ls segments iter]
  (if (zero? iter)
    segments
    (let [segs (vec (mapcat (partial rewrite ls) segments))]
      (recur ls segs (- iter 1)))))

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

(defn parse [in]
  (let [stringify (fn [c] (cond
                           (= \[ c) "push"
                           (= \] c) "pop"
                           :true (str c)))]
    (vec (map (comp keyword stringify) (seq in)))))
