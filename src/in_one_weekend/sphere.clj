(ns in-one-weekend.sphere
  (:require [in-one-weekend.hitable :refer [hit-record]]
            [in-one-weekend.vec :refer [minus dot divs]]
            [in-one-weekend.ray :refer [point-at-parameter]]))

(defrecord Sphere [center radius])

(defn equation-of-ray
  "B.Bt^2+2B.(A-C)t+(A-C)^2-R^2 = 0."
  [A B C R]
  (let [A-C (minus A C)
        a (dot B B)
        b (dot A-C B)
        c (- (dot A-C A-C) (* R R))
        d (- (* b b) (* a c))]
    {:discriminant d
     :x1 (if (< d 0) 0 (/ (- (- b) (Math/sqrt d)) a))
     :x2 (if (< d 0) 0 (/ (+ (- b) (Math/sqrt d)) a))}))

(defn hit-result [sphere r t]
  {:pre [(= (class sphere) Sphere)]}
  (let [p (point-at-parameter r t)
        normal (divs (minus p (:center sphere)) (:radius sphere))]
    (struct hit-record t p normal)))

(defn in? [x x-min x-max]
  (and (< x-min x) (< x x-max)))

(defn hit-sphere [sphere r t-min t-max]
  (let [{d :discriminant t1 :x1 t2 :x2}
        (equation-of-ray (:origin r) (:direction r) (:center sphere) (:radius sphere))]
    (cond
      (and (> d 0) (in? t1 t-min t-max)) {:result true :rec (hit-result sphere r t1)}
      (and (> d 0) (in? t2 t-min t-max)) {:result true :rec (hit-result sphere r t2)}
      :else {:result false})))
