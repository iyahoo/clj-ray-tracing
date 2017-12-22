(ns in-one-weekend.sphere
  (:require [in-one-weekend.hitable :refer [->HitRecord]]
            [in-one-weekend.hitable :refer [Hitable]]
            [in-one-weekend.vec :refer [minus dot divs]]
            [in-one-weekend.ray :refer [point-at-parameter]]))

(declare hit-result)

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

(defn between? [x x-min x-max]
  (and (< x-min x) (< x x-max)))

(defrecord Sphere [center radius attr]
  Hitable
  (hit? [sphere r t-min t-max id]
    (let [{d :discriminant t1 :x1 t2 :x2}
          (equation-of-ray (:origin r) (:direction r) (:center sphere) (:radius sphere))
          hit? (> d 0)]
      (cond
        (and hit? (between? t1 t-min t-max)) (hit-result sphere r t1 id)
        (and hit? (between? t2 t-min t-max)) (hit-result sphere r t2 id)
        :eles false))))

(defn hit-result [sphere r t id]
  (let [p (point-at-parameter r t) ;; The point where ray hits
        normal (divs (minus p (:center sphere)) (:radius sphere))]
    (->HitRecord t p normal id)))

