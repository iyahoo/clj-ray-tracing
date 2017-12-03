(ns in-one-weekend.sphere
  (:require [in-one-weekend.hitable :refer :all]
            [in-one-weekend.vec :refer :all]
            [in-one-weekend.ray :refer :all]))

(defrecord Sphere [center radius])

(defmethod print-method Sphere [s ^java.io.Writer w]
  (.write w (str (:center s) " " (:radius s))))

(defn hit-sphere [this r t-min t-max]
  (let [oc (minus (:origin r) (:center this))
        a  (dot (:direction r) (:direction r))
        b  (dot oc (:direction r))
        c  (- (dot oc oc) (* (:radius this) (:radius this)))
        d  (- (* b b) (* a c))]
    (if (> d 0)
      (let [temp (/ (- (- b) (Math/sqrt d)) a)]
        (if (and (> t-max temp) (> temp t-min))
          (let [t temp
                p (point-at-parameter r t)
                normal (divs (minus p (:center this)) (:radius this))]
            {:result true :rec (struct hit-record t p normal)})
          (let [temp (/ (+ (- b) (Math/sqrt d)) a)]
            (if (and (> t-max temp) (> temp t-min))
              (let [t temp
                    p (point-at-parameter r t)
                    normal (divs (minus p (:center this)) (:radius this))]
                {:result true :rec (struct hit-record t p normal)})
              {:result false}))))
      {:result false})))
