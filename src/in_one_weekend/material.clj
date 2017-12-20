(ns in-one-weekend.material
  (:require [in-one-weekend.vec :refer :all]
            [in-one-weekend.ray :refer [->Ray]]))

(defprotocol Material
  (scatter [this ray rec]))

(defrecord Lambertian [albedo]
  Material
  (scatter [lamb ray rec]
    (let [{:keys [p normal]} rec
          target (plus p normal (random-in-unit-sphere))]
      {:result true :attenuation (:albedo lamb) :scattered (->Ray p (minus target p))})))

(defn reflect [v n]
  (minus v (times 2.0 (times (dot v n) n))))

(defrecord Metal [albedo fuzz]
  Material
  (scatter [met ray rec]
    (let [{:keys [p normal]} rec
          reflected (reflect (unit-vector (:direction ray)) normal)
          scattered (->Ray p (plus reflected (times (random-in-unit-sphere) fuzz)))]
      {:result (> (dot (:direction scattered) normal) 0)
       :attenuation (:albedo met) :scattered scattered})))

(defn metal [albedo fuzz]
  (->Metal albedo (if (< fuzz 1) fuzz 1)))
