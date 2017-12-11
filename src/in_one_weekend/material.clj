(ns in-one-weekend.material
  (:require [in-one-weekend.vec :refer :all]
            [in-one-weekend.ray :refer [->Ray]]))

(defprotocol Material
  (scatter [this ray rec]))

(defrecord Lambertian [albedo]
  Material
  (scatter [lamb ray rec]
    {:pre [(= (class lamb) Lambertian)]}
    (let [{:keys [p normal]} rec
          target (plus p normal (random-in-unit-sphere))]
      {:result true :attenuation (:albedo lamb) :scattered (->Ray p (minus target p))})))

(defn reflect [v n]
  (minus v (times n 2.0 (dot v n))))

(defrecord Metal [albedo]
  Material
  (scatter [met ray rec]
    {:pre [(= (class met) Metal)]}
    (let [{:keys [p normal]} rec
          scattered (->Ray p (reflect (unit-vector (:direction ray)) normal))]
      {:result (> (dot (:direction scattered) normal) 0) :attenuation (:albedo met) :scattered scattered})))
