(ns in-one-weekend.material
  (:require [in-one-weekend.vec :refer :all]
            [in-one-weekend.ray :refer [->Ray]])
  (:import [in_one_weekend.vec Vec3]
           [in_one_weekend.ray Ray]))

(defprotocol Material
  (scatter [this ray rec]))

(defrecord Lambertian [albedo]
  Material
  (scatter [lamb ray rec]
    (let [{:keys [p normal]} rec
          target (plus p normal (random-in-unit-sphere))]
      {:attenuation (:albedo lamb) :scattered (->Ray p (minus target p))})))

(defn reflect [v n]
  (minus v (times 2.0 (times (dot v n) n))))

(defrecord Metal [albedo fuzz]
  Material
  (scatter [met ray rec]
    (let [{:keys [p normal]} rec
          reflected (reflect (unit-vector (:direction ray)) normal)
          scattered (->Ray p (plus reflected (times (random-in-unit-sphere) fuzz)))]
      (when (> (dot (:direction scattered) normal) 0)
        {:attenuation (:albedo met) :scattered scattered}))))

(defn metal [albedo fuzz]
  (->Metal albedo (if (< fuzz 1) fuzz 1)))

(defn refract [v n ni-over-nt]
  {:pre [(= (class v) Vec3)]}
  (let [uv (unit-vector v)
        dt (dot uv n)
        d (- 1.0 (* ni-over-nt ni-over-nt (- 1 (* dt dt))))]
    (if (> d 0)
      {:refracted (minus (times ni-over-nt (minus uv (times n dt)))
                         (times n (Math/sqrt d)))})))

;; (defn reflect [v n ni-over-nt]
;;   (let [discriminant  (fn [dt]
;;                         (- 1.0 (* ni-over-nt ni-over-nt (- 1 (* dt dt)))))
;;         reflected-vec (fn [d]
;;                         (minus (times ni-over-nt (minus u))))])
;;   (some-> (unit-vector v)
;;           (dot v)
;;           discriminant
;;           (as-> d
;;               (if (> d 0)
;;                 d))
;;           ()))

(defrecord Dielectric [ref-idx]
  Material
  (scatter [diele ray rec]
    {:pre [(= (class ray) Ray)]}
    (let [{:keys [normal p]} rec
          inpro (dot (:direction ray) normal)]
      (when-let [{:keys [refracted]}
                 (refract (:direction ray)
                          (if (> inpro 0) (minus normal) normal)
                          (if (> inpro 0) ref-idx (/ 1.0 ref-idx)))]
        {:attenuation (->Vec3 1.0 1.0 1.0)
         :scattered (->Ray p refracted)}))))
