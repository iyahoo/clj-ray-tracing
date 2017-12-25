(ns in-one-weekend.material
  (:require [in-one-weekend.vec :refer :all]
            [in-one-weekend.ray :refer [->Ray]])
  (:import [in_one_weekend.vec Vec3]
           [in_one_weekend.ray Ray]))

(defprotocol Material
  (scatter? [this ray rec]))

(defrecord Lambertian [albedo]
  Material
  (scatter? [lamb ray rec]
    (let [{:keys [p normal]} rec
          target (plus p normal (random-in-unit-sphere))]
      {:attenuation (:albedo lamb) :scattered (->Ray p (minus target p))})))

(defn acute-angle? [v1 v2]
  (> (dot v1 v2) 0))

(defn reflect [v n]
  (minus v (times 2.0 (times (dot v n) n))))

(defrecord Metal [albedo fuzz]
  Material
  (scatter? [met ray rec]
    (let [{:keys [p normal]} rec
          reflected (reflect (unit-vector (:direction ray)) normal)
          scattered (->Ray p (plus reflected (times (random-in-unit-sphere) fuzz)))]
      (when (acute-angle? (:direction scattered) normal)
        {:attenuation (:albedo met) :scattered scattered}))))

(defn metal [albedo fuzz]
  (->Metal albedo (if (< fuzz 1) fuzz 1)))

(defn refract [v n ni-over-nt]
  {:pre [(= (class v) Vec3)]}
  (let [uv (unit-vector v)
        dt (dot uv n)
        d (- 1.0 (* ni-over-nt ni-over-nt (- 1 (* dt dt))))
        refracted (minus (times ni-over-nt (minus uv (times n dt)))
                         (times n (Math/sqrt d)))]
    (if (> d 0)
      {:result true  :refracted refracted}
      {:result false :refracted refracted})))

(defn pow2 [x]
  (* x x))

(defn cosine-val [acute-angle-p ref-idx normal direction]
  (if acute-angle-p
    (/ (* ref-idx (dot direction normal))
       (vector-length direction))
    (/ (- (dot direction normal))
       (vector-length direction))))

(defn schlick [cosine ref-idx]
  "https://en.wikipedia.org/wiki/Schlick%27s_approximation."
  {:pre [(float? cosine) (float? ref-idx)]}
  (let [r0 (-> (/ (- 1 ref-idx) (+ 1 ref-idx)) ; The ref-idx of air is 1.
               (pow2))]
    (+ r0 (* (- 1 r0)
             (Math/pow (- 1 cosine) 5)))))

(defn scatter-dielectric [ref-idx ray rec]
  (let [{:keys [normal p]} rec
        {:keys [direction]} ray
        acute-angle-p (acute-angle? normal direction)]
    (let [{:keys [result refracted]}
          (if acute-angle-p
            (refract direction (minus normal)        ref-idx)
            (refract direction         normal (/ 1.0 ref-idx)))
          reflected (reflect direction normal)]
      (if result
        (let [cosine (cosine-val acute-angle-p ref-idx normal direction)
              reflect-prob (schlick cosine ref-idx)]
          (if (< (rand) reflect-prob)
            {:attenuation (->Vec3 1.0 1.0 1.0) :scattered (->Ray p reflected)}
            {:attenuation (->Vec3 1.0 1.0 1.0) :scattered (->Ray p refracted)}))
        {:attenuation (->Vec3 1.0 1.0 1.0) :scattered (->Ray p reflected)}))))

;; `ref-idx` is the refractive index.
(defrecord Dielectric [ref-idx]
  Material
  (scatter? [diele ray rec]
    {:pre [(= (class ray) Ray)]}
    (scatter-dielectric ref-idx ray rec)))
