(ns in-one-weekend.camera
  (:require [in-one-weekend.ray :refer [->Ray]]
            [in-one-weekend.vec :refer :all])
  (:import [in_one_weekend.vec Vec3]))

(defrecord Camera [lookfrom lookat vup vfov aspect])

(defn camera-view [{:keys [lookfrom lookat vup vfov aspect]}]
  {:pre [(= (class lookfrom) Vec3)
         (= (class lookat) Vec3)
         (= (class vup) Vec3)]}
  (let [theta (angle->rad vfov)
        half-height (Math/tan (/ theta 2.0))
        half-width (* aspect half-height)
        w (unit-vector (minus lookfrom lookat))
        u (unit-vector (cross vup w))
        v (cross w u)]
    {:lower-left-corner (minus lookfrom (times half-width u) (times half-height v) w)
     :horizontal (times (* 2.0 half-width) u)
     :vertical (times (* 2.0 half-height) v)
     :origin lookfrom}))

(defn get-ray [[v u] camera]
  {:pre [(= (class camera) Camera)]}
  (let [{:keys [lower-left-corner horizontal vertical origin]} (camera-view camera)]
    (->Ray origin (minus (plus lower-left-corner (times u horizontal) (times v vertical)) origin))))
