(ns in-one-weekend.camera
  (:require [in-one-weekend.ray :refer [->Ray]]
            [in-one-weekend.vec :refer :all]
            [taoensso.timbre.profiling :refer [p]])
  (:import [in_one_weekend.vec Vec3]))

(defrecord Camera [lookfrom lookat vup vfov aspect])

(defn orthonormal-basis [a b c]
  (p :orthonormal-basis
     (let [w (unit-vector (minus a b))
           u (unit-vector (cross c w))
           v (cross w u)]
       [w u v])))

(defn camera-ob [lookfrom lookat vup]
  (orthonormal-basis lookfrom lookat vup))

(defn camera-view [{:keys [lookfrom lookat vup vfov aspect]}]
  {:pre [(= (class lookfrom) Vec3)
         (= (class lookat) Vec3)
         (= (class vup) Vec3)]}
  (let [theta (angle->rad vfov)
        half-height (Math/tan (/ theta 2.0))
        half-width (* aspect half-height)
        [w u v] (camera-ob lookfrom lookat vup)]
    {:lower-left-corner (minus lookfrom (times half-width u) (times half-height v) w)
     :horizontal (times (* 2.0 half-width) u)
     :vertical (times (* 2.0 half-height) v)
     :origin lookfrom}))

(defn correspond-point [[v u] camera-range]
  (let [{:keys [lower-left-corner horizontal vertical origin]} camera-range]
    (plus lower-left-corner (times u horizontal) (times v vertical))))

(defn get-ray [coordinate {:keys [lookfrom] :as camera}]
  {:pre [(= (class camera) Camera)]}
  (p :get-ray
     (->> (camera-view camera)
          (correspond-point coordinate)
          (plus (minus lookfrom))
          (->Ray lookfrom))))
