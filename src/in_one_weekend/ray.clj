(ns in-one-weekend.ray
  (:require [in-one-weekend.vec :as vec]))

(defprotocol Ray
  (point-at-parameter [this t]))

(defrecord Ray3 [origin direction]
  Ray
  (point-at-parameter [this t]
    (vec/plus (:origin this) (vec/times (:direction this) t))))
