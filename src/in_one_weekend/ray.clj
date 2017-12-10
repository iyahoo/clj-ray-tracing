(ns in-one-weekend.ray
  (:require [in-one-weekend.vec :refer [plus times]]))

(defrecord Ray [origin direction])

(defn point-at-parameter [this t]
  {:pre [(= (class this) Ray)]}
  (plus (:origin this) (times (:direction this) t)))
