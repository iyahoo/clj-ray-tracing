(ns in-one-weekend.ray
  (:require [in-one-weekend.vec :refer [plus times]]
            [taoensso.timbre.profiling :refer [p]]))

(defrecord Ray [origin direction])

(defn point-at-parameter [this t]
  {:pre [(= (class this) Ray)]}
  (p :point-at-parameter
     (plus (:origin this) (times (:direction this) t))))
