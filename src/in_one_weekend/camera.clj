(ns in-one-weekend.camera
  (:require [in-one-weekend.ray :refer [->Ray]]
            [in-one-weekend.vec :refer [plus times]]))

(defrecord Camera [lower-left-corner
                   horizontal
                   vertical
                   origin])

(defn get-ray [[v u] camera]
  {:pre [(= (class camera) Camera)]}
  (let [{:keys [lower-left-corner horizontal vertical origin]} camera]
    (->Ray origin (plus lower-left-corner (times u horizontal) (times v vertical)))))
