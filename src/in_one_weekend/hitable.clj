(ns in-one-weekend.hitable)

(defrecord HitRecord [t p normal id])

(defprotocol Hitable
  (hit [this ray t-min t-max id]))
