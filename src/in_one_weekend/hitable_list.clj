(ns in-one-weekend.hitable-list
  (:require [in-one-weekend.vec :refer :all]
            [in-one-weekend.ray :refer :all]
            [in-one-weekend.hitable :refer :all]
            [in-one-weekend.sphere :refer :all]))

(defn hit [this ray t-min t-max]
  (loop [i 0
         temp-rec nil
         hit-anything false
         closest-so-far t-max]
    (if (< i (:list-size this))
      (let [list-i (nth (:lis this) i)
            return-hit (hit-sphere list-i ray t-min closest-so-far)
            return-rec (:rec return-hit)]
        (if (return-hit :result)
          (recur (+ i 1) return-rec true         (:t return-rec))
          (recur (+ i 1) temp-rec   hit-anything closest-so-far)))
      {:result hit-anything :rec temp-rec})))

(defrecord Hitable-list [lis list-size])


