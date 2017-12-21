(ns in-one-weekend.hitable-list
  (:require [in-one-weekend.ray :refer :all]
            [in-one-weekend.hitable :refer :all]
            [in-one-weekend.sphere :refer :all]))

(declare hit-hitablelist)

(defrecord HitableList [lis list-size]
  Hitable
  (hit [hitlis ray t-min t-max id]
    (hit-hitablelist hitlis ray t-min t-max id)))

(defn hit-hitablelist [hitlis ray t-min t-max id]
  (loop [i 0
         temp-rec nil
         hit-anything false
         closest-so-far t-max]
    (if (< i (:list-size hitlis))
      (let [list-i (get-in hitlis [:lis i])]
        (if-let [rec (hit list-i ray t-min closest-so-far i)]
          (recur (+ i 1) rec      true         (:t rec))
          (recur (+ i 1) temp-rec hit-anything closest-so-far)))
      (when hit-anything
        temp-rec))))
