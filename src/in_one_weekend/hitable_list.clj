(ns in-one-weekend.hitable-list
  (:require [in-one-weekend.ray :refer :all]
            [in-one-weekend.hitable :refer :all]
            [in-one-weekend.sphere :refer :all]
            [taoensso.timbre.profiling :refer [p]]))

(defn hit-hitablelist [{:keys [lis list-size]} ray t-min t-max id]
  (p :hit-hitablelist
     (loop [i 0
            temp-rec nil
            hit-anything false
            closest-so-far t-max]
       (if (< i list-size)
         (if-let [rec (hit? (nth lis i) ray t-min closest-so-far i)]
           (recur (+ i 1) rec      true         (:t rec))
           (recur (+ i 1) temp-rec hit-anything closest-so-far))
         (when hit-anything
           temp-rec)))))

(defrecord HitableList [lis list-size]
  Hitable
  (hit? [hitlis ray t-min t-max id]
    (hit-hitablelist hitlis ray t-min t-max id)))
