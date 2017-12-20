(ns in-one-weekend.hitable-list
  (:require [in-one-weekend.ray :refer :all]
            [in-one-weekend.hitable :refer :all]
            [in-one-weekend.sphere :refer :all]))

(defrecord HitableList [lis list-size]
  Hitable
  (hit [hitlis ray t-min t-max id]
    {:pre [(= (class hitlis) HitableList)]}
    (loop [i 0
           temp-rec nil
           hit-anything false
           closest-so-far t-max]
      (if (< i (:list-size hitlis))
        (let [list-i (get-in hitlis [:lis i])
              {:keys [result rec]} (hit list-i ray t-min closest-so-far i)]
          (if result
            (recur (+ i 1) rec      true         (:t rec))
            (recur (+ i 1) temp-rec hit-anything closest-so-far)))
        {:result hit-anything :rec temp-rec}))))

;; (defn hit [hitlis ray t-min t-max]
;;   {:pre [(= (class hitlis) Hitable-list)]}
;;   (->> (range (:list-size hitlis))
;;        (map #(hit-sphere (nth (:lis hitlis) %) ray t-min t-max %))
;;        (filter :rec)
;;        (sort-by :t)
;;        (first)))
