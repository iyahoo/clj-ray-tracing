(ns in-one-weekend.hitable-list
  (:require [in-one-weekend.ray :refer :all]
            [in-one-weekend.hitable :refer :all]
            [in-one-weekend.sphere :refer [hit-sphere]]))

(defrecord Hitable-list [lis list-size])

(defn hit [hitlis ray t-min t-max]
  {:pre [(= (class hitlis) Hitable-list)]}
  (loop [i 0
         temp-rec nil
         hit-anything false
         closest-so-far t-max]
    (if (< i (:list-size hitlis))
      (let [list-i (nth (:lis hitlis) i)
            {:keys [result rec]} (hit-sphere list-i ray t-min closest-so-far i)]
        (if result
          (recur (+ i 1) rec      true         (:t rec))
          (recur (+ i 1) temp-rec hit-anything closest-so-far)))
      {:result hit-anything :rec temp-rec})))

;; (defn hit [hitlis ray t-min t-max]
;;   {:pre [(= (class hitlis) Hitable-list)]}
;;   (->> (range (:list-size hitlis))
;;        (map #(hit-sphere (nth (:lis hitlis) %) ray t-min t-max %))
;;        (filter :rec)
;;        (sort-by :t)
;;        (first)))
