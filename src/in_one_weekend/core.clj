(ns in-one-weekend.core
  (:require [clojure.java.io :as io :refer [writer]]
            [in-one-weekend.vec :refer :all]
            [in-one-weekend.ray :refer :all]
            [in-one-weekend.hitable-list :refer :all]
            [in-one-weekend.sphere :refer :all])
  (:gen-class))

(defn color [r world]
  (let [return-hit (hit world r 0.0 Float/MAX_VALUE)
        rec (:rec return-hit)]
    (if (:result return-hit)
      (times 0.5 (plus 1 (->Vec3 (x (:normal rec)) (y (:normal rec)) (z (:normal rec)))))
      (let [unit-direction (unit-vector (:direction r))
            t (* 0.5 (+ (y unit-direction) 1.0)) ]
        (plus (times (->Vec3 1.0 1.0 1.0) (- 1.0 t))
              (times (->Vec3 0.5 0.7 1.0) t))))))

(defn header [nx ny]
  (str "P3\n" nx " " ny "\n255\n"))

(defn int-color [f-color]
  (int (* 255.99 f-color)))

(defn body [nx ny]
  (let [lower-left-corner (->Vec3 -2.0 -1.0 -1.0)
        horizontal (->Vec3 4.0 0.0 0.0)
        vertical   (->Vec3 0.0 2.0 0.0)
        origin     (->Vec3 0.0 0.0 0.0)
        sphere1    (->Sphere (->Vec3 0 0 -1) 0.5)
        sphere2    (->Sphere (->Vec3 0 -100.5 -1) 100)
        world      (->Hitable-list (list sphere1 sphere2) 2)]
    (apply str
           (for [j (range (- ny 1) -1 -1)
                 i (range 0 nx)]
             (let [u (/ i (float nx))
                   v (/ j (float ny))
                   r (->Ray3 origin
                             (plus (plus lower-left-corner (times u horizontal))
                                   (times v vertical)))
                   p (point-at-parameter r 2.0)
                   col (color r world)
                   vs (vals col)
                   [ir ig ib] (map int-color vs)]
               (str ir " " ig " " ib "\n"))))))

(defn -main [& args]
  (let [nx 200
        ny 100]
    (with-open [fout (io/writer "out.ppm")]
      (-> fout
          (.write (str (header nx ny) (body nx ny)))))))
