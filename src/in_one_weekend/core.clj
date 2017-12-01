(ns in-one-weekend.core
  (:require [clojure.java.io :as io :refer [writer]]
            [in-one-weekend.vec :refer :all]
            [in-one-weekend.ray :refer :all])
  (:gen-class))

(defn color [r]
  (let [unit-direction (unit-vector (:direction r))
        t (* 0.5 (+ (y unit-direction) 1.0)) ]
    (plus (times (->Vec3 1.0 1.0 1.0) (- 1.0 t))
          (times (->Vec3 0.5 0.7 1.0) t))))

(defn header [nx ny]
  (str "P3\n" nx " " ny "\n255\n"))

(defn int-color [f-color]
  (int (* 255.99 f-color)))

(defn body [nx ny]  
  (let [lower-left-corner (->Vec3 -2.0 -1.0 -1.0)
        horizontal (->Vec3 4.0 0.0 0.0)
        vertical   (->Vec3 0.0 2.0 0.0)
        origin     (->Vec3 0.0 0.0 0.0)]
    (apply str
           (for [j (range (- ny 1) -1 -1)
                 i (range 0 nx)]
             (let [u (/ i (float nx))
                   v (/ j (float ny))
                   r (->Ray3 origin 
                             (plus (plus lower-left-corner (times u horizontal))
                                   (times v vertical)))
                   col (color r)
                   vs (vals col)
                   [ir ig ib] (map int-color vs)]
               (str ir " " ig " " ib "\n"))))))

(defn -main [& args]
  (let [nx 200
        ny 100]
    (with-open [fout (io/writer "out.ppm")]
      (-> fout
          (.write (str (header nx ny) (body nx ny)))))))
