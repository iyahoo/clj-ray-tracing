(ns in-one-weekend.core
  (:require [clojure.java.io :as io :refer [writer]]
            [in-one-weekend.vec :refer :all]
            [in-one-weekend.ray :refer :all]
            [in-one-weekend.hitable-list :refer :all]
            [in-one-weekend.sphere :refer :all]
            [in-one-weekend.camera :refer :all])
  (:gen-class))

(defn random-in-unit-sphere []
  (let [p (squared-length (times 2.0 (minus (->Vec3 (rand) (rand) (rand)) (->Vec3 1 1 1))))]
    (if (>= p 1.0)
      p
      (recur))))

(defn color [r world]
  (let [{:keys [result rec]} (hit world r 0.001 Float/MAX_VALUE)]
    (if result
      (let [{:keys [t p normal]} rec
            target (plus p normal (random-in-unit-sphere))]
        (times 0.5 (color (->Ray p (minus target p)) world)))
      (let [unit-direction (unit-vector (:direction r))
            t (* 0.5 (+ (y unit-direction) 1.0)) ]
        (plus (times (->Vec3 1.0 1.0 1.0) (- 1.0 t))
              (times (->Vec3 0.5 0.7 1.0) t))))))

(defn header [nx ny]
  (str "P3\n" nx " " ny "\n255\n"))

(defn int-color [f-color]
  (int (* 255.99 f-color)))

(defn int-color* [f-colors]
  (map int-color f-colors))

;; (defn body [nx ny]
;;   (let [lower-left-corner (->Vec3 -2.0 -1.0 -1.0)
;;         horizontal (->Vec3 4.0 0.0 0.0)
;;         vertical   (->Vec3 0.0 2.0 0.0)
;;         origin     (->Vec3 0.0 0.0 0.0)
;;         sphere1    (->Sphere (->Vec3 0 0 -1) 0.5)
;;         sphere2    (->Sphere (->Vec3 0 -100.5 -1) 100)
;;         world      (->Hitable-list (list sphere1 sphere2) 2)]
;;     (apply str
;;            (for [j (range (- ny 1) -1 -1)
;;                  i (range 0 nx)]
;;              (let [u (/ i (float nx))
;;                    v (/ j (float ny))
;;                    r (->Ray origin
;;                              (plus lower-left-corner (times u horizontal) (times v vertical)))
;;                    p (point-at-parameter r 2.0)
;;                    col (color r world)
;;                    vs (vals col)
;;                    [ir ig ib] (map int-color vs)]
;;                (str ir " " ig " " ib "\n"))))))

(defn make-coordinates
  "左上から右下への座標のリスト"
  [nx ny]
  (for [j (range (- ny 1) -1 -1) i (range 0 nx)] [j i]))

(defn coordinates-to-rate [[j i] ny nx]
  [(/ j (float ny)) (/ i (float nx))])

(defn make-color [ray world]
  (color ray world))

(defn make-str [[ir ig ib]]
  (str ir " " ig " " ib "\n"))

(defn anti-aliasing [[j i] ny nx ns camera world]
  (->> (repeatedly ns #(coordinates-to-rate [(+ j (rand)) (+ i (rand))] ny nx))
       (map (comp #(make-color % world) #(get-ray % camera)))
       (reduce plus)
       (times (/ 1 (float ns)))
       ;; (apply-vec #(Math/sqrt %)) ;; Gamma correction
       ))

(defn body [nx ny ns]
  (let [camera     (->Camera (->Vec3 -2.0 -1.0 -1.0)
                             (->Vec3 4.0 0.0 0.0)
                             (->Vec3 0.0 2.0 0.0)
                             (->Vec3 0.0 0.0 0.0))
        sphere1    (->Sphere (->Vec3 0 0 -1) 0.5)
        sphere2    (->Sphere (->Vec3 0 -100.5 -1) 100)
        world      (->Hitable-list (list sphere1 sphere2) 2)
        allprocess #(-> %
                        (anti-aliasing ny nx ns camera world)
                        vals
                        int-color*
                        make-str)]
    (->> (make-coordinates nx ny)
         (map allprocess)
         (apply str))))

(defn -main [nx ny ns]  
  (dorun
   (with-open [fout (io/writer "out.pnm")]
     (-> fout
         (.write (str (header nx ny) (body nx ny ns)))))))
