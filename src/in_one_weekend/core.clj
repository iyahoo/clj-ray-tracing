(ns in-one-weekend.core
  (:require [clojure.java.io :as io :refer [writer]]
            [in-one-weekend.vec :refer :all]
            [in-one-weekend.ray :refer :all]
            [in-one-weekend.hitable-list :refer :all]
            [in-one-weekend.sphere :refer :all]
            [in-one-weekend.camera :refer :all]
            [in-one-weekend.material :refer :all]
            [overtone.live :refer :all]
            [overtone.inst.piano :refer [piano]])
  (:import [in_one_weekend.ray Ray]
           [in_one_weekend.hitable_list Hitable-list])
  (:gen-class))

(defn color [r world depth]
  {:pre [(= (class r) Ray) (= (class world) Hitable-list)]}
  (let [{:keys [result rec]} (hit world r 0.001 Float/MAX_VALUE)]
    (if result
      (let [{:keys [t p normal id]} rec
            {:keys [result scattered attenuation]} (scatter (:attr (nth (:lis world) id)) r rec)]
        (if (and (< depth 50) result)
          (times attenuation (color scattered world (+ depth 1)))
          (->Vec3 0 0 0)))
      (let [unit-direction (unit-vector (:direction r))
            t (* 0.5 (+ (y unit-direction) 1.0)) ]
        (plus (times (->Vec3 1.0 1.0 1.0) (- 1.0 t))
              (times (->Vec3 0.5 0.7 1.0) t))))))

(defn header [nx ny]
  (str "P3\n" nx " " ny "\n255\n"))

(defn int-color [f-colors]
  (map #(int (* 255.99 %)) f-colors))

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
  "左上から右下への座標のペアのリスト"
  [nx ny]
  (for [j (range (- ny 1) -1 -1) i (range 0 nx)] [j i]))

(defn coordinates-to-rate [[j i] ny nx]
  [(/ j (float ny)) (/ i (float nx))])

(defn make-color [ray world depth]
  (color ray world depth))

(defn make-str [[ir ig ib]]
  (str ir " " ig " " ib "\n"))

(defn gamma-correction [v]
  (apply-vec #(Math/sqrt %) v))

(defn anti-aliasing [[j i] ny nx ns camera world]
  (->> (repeatedly ns #(coordinates-to-rate [(+ j (rand)) (+ i (rand))] ny nx))
       (map (comp #(make-color % world 0) #(get-ray % camera)))
       (reduce plus)
       (times (/ 1 (float ns)))
       gamma-correction))

(defn make-camera []
  (->Camera (->Vec3 -2.0 -1.0 -1.0)
            (->Vec3 4.0 0.0 0.0)
            (->Vec3 0.0 2.0 0.0)
            (->Vec3 0.0 0.0 0.0)))

(defn make-world []
  (let [sphere1 (->Sphere (->Vec3 0 0 -1)      0.5 (->Lambertian (->Vec3 0.8 0.3 0.3)))
        sphere2 (->Sphere (->Vec3 0 -100.5 -1) 100 (->Lambertian (->Vec3 0.8 0.8 0.0)))
        sphere3 (->Sphere (->Vec3 1 0 -1)      0.5 (->Metal (->Vec3 0.8 0.6 0.2) 0.3))
        sphere4 (->Sphere (->Vec3 -1 0 -1)     0.5 (->Metal (->Vec3 0.8 0.8 0.8) 1.0))
        lis     (list sphere1 sphere2 sphere3 sphere4)]
    (->Hitable-list lis (count lis))))

(defn body [nx ny ns]
  (let [camera (make-camera)
        world  (make-world)
        allprocess #(-> %
                        (anti-aliasing ny nx ns camera world)
                        vals
                        int-color
                        make-str)]
    (->> (make-coordinates nx ny)
         (pmap allprocess)
         (apply str))))

(defn -main [nx ny ns]
  (dorun
   (with-open [fout (io/writer "out.pnm")]
     (-> fout
         (.write (str (header nx ny) (body nx ny ns)))))))

;; For notify using sound

(use 'overtone.live)
(use 'overtone.inst.piano)

(defn end-sound []
  (do (piano)
      true))

(defn main []
  (do (-main 400 200 100)
      (end-sound)))
