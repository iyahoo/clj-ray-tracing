(ns in-one-weekend.core
  (:require [clojure.java.io :as io :refer [writer]]
            [in-one-weekend.hitable :refer :all]
            [in-one-weekend.hitable-list :refer :all]
            [in-one-weekend.sphere :refer :all]
            [in-one-weekend.vec :refer :all]
            [in-one-weekend.ray :refer :all]
            [in-one-weekend.camera :refer :all]
            [in-one-weekend.material :refer :all])
  (:import [in_one_weekend.ray Ray]
           [in_one_weekend.hitable_list HitableList]
           [in_one_weekend.sphere Sphere])
  (:gen-class))

(defn color [r world depth]
  ;; {:pre [(= (class r) Ray) (= (class world) HitableList)]}
  (if-let [{:keys [id] :as rec} (hit world r 0.001 Float/MAX_VALUE 0)]
    (if-let [{:keys [scattered attenuation]} (scatter (get-in world [:lis id :attr]) r rec)]
      (if (< depth 50)
        (times attenuation (color scattered world (+ depth 1)))
        (->Vec3 0 0 0))
      (->Vec3 0 0 0))
    (let [unit-direction (unit-vector (:direction r))
          t (* 0.5 (+ (y unit-direction) 1.0))]
      (plus (times (->Vec3 1.0 1.0 1.0) (- 1.0 t))
            (times (->Vec3 0.5 0.7 1.0) t)))))

(defn header [nx ny]
  (str "P3\n" nx " " ny "\n255\n"))

(defn int-color [f-colors]
  (map #(int (* 255.99 %)) f-colors))

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
        sphere3 (->Sphere (->Vec3 1 0 -1)      0.5 (->Metal (->Vec3 0.8 0.6 0.2)))
        sphere4 (->Sphere (->Vec3 -1 0 -1)     0.5 (->Metal (->Vec3 0.8 0.8 0.8)))
        lis     (vector sphere1 sphere2 sphere3 sphere4)]
    (->HitableList lis (count lis))))

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
