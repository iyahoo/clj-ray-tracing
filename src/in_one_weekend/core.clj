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

(defn air-color [r color-val]
  (let [unit-direction (unit-vector (:direction r))
        t (* 0.5 (+ (y unit-direction) 1.0))]
    (plus (times (->Vec3 1.0 1.0 1.0) (- 1.0 t))
          (times (->Vec3 0.5 0.7 1.0) t))))

(defn color
  ([r world depth]
   {:pre [(= (class r) Ray) (= (class world) HitableList)]}
   (color r world depth (->Vec3 1.0 1.0 1.0)))
  ([r world depth color-val]
   (if-let [{:keys [id] :as rec}
            (hit? world r 0.001 Float/MAX_VALUE 0)]
     (if-let [{:keys [scattered attenuation]}
              (scatter? (get-in world [:lis id :attr]) r rec)]
       (if (< depth 50)
         (recur scattered world (+ depth 1) (times color-val attenuation))
         (->Vec3 0 0 0))
       (->Vec3 0 0 0))
     ;; When ray don't hit any sphere
     (times color-val (air-color r color-val)))))

(defn header [nx ny]
  (str "P3\n" nx " " ny "\n255\n"))

(defn int-color [f-colors]
  (map #(int (* 255.99 %)) f-colors))

(defn make-coordinates
  "The sequence of coordinates from left top to right down."
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

;; from slanting
;; (defn make-camera [nx ny]
;;   (->Camera (->Vec3 3 2 4) (->Vec3 0 0 -1) (->Vec3 0 1 0) 90.0 (/ (float nx) (float ny))))

(defn make-camera [nx ny]
  (->Camera (->Vec3 7 1.5 2.5) (->Vec3 0 0 -1) (->Vec3 0 1 0) 45.0 (/ (float nx) (float ny))))

(defn make-world []
  (let [sphere1 (->Sphere (->Vec3 0  0 -1)     0.5  (->Lambertian (->Vec3 0.1 0.2 0.5)))
        sphere2 (->Sphere (->Vec3 0 -100.5 -1) 100  (->Lambertian (->Vec3 0.8 0.8 0.0)))
        sphere3 (->Sphere (->Vec3 1  0 -1)     0.5  (->Metal (->Vec3 0.8 0.6 0.2) 0.0))
        sphere4 (->Sphere (->Vec3 -1 0 -1)     0.5  (->Dielectric 1.5))
        sphere5 (->Sphere (->Vec3 -1 0 -1)    -0.45 (->Dielectric 1.5))
        ;; r (Math/cos (/ Math/PI 4.0))
        ;; sphere1 (->Sphere (->Vec3 (- r) 0 -1) r (->Lambertian (->Vec3 0.0 0.0 1.0)))
        ;; sphere2 (->Sphere (->Vec3    r  0 -1) r (->Lambertian (->Vec3 1.0 0.0 0.0)))
        lis     (vector sphere1 sphere2 sphere3 sphere4 sphere5)]
        ;; lis     (vector sphere1 sphere2)]
    (->HitableList lis (count lis))))

(defn random-coordinates []
  (for [a (range -6 6) b (range -6 6)]
    [a b]))

(defn make-random-center [[a b]]
  (->Vec3 (+ a (* 0.9 (rand)))
          0.2
          (+ b (* 0.9 (rand)))))

(defn rand*rand []
  (* (rand) (rand)))

(defn make-lambertian [center]
  (->Sphere center 0.2 (->Lambertian (->Vec3 (rand*rand) (rand*rand) (rand*rand)))))

(defn random-fcolor []                  ; float color > 0.5
  (* 0.5 (+ 1 (rand))))

(defn random-fcolors []
  {:e0 (random-fcolor) :e1 (random-fcolor) :e2 (random-fcolor)})

(defn make-metal [center]
  (->Sphere center 0.2 (->Metal (map->Vec3 (random-fcolors)) (* 0.5 (rand)))))

(defn make-dielectric [center]
  (->Sphere center 0.2 (->Dielectric 1.5)))

(defn make-random-material [center]
  (let [rv (rand)]
    (cond
      (< rv 0.8)  (make-lambertian center)
      (< rv 0.95) (make-metal center)
      :else (make-dielectric center))))

(defn close-standard-center? [center standard-center]
  (> (vector-length (minus center standard-center)) 0.9))

(defn add-main-spheres [sequ]
  (concat sequ (list (->Sphere (->Vec3 0 1 0)  1.0 (->Dielectric 1.5))
                     (->Sphere (->Vec3 -4 1 0) 1.0 (->Lambertian (->Vec3 0.4 0.2 0.1)))
                     (->Sphere (->Vec3 4 1 0)  1.0 (->Metal (->Vec3 0.7 0.6 0.5) 0.0))
                     (->Sphere (->Vec3 0 -1000 0) 1000 (->Lambertian (->Vec3 0.5 0.5 0.5))))))

(defn make-random-world [standard-center]
  (->> (random-coordinates)
       (map make-random-center)
       (filter #(close-standard-center? % standard-center))
       (map make-random-material)
       (add-main-spheres)
       (vec)
       (#(->HitableList % (count %)))))

(defn body [nx ny ns]
  (let [camera (make-camera nx ny)
        ;; world (make-world)
        world  (make-random-world (->Vec3 4 0.2 0))
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

