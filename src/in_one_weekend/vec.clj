(ns in-one-weekend.vec)

(defprotocol Vec
  (x [this])
  (y [this])
  (z [this])
  (r [this])
  (g [this])
  (b [this]))

(defrecord Vec3 [e0 e1 e2]
  Vec
  (x [this] (:e0 this))
  (y [this] (:e1 this))
  (z [this] (:e2 this))
  (r [this] (:e0 this))
  (g [this] (:e1 this))
  (b [this] (:e2 this)))

;; For test
(defonce v1 (->Vec3 1 2 3))

(defmethod print-method Vec3 [x ^java.io.Writer w]
  (.write w (str (:e0 x) " " (:e1 x) " " (:e2 x))))

(defmulti %apply-vec (fn [f this value] (class value)))

(defmethod ^Vec3 %apply-vec Vec3 [f this v1]
  (apply ->Vec3 (map f (vals this) (vals v1))))

(defmethod ^Vec3 %apply-vec Number [f this v1]
  (apply ->Vec3 (map #(f % v1) (vals this))))

(defn ^Vec3 apply-vec
  ([f v]
   (apply ->Vec3 (map f (vals v))))
  ([f v1 v2]
   (%apply-vec f v1 v2)))

;; Calc methods

(defn plus
  ([this]
   this)
  ([this v1]
   (apply-vec + this v1)))

(defn minus
  ([this]
   (apply-vec - this))
  ([this v1]
   (apply-vec - this v1)))

(defn times [this v1]
  (apply-vec * this v1))

(defn divs [this v1]
  (apply-vec / this v1))

;; Definition methods

(defn make-unit-vector [this]
  (let [elems (vals this)
        k (/ 1.0 (Math/sqrt (reduce + (map * elems elems))))]
    (apply ->Vec3 (map #(* %1 k) elems))))

(defn squared-length [this]
  (reduce + (map #(* % %) (vals this))))

(defn length [this]
  (Math/sqrt (squared-length this)))

;; Equality

(defn difference [x y]
  (Math/abs (double (- x y))))

(defn close? [tolerance x y]
  (< (difference x y) tolerance))

(defn float-vec? [v]
  (reduce #(or %1 %2) (map float? (vals v))))

(defmulti equals (fn [this v1] (or (float-vec? this) (float-vec? v1))))

(defmethod equals false [this v1]
  (.equals this v1))

(defmethod equals true [this v1]
  (reduce #(or %1 %2) (map #(close? 0.00001 %1 %2) (vals this) (vals v1))))
