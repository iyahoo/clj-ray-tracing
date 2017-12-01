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
(defonce v_test (->Vec3 1 2 3))

(defmethod print-method Vec3 [x ^java.io.Writer w]
  (.write w (str (:e0 x) " " (:e1 x) " " (:e2 x))))

(defmulti %apply-vec (fn [f v1 v2] [(class v1) (class v2)]))

(defmethod ^Vec3 %apply-vec [Vec3 Vec3] [f v1 v2]
  (apply ->Vec3 (map f (vals v1) (vals v2))))

(defmethod ^Vec3 %apply-vec [Number Vec3] [f v1 v2]
  (apply ->Vec3 (map #(f v1 %) (vals v2))))

(defmethod ^Vec3 %apply-vec [Vec3 Number] [f v1 v2]
  (apply ->Vec3 (map #(f % v2) (vals v1))))

(defn ^Vec3 apply-vec
  ([f v]
   (apply ->Vec3 (map f (vals v))))
  ([f v1 v2]
   (%apply-vec f v1 v2)))

;; Calc methods

(defn plus
  ([v] v)
  ([v1 v2]
   (apply-vec + v1 v2)))

(defn minus
  ([v]
   (apply-vec - v))
  ([v1 v2]
   (apply-vec - v1 v2)))

(defn times [v1 v2]
  (apply-vec * v1 v2))

(defn divs [v1 v2]
  (apply-vec / v1 v2))

;; Definition methods

(defn make-unit-vector [v]
  (let [elems (vals v)
        k (/ 1.0 (Math/sqrt (reduce + (map * elems elems))))]
    (apply ->Vec3 (map #(* %1 k) elems))))

(defn squared-length [v]
  (reduce + (map #(* % %) (vals v))))

(defn vector-length [v]
  (Math/sqrt (squared-length v)))

(defn dot [v1 v2]
  (reduce + (map * (vals v1) (vals v2))))

(defn cross [v1 v2]
  (->Vec3    (- (* (:e1 v1) (:e2 v2)) (* (:e2 v1) (:e1 v2)))
          (- (- (* (:e0 v1) (:e2 v2)) (* (:e2 v1) (:e0 v2))))
             (- (* (:e0 v1) (:e1 v2)) (* (:e1 v1) (:e0 v2)))))

(defn unit-vector [v]
  (divs v (vector-length v)))

;; Equality

(defn difference [x y]
  (Math/abs (double (- x y))))

(defn close? [tolerance x y]
  (< (difference x y) tolerance))

(defn float-vec? [v]
  (reduce #(or %1 %2) (map float? (vals v))))

(defmulti equals (fn [v1 v2] (or (float-vec? v1) (float-vec? v2))))

(defmethod equals false [v1 v2]
  (.equals v1 v2))

(defmethod equals true [v1 v2]
  (reduce #(and %1 %2) (map #(close? 0.00001 %1 %2) (vals v1) (vals v2))))