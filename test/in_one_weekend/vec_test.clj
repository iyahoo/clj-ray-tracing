(ns in-one-weekend.vec-test
  (:require [clojure.test :refer :all]
            [in-one-weekend.vec :refer :all]))

(defonce v (->Vec3 1 2 3))

(deftest vec-methods
  (testing "equals"
    (is (equals v v))
    (is (not (equals v (->Vec3 1 2 2))))
    (is (equals (->Vec3 (/ 1 3)   (/ 2 3)  (/ 3 3))
                (->Vec3 (/ 1 3.0) (/ 2 3.0) (/ 3 3.0)))))

  (testing "apply-vec"
    (is (equals (apply-vec + v)
                (->Vec3 1 2 3)))
    (is (equals (apply-vec + v v)
                (->Vec3 2 4 6)))
    (is (equals (apply-vec - v)
                (->Vec3 -1 -2 -3)))
    (is (equals (apply-vec - v v)
                (->Vec3 0 0 0)))
    (is (thrown? java.lang.Exception (%apply-vec + 1 1))))

  (testing "plus"
    (is (equals (plus v)   (->Vec3 1 2 3)))
    (is (equals (plus v v) (->Vec3 2 4 6)))
    (is (equals (plus v 1) (->Vec3 2 3 4)))
    (is (equals (plus 1 v) (->Vec3 2 3 4)))
    (is (equals (plus v v v) (->Vec3 3 6 9))))
  (testing "minus"
    (is (equals (minus v)   (->Vec3 -1 -2 -3)))
    (is (equals (minus v v) (->Vec3 0 0 0)))
    (is (equals (minus v 1) (->Vec3 0 1 2)))
    (is (equals (minus 1 v) (->Vec3 0 -1 -2)))
    (is (equals (minus v 1)
                (plus (- 1) v))))
  (testing "times"
    (is (equals (times v v) (->Vec3 1 4 9)))
    (is (equals (times v 1) (->Vec3 1 2 3))))
  (testing "divs"
    (is (equals (divs v v) (->Vec3 1 1 1))
        (equals (divs v 1) (->Vec3 1 2 3))))

  (testing "make-unit-vector"
    (is (equals (make-unit-vector (->Vec3 -4 -8 0))
                (->Vec3 (- (/ 1.0 (Math/sqrt 5)))
                        (- (/ 2.0 (Math/sqrt 5)))
                        0.0))))
  (testing "squared-length"
    (is (== (squared-length v) 14.0)))
  (testing "vector-length"
    (is (== (vector-length (->Vec3 2 4 4)) 6.0)))
  (testing "dot"
    (is (== (dot v v) 14)))
  (testing "cross"
    (is (equals (cross (->Vec3 1 2 0) (->Vec3 0 1 -1))
                (->Vec3 -2 1 1))))
  (testing "unit-vector"
    (is (equals (unit-vector (->Vec3 3 4 0))
                (->Vec3 (/ 3 5) (/ 4 5) 0)))))

(deftest util-functions
  (testing "random-in-unit-sphere"
    (is (<= (squared-length (random-in-unit-sphere)) 1.0)))

  (testing "angle->rad"
    (is (close? (angle->rad 180) Math/PI 0.000001))))
