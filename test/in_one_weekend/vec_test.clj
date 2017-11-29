(ns in-one-weekend.vec-test
  (:require [clojure.test :refer :all]
            [in-one-weekend.vec :refer :all]))

(defonce v (->Vec3 1 2 3))

(deftest vec-methods
  (testing "apply-vec"
    (is (equals (apply-vec + v)
                (->Vec3 1 2 3)))    
    (is (equals (apply-vec + v v)
                (->Vec3 2 4 6)))
    (is (equals (apply-vec - v)
                (->Vec3 -1 -2 -3)))
    (is (equals (apply-vec - v v)
                (->Vec3 0 0 0))))
  
  (testing "plus"
    (is (equals (plus v)   (->Vec3 1 2 3)))
    (is (equals (plus v v) (->Vec3 2 4 6)))
    (is (equals (plus v 1) (->Vec3 2 3 4))))
  (testing "minus"
    (is (equals (minus v)   (->Vec3 -1 -2 -3)))
    (is (equals (minus v v) (->Vec3 0 0 0)))
    (is (equals (minus v 1) (->Vec3 0 1 2))))
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
  (testing "length"
    (is (== (length (->Vec3 2 4 4)) 6.0))))
