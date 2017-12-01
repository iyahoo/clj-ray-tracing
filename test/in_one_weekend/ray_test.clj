(ns in-one-weekend.ray-test
  (:require [clojure.test :refer :all]
            [in-one-weekend.ray :refer :all]
            [in-one-weekend.vec :refer :all]))

(deftest raymethods
  (testing "point-at-parameter"
    (is (equals (point-at-parameter (->Ray3 (->Vec3 1 2 3) (->Vec3 1 2 3)) 2)
                (->Vec3 3 6 9)))))
