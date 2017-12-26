(ns in-one-weekend.core-test
  (:require [clojure.test :refer :all]
            [in-one-weekend.core :refer :all]
            [in-one-weekend.ray :refer :all]
            [in-one-weekend.vec :refer :all]
            [in-one-weekend.hitable-list :refer :all]))

(deftest core-methods
  (testing "air-color"
    (is (equals (air-color (->Ray (->Vec3 0 0 0) (->Vec3 0 0 3)))
                {:e0 0.75 :e1 0.85 :e2 1.0})))

  (testing "hit-sphere-attribute"
    (is (= (hit-sphere-attribute {:lis [{:attr :test}]} 0)
           :test))
    (is (= (hit-sphere-attribute {:lis [{:attr :test1} {:attr :test2}]} 1)
           :test2)))

  (testing "header"
    (is (= (header 1 1)
           "P3\n1 1\n255\n")))

  (testing "int-color"
    (is (= (int-color [0.5 0.5 0.5])
           [127 127 127]))
    (is (= (int-color [0.0 0.0 0.0])
           [0 0 0]))
    (is (= (int-color [1.0 1.0 1.0])
           [255 255 255])))

  (testing "make-coordinates"
    (is (= (make-coordinates 3 2)
           [[1 0] [1 1] [1 2] [0 0] [0 1] [0 2]])))

  (testing "coordinates-to-rate"
    (is (= (coordinates-to-rate [1 1] 10 10)
           [0.1 0.1]))))
