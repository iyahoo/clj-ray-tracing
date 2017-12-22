(ns in-one-weekend.core-test
  (:require [clojure.test :refer :all]
            [in-one-weekend.core :refer :all]))

;; (deftest bodytest
;;   (testing "body"
;;     (is (= (body 3 3)
;;            "0 170 51\n85 170 51\n170 170 51\n0 85 51\n85 85 51\n170 85 51
;; 0 0 51\n85 0 51\n170 0 51\n"))))

(deftest core-methods
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
           [[1 0] [1 1] [1 2] [0 0] [0 1] [0 2]]))))
