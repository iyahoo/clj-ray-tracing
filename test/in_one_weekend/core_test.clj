(ns in-one-weekend.core-test
  (:require [clojure.test :refer :all]
            [in-one-weekend.core :refer :all]))

;; (deftest bodytest
;;   (testing "body"
;;     (is (= (body 3 3)
;;            "0 170 51\n85 170 51\n170 170 51\n0 85 51\n85 85 51\n170 85 51
;; 0 0 51\n85 0 51\n170 0 51\n"))))

(deftest core-methods
  (testing "make-coordinates"
    (is (= (make-coordinates 3 2)
           [[1 0] [1 1] [1 2] [0 0] [0 1] [0 2]]))))
