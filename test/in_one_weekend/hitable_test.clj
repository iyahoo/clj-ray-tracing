(ns in-one-weekend.hitable-test
  (:require [clojure.test :refer :all]
            [in-one-weekend.hitable :refer :all]))

(deftest hitable-methods
  (testing "HitRecord"
    (is (.equals (->HitRecord 1 2 3 1)
                 {:t 1 :p 2 :normal 3 :id 1}))))
