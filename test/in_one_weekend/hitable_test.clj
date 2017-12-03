(ns in-one-weekend.hitable-test
  (:require [clojure.test :refer :all]
            [in-one-weekend.hitable :refer :all]))

(deftest hitable-methods
  (testing "hit-record"
    (is (= (struct hit-record 1 2 3)
           {:t 1 :p 2 :normal 3}))))
