(ns in-one-weekend.sphere-test
  (:require [clojure.test :refer :all]
            [in-one-weekend.sphere :refer :all]))

(deftest sphere-methods
  (testing "between?"
    (is (between? 3 2 5))
    (is (not (between? 2 2 2)))))
