(ns in-one-weekend.sphere-test
  (:require [clojure.test :refer :all]
            [in-one-weekend.sphere :refer :all]))

(deftest sphere-methods
  (testing "in?"
    (is (in? 3 2 5))
    (is (not (in? 2 2 2)))))
