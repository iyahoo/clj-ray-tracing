(ns in-one-weekend.hitable-list-test
  (:require [clojure.test :refer :all]
            [in-one-weekend.hitable-list :refer :all]))

(deftest hitable-list-function
  (testing ""))

(defn for-destruct-test [{:keys [a b]} c d]
  (+ a b c d))

(deftest destructuring-test
  (testing "for-destruct-test"
    (= (for-destruct-test {:a 1 :b 2} 3 4)
       10)))
