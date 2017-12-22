(ns in-one-weekend.material-test
  (:require [clojure.test :refer :all]
            [in-one-weekend.material :refer :all]
            [in-one-weekend.vec :refer :all]))

(deftest material-methods
  (testing "reflect"
    (is (equals (reflect (->Vec3 1.0 1.0 0.0) (->Vec3 0.0 1.0 0.0))
                (->Vec3 1.0 -1.0 0.0))))

  (testing "acute-angle?"
    (is (acute-angle? (->Vec3 1 1 1) (->Vec3 10 3 1)))
    (is (not (acute-angle? (->Vec3 -1 -1 0) (->Vec3 1 1 0))))))
