(ns in-one-weekend.material-test
  (:require [clojure.test :refer :all]
            [in-one-weekend.material :refer :all]
            [in-one-weekend.vec :refer :all]))

(deftest material-methods
  (testing "reflect"
    (is (equals (reflect (->Vec3 1.0 1.0 0.0) (->Vec3 0.0 1.0 0.0))
                (->Vec3 1.0 -1.0 0.0))))

  (testing "schlick by https://www.wolframalpha.com/input/?i=((1-0.5)+%2F+(1+%2B+0.5))%5E2+%2B+(1+-+((1-0.5)+%2F+(1+%2B+0.5))%5E2)*(1-cos60)%5E5"
    (is (close? (schlick (Math/cos (angle->rad 60)) 0.5)
                0.13888888
                0.000001)))
  (testing "schlick by https://www.wolframalpha.com/input/?i=((1-0.2)+%2F+(1+%2B+0.2))%5E2+%2B+(1+-+((1-0.2)+%2F+(1+%2B+0.2))%5E2)*(1-cos30)%5E5"
    (is (close? (schlick (Math/cos (angle->rad 30)) 0.2)
                0.444468
                0.000001)))

  (testing "acute-angle?"
    (is (acute-angle? (->Vec3 1 1 1) (->Vec3 10 3 1)))
    (is (not (acute-angle? (->Vec3 -1 -1 0) (->Vec3 1 1 0))))))
