(ns in-one-weekend.camera-test
  (:require [clojure.test :refer :all]
            [in-one-weekend.camera :refer :all]
            [in-one-weekend.vec :refer :all]))

(deftest cameramethod
  (testing "orthonormal-basis"
    (let [[w u v] (orthonormal-basis (->Vec3 1 1 0)
                                     (->Vec3 2 -2 0)
                                     (->Vec3 0 0 3))]
      (are [x] (close? x 0 0.00001)
        (dot w u)
        (dot u v)
        (dot w v))

      (are [x] (close? x 1 0.00001)
        (vector-length w)
        (vector-length u)
        (vector-length v)))))
