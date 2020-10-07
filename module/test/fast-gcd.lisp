(defpackage :cp/test/fast-gcd
  (:use :cl :fiveam :cp/fast-gcd)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/fast-gcd)
(in-suite base-suite)

(test fast-gcd
  (finishes
    (dotimes (x 30)
      (dotimes (y 30)
        (assert (= (gcd x y) (fast-gcd x y)))
        (assert (= (gcd (- most-positive-fixnum x) (- most-positive-fixnum y))
                   (fast-gcd (- most-positive-fixnum x) (- most-positive-fixnum y))))
        (assert (= (lcm x y) (fast-lcm x y)))
        (assert (= (lcm (- most-positive-fixnum x) (- most-positive-fixnum y))
                   (fast-lcm (- most-positive-fixnum x) (- most-positive-fixnum y))))))))
