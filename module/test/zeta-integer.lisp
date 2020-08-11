(defpackage :cp/test/zeta-integer
  (:use :cl :fiveam :cp/zeta-integer)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/zeta-integer)
(in-suite base-suite)

(declaim (notinline divisor-transform! inverse-divisor-transform! multiple-transform! inverse-multiple-transform!))

(test divisor/multiple-transform
  (declare (notinline divisor-transform! inverse-divisor-transform! multiple-transform! inverse-multiple-transform!))
  (is (equalp #(10 1 2 2 3 2 4 2 4 3) (divisor-transform! (vector 1 1 1 1 1 1 1 1 1 1))))
  (is (equalp #(11 1 2 2 3 2 4 2 4 3 4) (divisor-transform! (vector 1 1 1 1 1 1 1 1 1 1 1))))

  (is (equalp #(1 1 1 1 1 1 1 1 1 1) (inverse-divisor-transform! (vector 10 1 2 2 3 2 4 2 4 3))))
  (is (equalp #(1 1 1 1 1 1 1 1 1 1 1) (inverse-divisor-transform! (vector 11 1 2 2 3 2 4 2 4 3 4))))

  (is (equalp #(0 9 4 3 2 1 1 1 1 1) (multiple-transform! (vector 0 1 1 1 1 1 1 1 1 1))))
  (is (equalp #(0 10 5 3 2 2 1 1 1 1 1) (multiple-transform! (vector 0 1 1 1 1 1 1 1 1 1 1))))

  (is (equalp #(0 1 1 1 1 1 1 1 1 1) (inverse-multiple-transform! (vector 0 9 4 3 2 1 1 1 1 1))))
  (is (equalp #(0 1 1 1 1 1 1 1 1 1 1) (inverse-multiple-transform! (vector 0 10 5 3 2 2 1 1 1 1 1))))
  
  ;; boundary case
  (is (equalp #(1) (divisor-transform! (vector 1))))
  (is (equalp #() (divisor-transform! (vector))))
  (is (equalp #(1) (inverse-divisor-transform! (vector 1))))
  (is (equalp #() (inverse-divisor-transform! (vector))))
  (is (equalp #(1) (multiple-transform! (vector 1))))
  (is (equalp #() (multiple-transform! (vector))))
  (is (equalp #(1) (inverse-multiple-transform! (vector 1))))
  (is (equalp #() (inverse-multiple-transform! (vector)))))
