(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../zeta-integer.lisp"))

(use-package :test-util)

(with-test (:name divisor/multiple-transform)
  (assert (equalp #(10 1 2 2 3 2 4 2 4 3) (divisor-transform! (vector 1 1 1 1 1 1 1 1 1 1))))
  (assert (equalp #(11 1 2 2 3 2 4 2 4 3 4) (divisor-transform! (vector 1 1 1 1 1 1 1 1 1 1 1))))

  (assert (equalp #(1 1 1 1 1 1 1 1 1 1) (inverse-divisor-transform! (vector 10 1 2 2 3 2 4 2 4 3))))
  (assert (equalp #(1 1 1 1 1 1 1 1 1 1 1) (inverse-divisor-transform! (vector 11 1 2 2 3 2 4 2 4 3 4))))

  (assert (equalp #(0 9 4 3 2 1 1 1 1 1) (multiple-transform! (vector 0 1 1 1 1 1 1 1 1 1))))
  (assert (equalp #(0 10 5 3 2 2 1 1 1 1 1) (multiple-transform! (vector 0 1 1 1 1 1 1 1 1 1 1))))

  (assert (equalp #(0 1 1 1 1 1 1 1 1 1) (inverse-multiple-transform! (vector 0 9 4 3 2 1 1 1 1 1))))
  (assert (equalp #(0 1 1 1 1 1 1 1 1 1 1) (inverse-multiple-transform! (vector 0 10 5 3 2 2 1 1 1 1 1))))
  
  ;; boundary case
  (assert (equalp #(1) (divisor-transform! (vector 1))))
  (assert (equalp #() (divisor-transform! (vector))))
  (assert (equalp #(1) (inverse-divisor-transform! (vector 1))))
  (assert (equalp #() (inverse-divisor-transform! (vector))))
  (assert (equalp #(1) (multiple-transform! (vector 1))))
  (assert (equalp #() (multiple-transform! (vector))))
  (assert (equalp #(1) (inverse-multiple-transform! (vector 1))))
  (assert (equalp #() (inverse-multiple-transform! (vector)))))
