(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../geometry.lisp")
  (load "./nearly-equal.lisp"))

(use-package :test-util)

(with-test (:name calc-angle)
  (assert (nearly= 1d-6 (* 1/2 pi) (calc-internal-angle 0 1 -1 0)))
  (assert (nearly= 1d-6 (* 1/2 pi) (calc-internal-angle -1 0 0 1)))
  (assert (nearly= 1d-6 (* 1/2 pi) (calc-angle 0 1 -1 0)))
  (assert (nearly= 1d-6 (* 3/2 pi) (calc-angle -1 0 0 1))))
