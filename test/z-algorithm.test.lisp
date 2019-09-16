(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../z-algorithm.lisp"))

(use-package :test-util)

(with-test (:name z-algorithm)
  (assert (equalp #() (make-z-array "")))
  (assert (equalp #(1) (make-z-array "a")))
  (assert (equalp #(4 3 2 1) (make-z-array "aaaa")))
  (assert (equalp #(6 0 4 0 2 0) (make-z-array "ababab")))
  (assert (equalp #(11 0 0 1 0 1 0 4 0 0 1) (make-z-array "abracadabra"))))
