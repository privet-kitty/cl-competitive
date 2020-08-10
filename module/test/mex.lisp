(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../mex.lisp"))

(use-package :test-util)

(with-test (:name mex)
  (assert (= 0 (mex)))
  (assert (= 0 (mex 1 2 4 7)))
  (assert (= 3 (mex 0 1 2 4 7)))
  (assert (= 5 (mex 0 1 2 3 4))))
