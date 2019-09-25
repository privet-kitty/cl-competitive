(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../sliding-optimum.lisp"))

(use-package :test-util)

(with-test (:name sliding-optimum)
  (assert (equalp #(1 1 2 2 3 -1) (calc-sliding-opt #(5 1 4 2 3 5 7 -1) 3 #'<)))
  (assert (equalp #(5 4 4 5 7 7) (calc-sliding-opt #(5 1 4 2 3 5 7 -1) 3 #'>)))
  (assert (equalp #(-1) (calc-sliding-opt #(5 1 4 2 3 5 7 -1) 8 #'<)))
  (signals error (calc-sliding-opt #(5 1 4 2 3 5 7 -1) 9 #'>))

  (assert (equalp #(10) (calc-sliding-opt #(10) 1 #'<)))
  (assert (equalp #(10) (calc-sliding-opt #(10) 1 #'>)))
  (locally (declare (muffle-conditions warning))
    (signals error (calc-sliding-opt #() 0 #'<))))
