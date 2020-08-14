(defpackage :cp/test/matrix-rotate
  (:use :cl :fiveam :cp/matrix-rotate)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/matrix-rotate)
(in-suite base-suite)

(test matrix-rotate
  (declare (notinline matrix-rotate))
  (let ((mat #2a((1 2 3) (4 5 6))))
    (is (equalp #2a((1 2 3) (4 5 6)) (matrix-rotate mat 0)))
    (is (equalp #2a((3 6) (2 5) (1 4)) (matrix-rotate mat 1)))
    (is (equalp #2a((6 5 4) (3 2 1)) (matrix-rotate mat 2)))
    (is (equalp #2a((4 1) (5 2) (6 3)) (matrix-rotate mat 3)))
    (is (equalp #2a((1 2 3) (4 5 6)) (matrix-rotate mat -4)))
    (is (equalp #2a((3 6) (2 5) (1 4)) (matrix-rotate mat 5)))
    (is (equalp #2a((6 5 4) (3 2 1)) (matrix-rotate mat -2)))
    (is (equalp #2a((4 1) (5 2) (6 3)) (matrix-rotate mat 7))))
  ;; empty case
  (finishes
    (dotimes (i 100)
      (assert (equalp #2a() (matrix-rotate #2a() (- (random 100) 100)))))))

(test matrix-transpose
  (declare (notinline matrix-transpose))
  (let ((mat #2a((1 2 3) (4 5 6))))
    (is (equalp #2a((1 4) (2 5) (3 6)) (matrix-transpose mat))))
  ;; empty case
  (is (equalp #2a() (matrix-transpose #2a()))))
