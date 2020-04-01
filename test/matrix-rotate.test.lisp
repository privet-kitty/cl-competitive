(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../matrix-rotate.lisp"))

(use-package :test-util)

(with-test (:name matrix-rotate)
  (let ((mat #2a((1 2 3) (4 5 6))))
    (assert (equalp #2a((1 2 3) (4 5 6)) (matrix-rotate mat 0)))
    (assert (equalp #2a((3 6) (2 5) (1 4)) (matrix-rotate mat 1)))
    (assert (equalp #2a((6 5 4) (3 2 1)) (matrix-rotate mat 2)))
    (assert (equalp #2a((4 1) (5 2) (6 3)) (matrix-rotate mat 3)))
    (assert (equalp #2a((1 2 3) (4 5 6)) (matrix-rotate mat -4)))
    (assert (equalp #2a((3 6) (2 5) (1 4)) (matrix-rotate mat 5)))
    (assert (equalp #2a((6 5 4) (3 2 1)) (matrix-rotate mat -2)))
    (assert (equalp #2a((4 1) (5 2) (6 3)) (matrix-rotate mat 7))))
  ;; empty case
  (dotimes (i 100)
    (assert (equalp #2a() (matrix-rotate #2a() (- (random 100) 100))))))

(with-test (:name matrix-transpose)
  (let ((mat #2a((1 2 3) (4 5 6))))
    (assert (equalp #2a((1 4) (2 5) (3 6)) (matrix-transpose mat))))
  ;; empty case
  (assert (equalp #2a() (matrix-transpose #2a()))))
