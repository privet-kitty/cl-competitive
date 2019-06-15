(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../convex-hull.lisp"))

(use-package :test-util)

(with-test (:name convex-hull)
  (assert (equalp #() (make-convex-hull! (vector))))
  (assert (equalp #(#c(1 3)) (make-convex-hull! (vector #c(1 3)))))
  (assert (equalp #(#c(2 -4) #c(2 -4))
                  (make-convex-hull! (vector #c(2 -4) #c(2 -4)))))
  (assert (equalp #(#c(0 0) #c(4 0) #c(2 2) #c(1 2) #c(0 1))
                  (make-convex-hull! (vector #c(0 0) #c(1 0) #c(2 0) #c(3 0) #c(4 0) #c(0 1) #c(1 1) #c(2 1) #c(3 1) #c(1 2) #c(2 2)))))
  ;; three points in a straight line are excluded if eps is positive
  (assert (equalp #(#c(0 0) #c(4 0) #c(2 2) #c(1 2) #c(0 1))
                  (make-convex-hull! (vector #c(0 0) #c(1 0) #c(2 0) #c(3 0) #c(4 0) #c(0 1) #c(1 1) #c(2 1) #c(3 1) #c(1 2) #c(2 2)) 1d-5)))
  ;; three points in a straight line are included if eps is negative
  (assert (equalp #(#c(0 0) #c(1 0) #c(2 0) #c(3 0) #c(4 0) #c(3 1) #c(2 2) #c(1 2) #c(0 1))
                  (make-convex-hull! (vector #c(0 0) #c(1 0) #c(2 0) #c(3 0) #c(4 0) #c(0 1) #c(1 1) #c(2 1) #c(3 1) #c(1 2) #c(2 2)) -1d-5))))
