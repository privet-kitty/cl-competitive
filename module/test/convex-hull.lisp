(defpackage :cp/test/convex-hull
  (:use :cl :fiveam :cp/convex-hull)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/convex-hull)
(in-suite base-suite)

(test convex-hull
  (declare (notinline make-convex-hull!))
  ;; Example from Wikipedia: https://en.wikipedia.org/wiki/Convex_hull
  (is (equalp #() (make-convex-hull! (vector))))
  (is (equalp #(#c(1 3)) (make-convex-hull! (vector #c(1 3)))))
  (is (equalp #(#c(2 -4) #c(2 -4))
              (make-convex-hull! (vector #c(2 -4) #c(2 -4)))))
  (is (equalp #(#c(0 0) #c(4 0) #c(2 2) #c(1 2) #c(0 1))
              (make-convex-hull! (vector #c(0 0) #c(1 0) #c(2 0) #c(3 0) #c(4 0) #c(0 1) #c(1 1) #c(2 1) #c(3 1) #c(1 2) #c(2 2)))))
  ;; three points in a straight line are excluded if eps is positive
  (is (equalp #(#c(0 0) #c(4 0) #c(2 2) #c(1 2) #c(0 1))
              (make-convex-hull! (vector #c(0 0) #c(1 0) #c(2 0) #c(3 0) #c(4 0) #c(0 1) #c(1 1) #c(2 1) #c(3 1) #c(1 2) #c(2 2)) 1d-5)))
  ;; three points in a straight line are included if eps is negative
  (is (equalp #(#c(0 0) #c(1 0) #c(2 0) #c(3 0) #c(4 0) #c(3 1) #c(2 2) #c(1 2) #c(0 1))
              (make-convex-hull! (vector #c(0 0) #c(1 0) #c(2 0) #c(3 0) #c(4 0) #c(0 1) #c(1 1) #c(2 1) #c(3 1) #c(1 2) #c(2 2)) -1d-5))))
