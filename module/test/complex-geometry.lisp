(defpackage :cp/test/complex-geometry
  (:use :cl :fiveam :cp/complex-geometry)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/complex-geometry)
(in-suite base-suite)

(test cross*
  (is (= 0 (cross* #c(1 3) #c(2 6))))
  (is (= -6 (cross* #c(5 7) #c(13 17)))))

(test inside-convex-polygon-p
  (let ((tri #(#c(0 0) #c(2 0) #c(0 3))))
    (is (inside-convex-polygon-p #c(1 1) tri))
    (is (not (inside-convex-polygon-p #c(1 2) tri)))
    (is (not (inside-convex-polygon-p #c(0 0) tri)))
    (is (not (inside-convex-polygon-p #c(2 0) tri)))
    (is (not (inside-convex-polygon-p #c(0 3) tri)))))
