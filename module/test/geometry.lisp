(defpackage :cp/test/geometry
  (:use :cl :fiveam :cp/geometry :cp/test/nearly-equal)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/geometry)
(in-suite base-suite)

(test calc-angle
  (is (nearly= 1d-6 (* 1/2 pi) (calc-internal-angle 0 1 -1 0)))
  (is (nearly= 1d-6 (* 1/2 pi) (calc-internal-angle -1 0 0 1)))
  (is (nearly= 1d-6 (* 1/2 pi) (calc-angle 0 1 -1 0)))
  (is (nearly= 1d-6 (* 3/2 pi) (calc-angle -1 0 0 1))))
