(defpackage :cp/test/gauss-jordan
  (:use :cl :fiveam :cp/gauss-jordan :cp/test/nearly-equal)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/gauss-jordan)
(in-suite base-suite)

(test solve-linear-system
  (let* ((matrix (make-array '(3 4)
                             :element-type 'double-float
                             :initial-contents '((2d0 3d0 4d0 0d0)
                                                 (3d0 -2d0 0d0 0d0)
                                                 (7d0 1d0 -5d0 0d0))))
         (bs (make-array 3 :element-type 'double-float :initial-contents '(1d0 -6d0 9d0)))
         (res (solve-linear-system matrix bs)))
    (is-true res)
    (is (nearly= 1d-8 (aref res 0) (float -32/133 1d0)))
    (is (nearly= 1d-8 (aref res 1) (float 351/133 1d0)))
    (is (nearly= 1d-8 (aref res 2) (float -214/133 1d0)))
    (is (nearly= 1d-8 (aref res 3) 0d0))))
