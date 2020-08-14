(defpackage :cp/test/welzl
  (:use :cl :fiveam :cp/welzl :cp/test/nearly-equal)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/welzl)
(in-suite base-suite)

(test welzl
  (declare (notinline calc-smallest-circle))
  (let ((points (vector #c(10 9) #c(5 9) #c(2 0) #c(0 0) #c(2 7)
                        #c(3 3) #c(2 5) #c(10 0) #c(3 7) #c(1 9))))
    (is (nearly= 1d-6
                 6.726812
                 (nth-value 1 (calc-smallest-circle points 1d-8))))
    (is (nearly= 1d-6
                 6.726812
                 (nth-value 1 (calc-smallest-circle
                               (map '(simple-array (complex double-float) (*))
                                    (lambda (p) (coerce p '(complex double-float)))
                                    points)
                               1d-8))))))
