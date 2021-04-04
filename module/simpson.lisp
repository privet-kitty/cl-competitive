(defpackage :cp/simpson
  (:use :cl)
  (:export #:simpson))
(in-package :cp/simpson)

;; not tested
(declaim (inline simpson))
(defun simpson (function left right sample)
  "Does numerical integration of FUNCTION on [LEFT, RIGHT] using Simpson's 1/3
rule. The returned value will be rational if FUNCTION is a rational function."
  (declare ((integer 2) sample))
  (assert (evenp sample))
  (let ((h (/ (- right left) sample))
        (res 0))
    (dotimes (i (+ sample 1))
      (let ((x (if (= i sample)
                   right
                   (+ left (* h i))))
            (coef (cond ((or (= i 0) (= i sample)) 1)
                        ((evenp i) 2)
                        (t 4))))
        (incf res (* coef (funcall function x)))))
    (/ (* res h) 3)))
