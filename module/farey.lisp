(defpackage :cp/farey
  (:use :cl)
  (:export #:map-farey))
(in-package :cp/farey)

;; PAY ATTENTION TO THE STACK SIZE! This function does DFS.
(declaim (inline map-farey))
(defun map-farey (function max-denominator &optional from-end)
  "Applies FUNCTION to all the non-negative fractions in (0, 1) whose
denominator are smaller than or equal to MAX-DENOMINATOR. The order of
application is ascending [descending] if FROM-END is false [true]."
  (declare ((integer 0 #.most-positive-fixnum) max-denominator))
  (labels ((recur (num1 denom1 num2 denom2)
             (declare ((integer 0 #.most-positive-fixnum) num1 denom1 num2 denom2))
             (let ((num3 (+ num1 num2))
                   (denom3 (+ denom1 denom2)))
               (when (<= denom3 max-denominator)
                 (recur num1 denom1 num3 denom3)
                 (funcall function num3 denom3)
                 (recur num3 denom3 num2 denom2)))))
    (if from-end
        (recur 1 1 0 1)
        (recur 0 1 1 1))))
