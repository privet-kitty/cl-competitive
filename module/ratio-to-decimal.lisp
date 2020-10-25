(defpackage :cp/ratio-to-decimal
  (:use :cl)
  (:export #:ratio-to-decimal))
(in-package :cp/ratio-to-decimal)

(defun ratio-to-decimal (ratio precision)
  "Returns a VECTOR of length PRECISION, which stores a decimal expression of
RATIO: VECTOR[0] stores the integer part and VECTOR[i] for i > 0 stores the i-th
decimal place.

Example:
\(ratio-to-decimal 41/4 5)
=> #(10 2 5 0 0)"
  (declare ((integer 0 #.most-positive-fixnum) precision))
  (assert (/= 0 ratio))
  (let ((res (make-array precision :element-type 'fixnum :initial-element 0))
        (num (numerator ratio))
        (denom (denominator ratio)))
    (declare (unsigned-byte num denom))
    (dotimes (i precision)
      (multiple-value-bind (quot rem) (floor num denom)
        (setf (aref res i) quot)
        (setq num (* rem 10))))
    res))
