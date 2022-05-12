(defpackage :cp/two-exponent
  (:use :cl)
  (:export #:calc-two-exp))
(in-package :cp/two-exponent)

(declaim (inline calc-two-exp))
(defun calc-two-exp (x)
  "Given non-zero X, |X| can be expressed as A * 2^B for a real number A in [1,
2) and a non-negative integer B. This function computes B. When X is zero, zero
is returned instead."
  (declare (real x))
  (when (zerop x)
    (return-from calc-two-exp 0))
  (setq x (abs x))
  (etypecase x
    (integer (- (integer-length x) 1))
    (ratio (let ((num (numerator x))
                 (denom (denominator x)))
             (declare ((integer 0) num denom))
             (if (>= num denom)
                 (loop for exp of-type (integer 0 #.most-positive-fixnum) from 0
                       until (< num denom)
                       do (setq denom (ash denom 1))
                       finally (return (- exp 1)))
                 (loop for exp of-type (integer 0 #.most-positive-fixnum) from 0
                       until (>= num denom)
                       do (setq num (ash num 1))
                       finally (return (- exp))))))
    (single-float (- (nth-value 1 (sb-kernel:decode-single-float x)) 1))
    (double-float (- (nth-value 1 (sb-kernel:decode-double-float x)) 1))))
