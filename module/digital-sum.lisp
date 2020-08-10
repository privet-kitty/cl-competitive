(defpackage :cp/digital-sum
  (:use :cl)
  (:export #:digital-sum))
(in-package :cp/digital-sum)

(declaim (inline digital-sum))
(defun digital-sum (x &optional (radix 10))
  "Returns the sum of the each digit of X w.r.t. RADIX. (Returns 0 when X =
0. Ignores the negative sign.)"
  (declare (integer x)
           ((integer 2 #.most-positive-fixnum) radix))
  (let ((sum 0)
        (x (abs x)))
    (declare (unsigned-byte x)
             ((integer 0 #.most-positive-fixnum) sum))
    (loop
      (when (zerop x)
        (return sum))
      (multiple-value-bind (quot rem) (floor x radix)
        (incf sum rem)
        (setq x quot)))))
