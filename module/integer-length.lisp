(defpackage :cp/integer-length
  (:use :cl)
  (:export #:integer-length*))
(in-package :cp/integer-length)

;; NOTE: Not optimized. This function is slow especially for bignum.
(declaim (inline integer-length*))
(defun integer-length* (x &optional (base 10))
  "Returns the length of the integer X when displayed with the radix
BASE. (Returns 0 when X = 0. Ignores the negative sign.)"
  (declare (integer x)
           ((integer 2 #.most-positive-fixnum) radix))
  (loop for length of-type (integer 0 #.most-positive-fixnum) from 0
        for y = (abs x) then (floor y radix)
        until (zerop y)
        finally (return length)))
