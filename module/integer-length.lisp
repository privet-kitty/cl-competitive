(defpackage :cp/integer-length
  (:use :cl)
  (:export #:integer-length*))
(in-package :cp/integer-length)

(declaim (inline integer-length*))
(defun integer-length* (x &optional (radix 10))
  "Returns the length of the integer X when displayed in RADIX. (Returns 0 when
X = 0. Ignores the negative sign.)"
  (declare (integer x)
           ((integer 2 #.most-positive-fixnum) radix))
  (loop for length of-type (integer 0 #.most-positive-fixnum) from 0
        for y = (abs x) then (floor y radix)
        until (zerop y)
        finally (return length)))

