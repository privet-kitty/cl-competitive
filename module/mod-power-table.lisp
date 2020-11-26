(defpackage :cp/mod-power-table
  (:use :cl)
  (:export #:make-mod-power-table))
(in-package :cp/mod-power-table)

(declaim (inline make-mod-power-table))
(defun make-mod-power-table (base length modulus &optional (element-type '(unsigned-byte 31)))
  "Returns a vector of the given length: VECTOR[x] := BASE^x mod MODULUS."
  (declare (fixnum base)
           ((integer 0 #.most-positive-fixnum) length)
           ((integer 1 #.most-positive-fixnum) modulus))
  (let ((res (make-array length :element-type element-type)))
    (unless (zerop length)
      (setf (aref res 0) (mod 1 modulus))
      (loop for i from 1 below length
            do (setf (aref res i)
                     (mod (* base (aref res (- i 1))) modulus))))
    res))
