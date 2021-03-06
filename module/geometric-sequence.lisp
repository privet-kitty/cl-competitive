(defpackage :cp/geometric-sequence
  (:use :cl)
  (:export #:make-geometric-sequence))
(in-package :cp/geometric-sequence)

(declaim (inline make-geometric-sequence))
(defun make-geometric-sequence (rate length modulus &key (scale 1) (element-type '(unsigned-byte 31)))
  "Returns a vector of the given length: VECTOR[x] := SCALE * (RATE^x) mod
MODULUS."
  (declare (fixnum rate scale)
           ((mod #.array-total-size-limit) length)
           ((integer 1 #.most-positive-fixnum) modulus))
  (let ((result (make-array length :element-type element-type)))
    (unless (zerop length)
      (setf (aref result 0) (mod scale modulus))
      (loop for i from 1 below length
            do (setf (aref result i)
                     (mod (* rate (aref result (- i 1))) modulus))))
    result))
