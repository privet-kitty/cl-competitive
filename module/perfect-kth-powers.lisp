(defpackage :cp/perfect-kth-powers
  (:use :cl :cp/mod-power)
  (:export #:make-perfect-kth-powers))
(in-package :cp/perfect-kth-powers)

(declaim (inline make-perfect-kth-powers))
(defun make-perfect-kth-powers (minfactor-table length exp modulus
                                &key (element-type '(unsigned-byte 31)))
  "Returns a vector of perfect EXP-th powers: VECTOR[x] = x^EXP mod MODULUS. Use
CP/LINEAR-SIEVE:MAKE-MINFACTOR-TABLE to generate MINFACTOR-TABLE. Time
complexity is O(LENGTH * log(EXP) / log(LENGTH))."
  (declare (vector minfactor-table)
           ((integer 0 #.most-positive-fixnum) length exp)
           ((integer 1 #.most-positive-fixnum) modulus))
  (assert (<= length (length minfactor-table)))
  (let ((result (make-array length :element-type element-type :initial-element 0)))
    (let ((one (mod 1 modulus)))
      (when (>= length 1)
        (when (zerop exp)
          (setf (aref result 0) one))
        (when (>= length 2)
          (setf (aref result 1) one))))
    (loop for x from 2 below length
          for minfactor = (aref minfactor-table x)
          when (= x minfactor)
          do (setf (aref result x) (mod-power x exp modulus))
          else
          do (setf (aref result x)
                   (mod (* (aref result minfactor)
                           (aref result (floor x minfactor)))
                        modulus)))
    result))
