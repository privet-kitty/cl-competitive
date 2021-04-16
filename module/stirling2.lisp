(defpackage :cp/stirling2
  (:use :cl)
  (:export #:make-stirling2-table)
  (:documentation "Provides computation of Stirling number of the second
kind."))
(in-package :cp/stirling2)

(declaim (ftype (function * (values (simple-array (unsigned-byte 31) (* *)) &optional))
                make-stirling2-table))
(defun make-stirling2-table (size1 size2 modulus)
  "Returns a table of Stirling numbers of the second kind."
  (declare (optimize (speed 3))
           ((unsigned-byte 32) size1 size2 modulus))
  (let ((table (make-array (list size1 size2) :element-type '(unsigned-byte 31))))
    (when (and (> size1 0) (> size2 0))
      (setf (aref table 0 0) (mod 1 modulus)))
    (loop for n from 1 below size1
          do (loop for k from 1 below (min (+ n 1) size2)
                   do (setf (aref table n k)
                            (mod (+ (aref table (- n 1) (- k 1))
                                    (mod (* k (aref table (- n 1) k))
                                         modulus))
                                 modulus))))
    table))
