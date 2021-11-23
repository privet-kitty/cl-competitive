(defpackage :cp/montmort
  (:use :cl)
  (:export #:make-montmort-sequence))
(in-package :cp/montmort)

(declaim (inline make-montmort-sequence))
(defun make-montmort-sequence (length modulus &optional (element-type '(unsigned-byte 31)))
  (declare ((mod #.array-dimension-limit) length)
           ((integer 1 #.most-positive-fixnum) modulus))
  (let ((res (make-array length :element-type element-type :initial-element 0)))
    (when (> length 0)
      (setf (aref res 0) (mod 1 modulus)))
    (loop for i from 1 below length
          do (setf (aref res i)
                   (mod (+ (mod (* (aref res (- i 1)) i) modulus)
                           (if (evenp i) 1 (- modulus 1)))
                        modulus)))
    res))
