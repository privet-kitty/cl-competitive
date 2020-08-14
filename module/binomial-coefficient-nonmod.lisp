;;;
;;; Generate table of binomial coefficients
;;; Time and space complexity: O(N^2)
;;;

(defpackage :cp/binomial-coefficient-nonmod
  (:use :cl)
  (:export #:*binom* #:make-binom-table))
(in-package :cp/binomial-coefficient-nonmod)

;; TODO: non-global handling

(defun make-binom-table (size &optional (infinity #xffffffff))
  (check-type infinity (unsigned-byte 32))
  (let* ((table (make-array (list size size)
                            :element-type '(unsigned-byte 32)
                            :initial-element 0)))
    (setf (aref table 0 0) 1)
    (loop for i from 1 below size
          do (setf (aref table i 0) 1)
             (loop for j from 1 below size
                   do (setf (aref table i j)
                            (min (+ (aref table (- i 1) (- j 1))
                                    (aref table (- i 1) j))
                                 infinity))))
    table))

(declaim ((simple-array (unsigned-byte 32) (* *)) *binom*))
(defparameter *binom* (make-binom-table 501))
