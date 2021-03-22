(defpackage :cp/binom-quadratic
  (:use :cl)
  (:export #:make-binom-table))
(in-package :cp/binom-quadratic)

(declaim (inline make-binom-table))
(defun make-binom-table (size element-type op)
  "Generates table of binomial coefficients. Time and space complexity is
O(N^2).

OP := addition"
  (declare ((integer 1) size))
  (let* ((table (make-array (list size size)
                            :element-type element-type
                            :initial-element (coerce 0 element-type)))
         (one (coerce 1 element-type)))
    (setf (aref table 0 0) one)
    (loop for i from 1 below size
          do (setf (aref table i 0) one)
             (loop for j from 1 below size
                   do (setf (aref table i j)
                            (funcall op
                                     (aref table (- i 1) (- j 1))
                                     (aref table (- i 1) j)))))
    table))

#+(or)
(progn
  (declaim ((simple-array (unsigned-byte 31) (* *)) *binom*))
  (sb-ext:define-load-time-global *binom*
    (make-binom-table 501
                      '(unsigned-byte 31)
                      (lambda (x y)
                        (min (+ x y) #.(ldb (byte 31 0) -1)))
                      0)))
