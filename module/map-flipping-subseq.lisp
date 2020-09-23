(defpackage :cp/map-flipping-subseq
  (:use :cl)
  (:export #:map-flipping-subseq))
(in-package :cp/map-flipping-subseq)

(declaim (inline map-flipping-subseq))
(defun map-flipping-subseq (function vector &key (test #'eql))
  "Applies FUNCTION to each `flipping' subarray of VECTOR. `Flipping' here means
that each two adjacent elements are different. FUNCTION receives two arguments L
and R which expresses an interval [L, R).

CL-USER> (map-flipping-subseq (lambda (x y) (format t \"~&~D ~D\" x y))
                              #(1 0 1 0 1 1 1 0))
0 5
5 6
6 8
"
  (declare (vector vector))
  (let ((n (length vector))
        (base 0))
    (declare ((integer 0 #.most-positive-fixnum) base))
    (loop for i from 1 below n
          when (funcall test (aref vector i) (aref vector (- i 1)))
          do (funcall function base i)
             (setq base i)
          finally (funcall function base n))))
