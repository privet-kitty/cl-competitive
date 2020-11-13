(defpackage :cp/run-length
  (:use :cl)
  (:export #:map-run-length))
(in-package :cp/run-length)

(deftype fixnum+ () '(integer 1 #.most-positive-fixnum))

(declaim (inline map-run-length))
(defun map-run-length (function seq &key (test #'eql))
  "Applies FUNCTION to each equal successive element of SEQ. FUNCTION must take
two arguments: the first one receives an element in SEQ and the second one
receives the number of the successive elements equal to the first one.

Example:
\(map-run-length (lambda (x c) (format t \"~D ~D~%\" x c)) #(1 1 1 2 2 1 3))
1 3
2 2
1 1
3 1
"
  (declare (sequence seq)
           ((or function symbol) test function))
  (unless (sb-sequence:emptyp seq)
    (let ((prev (elt seq 0))
          (start 0)
          (pos 0))
      (declare ((integer 0 #.most-positive-fixnum) start pos))
      (sb-sequence:dosequence (elm seq)
        (unless (or (zerop pos) (funcall test prev elm))
          (funcall function prev (the fixnum+ (- pos start)))
          (setq prev elm
                start pos))
        (incf pos))
      (funcall function prev (the fixnum+ (- pos start)))
      nil)))
