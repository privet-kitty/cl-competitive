(defpackage :cp/run-range
  (:use :cl)
  (:export #:map-run-range))
(in-package :cp/run-range)

(declaim (inline map-run-range))
(defun map-run-range (function seq &key (test #'eql))
  "Applies FUNCTION to each equal successive element of SEQ. FUNCTION must take
three arguments: the first one receives an element in SEQ and the rest ones
receive the left-end and the right-end half-open interval in which all the
elements are equal to the first one.

Example:
\(map-run-range (lambda (x l r) (format t \"~D ~D ~D~%\" x l r)) #(1 1 1 2 2 1 3))
1 0 3
2 3 5
1 5 6
3 6 7
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
          (funcall function prev start pos)
          (setq prev elm
                start pos))
        (incf pos))
      (funcall function prev start pos)
      nil)))
