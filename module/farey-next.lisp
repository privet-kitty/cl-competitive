(defpackage :cp/farey-next
  (:use :cl :cp/bezout)
  (:export #:farey-next #:farey-prev))
(in-package :cp/farey-next)

(declaim (inline farey-next))
(defun farey-next (num denom max-denom)
  "Returns the next number of a Farey sequence. Returns two values: numerator
and denominator."
  (declare ((integer 1) denom max-denom)
           ((integer 0) num))
  (assert (and (< num denom) (<= denom max-denom)))
  (multiple-value-bind (res-denom res-num) (solve-bezout (- num) denom 1 nil max-denom)
    (values res-num res-denom)))

(declaim (inline farey-prev))
(defun farey-prev (num denom max-denom)
  "Returns the next number of a Farey sequence "
  (declare ((integer 1) num denom max-denom))
  (assert (and (<= num denom) (<= denom max-denom)))
  (multiple-value-bind (res-denom res-num) (solve-bezout num (- denom) 1 nil max-denom)
    (values res-num res-denom)))
