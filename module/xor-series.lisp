(defpackage :cp/xor-series
  (:use :cl)
  (:export #:xor-series))
(in-package :cp/xor-series)

(declaim (inline xor-series))
(defun xor-series (x)
  "Returns the bitwise xor of 1, 2, ..., X."
  (declare (unsigned-byte x))
  (case (logand 3 x)
    (0 x)
    (1 1)
    (2 (+ x 1))
    (otherwise 0)))
