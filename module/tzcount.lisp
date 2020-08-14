(defpackage :cp/tzcount
  (:use :cl)
  (:export #:tzcount))
(in-package :cp/tzcount)

(declaim (inline tzcount))
(defun tzcount (x)
  "Returns the number of trailing zero bits. Note that (TZCOUNT 0) = -1."
  (- (integer-length (logand x (- x))) 1))
