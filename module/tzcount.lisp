(defpackage :cp/tzcount
  (:use :cl)
  (:export #:tzcount))
(in-package :cp/tzcount)

(declaim (inline tzcount))
(defun tzcount (x)
  "Is equivalent to TZCNT operation: it returns the number of trailing zero
bits. Note that (TZCOUNT 0) = -1."
  (- (integer-length (logand x (- x))) 1))

;; The following code also works though it is slower on AtCoder as LOGCOUNT is
;; not transformed to POPCNT on SBCL 1.1.14.

;; (declaim (inline tzcount2))
;; (defun tzcount2 (x)
;;   (logcount (- (logand x (- x)) 1)))
