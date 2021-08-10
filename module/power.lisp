(defpackage :cp/power
  (:use :cl)
  (:export #:power)
  (:documentation "Provides fast exponentiation over any monoids."))
(in-package :cp/power)

(declaim (inline power))
(defun power (base exponent op identity)
  "OP := binary operation (comprising a monoid)
IDENTITY := identity element w.r.t. OP"
  (declare ((integer 0) exponent))
  (loop with res = identity
        while (> exponent 0)
        when (oddp exponent)
        do (setq res (funcall op res base))
        do (setq base (funcall op base base)
                 exponent (ash exponent -1))
        finally (return res)))
