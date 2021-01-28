;;;
;;; Arithmetic operations with dynamic modulus
;;;

(defpackage :cp/dynamic-mod-operations
  (:use :cl)
  (:export #:*modulus* #:mod* #:mod+ #:incfmod #:decfmod #:mulfmod))
(in-package :cp/dynamic-mod-operations)

;; NOTE: Currently MOD* and MOD+ doesn't apply MOD when the number of
;; parameters is one. For simplicity I won't fix it for now.

(defvar *modulus*)
(declaim ((unsigned-byte 31) *modulus*))

(defun mod* (&rest args)
  (cond ((cdr args) (reduce (lambda (x y) (mod (* x y) *modulus*)) args))
        (args (mod (car args) *modulus*))
        (t 1)))

(defun mod+ (&rest args)
  (cond ((cdr args) (reduce (lambda (x y) (mod (+ x y) *modulus*)) args))
        (args (mod (car args) *modulus*))
        (t 0)))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (locally (declare (sb-ext:muffle-conditions warning))
    (sb-c:define-source-transform mod* (&rest args)
      (case (length args)
        (0 1)
        (1 `(mod ,(car args) *modulus*))
        (otherwise (reduce (lambda (x y) `(mod (* ,x ,y) *modulus*)) args))))
    (sb-c:define-source-transform mod+ (&rest args)
      (case (length args)
        (0 0)
        (1 `(mod ,(car args) *modulus*))
        (otherwise (reduce (lambda (x y) `(mod (+ ,x ,y) *modulus*)) args))))))

(define-modify-macro incfmod (delta &optional (divisor '*modulus*))
  (lambda (x y divisor) (mod (+ x y) divisor)))

(define-modify-macro decfmod (delta &optional (divisor '*modulus*))
  (lambda (x y divisor) (mod (- x y) divisor)))

(define-modify-macro mulfmod (delta &optional (divisor '*modulus*))
  (lambda (x y divisor) (mod (* x y) divisor)))
