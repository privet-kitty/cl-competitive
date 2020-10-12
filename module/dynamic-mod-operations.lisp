;;;
;;; Arithmetic operations with dynamic modulus
;;;

(defpackage :cp/dynamic-mod-operations
  (:use :cl)
  (:export #:*modulus* #:mod* #:mod+ #:incfmod #:decfmod #:mulfmod))
(in-package :cp/dynamic-mod-operations)

;; NOTE: Currently MOD* and MOD+ doesn't apply MOD when the number of
;; parameters is one. For simplicity I won't fix it for now.

(defvar *modulus* #.(+ 7 (expt 10 9)))
(declaim ((unsigned-byte 31) *modulus*)
         #+sbcl (sb-ext:always-bound *modulus*))

(defun mod* (&rest args)
  (reduce (lambda (x y) (mod (* x y) *modulus*)) args))

(defun mod+ (&rest args)
  (reduce (lambda (x y) (mod (+ x y) *modulus*)) args))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (locally (declare (sb-ext:muffle-conditions warning))
    (sb-c:define-source-transform mod* (&rest args)
      (if (null args)
          1
          (reduce (lambda (x y) `(mod (* ,x ,y) *modulus*)) args)))
    (sb-c:define-source-transform mod+ (&rest args)
      (if (null args)
          0
          (reduce (lambda (x y) `(mod (+ ,x ,y) *modulus*)) args)))))

(define-modify-macro incfmod (delta &optional (divisor '*modulus*))
  (lambda (x y divisor) (mod (+ x y) divisor)))

(define-modify-macro decfmod (delta &optional (divisor '*modulus*))
  (lambda (x y divisor) (mod (- x y) divisor)))

(define-modify-macro mulfmod (delta &optional (divisor '*modulus*))
  (lambda (x y divisor) (mod (* x y) divisor)))
