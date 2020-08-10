;;;
;;; Arithmetic operations with static modulus
;;;

(defpackage :cp/mod-operations
  (:use :cl)
  (:export #:define-mod-operations))
(in-package :cp/mod-operations)

;; NOTE: Currently MOD* and MOD+ doesn't apply MOD when the number of
;; parameters is one. For simplicity I won't fix it for now.
(defmacro define-mod-operations (divisor)
  (let ((mod* (intern "MOD*"))
        (mod+ (intern "MOD+"))
        (incfmod (intern "INCFMOD"))
        (decfmod (intern "DECFMOD"))
        (mulfmod (intern "MULFMOD")))
    `(progn
       (defun ,mod* (&rest args)
         (reduce (lambda (x y) (mod (* x y) ,divisor)) args))

       (defun ,mod+ (&rest args)
         (reduce (lambda (x y) (mod (+ x y) ,divisor)) args))

       #+sbcl
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (locally (declare (sb-ext:muffle-conditions warning))
           (sb-c:define-source-transform ,mod* (&rest args)
             (if (null args)
                 1
                 (reduce (lambda (x y) `(mod (* ,x ,y) ,',divisor)) args)))
           (sb-c:define-source-transform ,mod+ (&rest args)
             (if (null args)
                 0
                 (reduce (lambda (x y) `(mod (+ ,x ,y) ,',divisor)) args)))))

       (define-modify-macro ,incfmod (delta)
         (lambda (x y) (mod (+ x y) ,divisor)))

       (define-modify-macro ,decfmod (delta)
         (lambda (x y) (mod (- x y) ,divisor)))

       (define-modify-macro ,mulfmod (multiplier)
         (lambda (x y) (mod (* x y) ,divisor))))))
