;;;
;;; Arithmetic operations with dynamic modulus
;;;

(declaim ((unsigned-byte 32) *modulus*))
(defvar *modulus*)

(defun mod* (&rest args)
  (reduce (lambda (x y) (mod (* x y) *modulus*)) args))

(sb-c:define-source-transform mod* (&rest args)
  (if (null args)
      1
      (reduce (lambda (x y) `(mod (* ,x ,y) *modulus*)) args)))

(defun mod+ (&rest args)
  (reduce (lambda (x y) (mod (+ x y) *modulus*)) args))

(sb-c:define-source-transform mod+ (&rest args)
  (if (null args)
      0
      (reduce (lambda (x y) `(mod (+ ,x ,y) *modulus*)) args)))

(define-modify-macro incfmod (delta &optional (divisor '*modulus*))
  (lambda (x y divisor) (mod (+ x y) divisor)))

(define-modify-macro decfmod (delta &optional (divisor '*modulus*))
  (lambda (x y divisor) (mod (- x y) divisor)))
