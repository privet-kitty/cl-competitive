;;;
;;; Arithmetic operations with static modulus
;;;

(defmacro define-mod-operations (divisor)
  `(progn
     (defun mod* (&rest args)
       (reduce (lambda (x y) (mod (* x y) ,divisor)) args))

     (sb-c:define-source-transform mod* (&rest args)
       (if (null args)
           1
           (reduce (lambda (x y) `(mod (* ,x ,y) ,',divisor)) args)))

     (defun mod+ (&rest args)
       (reduce (lambda (x y) (mod (+ x y) ,divisor)) args))

     (sb-c:define-source-transform mod+ (&rest args)
       (if (null args)
           0
           (reduce (lambda (x y) `(mod (+ ,x ,y) ,',divisor)) args)))

     (define-modify-macro incfmod (delta)
       (lambda (x y) (mod (+ x y) ,divisor)))

     (define-modify-macro decfmod (delta)
       (lambda (x y) (mod (- x y) ,divisor)))

     (define-modify-macro mulfmod (multiplier)
       (lambda (x y) (mod (* x y) ,divisor)))))
