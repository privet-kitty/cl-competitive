;;;
;;; Arithmetic operations with static mod
;;;

;; TODO: Is it better to use DEFTRANSFORM to optimize these functions when they
;; are passed to an inlined function?
(defmacro define-mod-operations (&optional (divisor 1000000007))
  `(progn
     (defun mod* (&rest args)
       (reduce (lambda (x y) (mod (* x y) ,divisor)) args))

     (define-compiler-macro mod* (&rest args)
       (if (null args)
           1
           (reduce (lambda (x y) `(mod (* ,x ,y) ,',divisor)) args)))

     (defun mod+ (&rest args)
       (reduce (lambda (x y) (mod (+ x y) ,divisor)) args))

     (define-compiler-macro mod+ (&rest args)
       (if (null args)
           0
           (reduce (lambda (x y) `(mod (+ ,x ,y) ,',divisor)) args)))

     (define-modify-macro incfmod (delta divisor)
       (lambda (x y divisor) (mod (+ x y) divisor)))

     (define-modify-macro decfmod (delta divisor)
       (lambda (x y divisor) (mod (- x y) divisor)))))

(define-mod-operations)
