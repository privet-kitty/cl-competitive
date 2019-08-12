(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../polynomial.lisp"))

(use-package :test-util)

(with-test (:name poly-floor!)
  (multiple-value-bind (quot rem) (poly-floor! #(8 10 -5 3) #(-3 2 1) 10007)
    (assert (equalp quot #(9996 3)))
    (assert (equalp rem #(9982 41 0 0))))
  (let ((res #(0 0 0 0 0)))
    (multiple-value-bind (quot rem) (poly-floor! #(8 10 -5 3) #(-3 2 1) 10007 res)
      (assert (equalp quot #(9996 3 0 0 0)))
      (assert (eq quot res))
      (assert (equalp rem #(9982 41 0 0)))))
  ;; leading zeros in dividend
  (multiple-value-bind (quot rem) (poly-floor! #(8 10 -5 3 0 0) #(-3 2 1) 10007)
    (assert (equalp quot #(9996 3)))
    (assert (equalp rem #(9982 41 0 0 0 0))))
  ;; leading zeros in divisor
  (multiple-value-bind (quot rem) (poly-floor! #(8 10 -5 3) #(-3 2 1 0) 10007)
    (assert (equalp quot #(9996 3)))
    (assert (equalp rem #(9982 41 0 0))))
  ;; leading zeros in both arguments
  (multiple-value-bind (quot rem) (poly-floor! #(8 10 -5 3 0 0) #(-3 2 1 0) 10007)
    (assert (equalp quot #(9996 3)))
    (assert (equalp rem #(9982 41 0 0 0 0))))
  ;; degenerative case
  (multiple-value-bind (quot rem) (poly-floor! #(8 10 -5 3 0 0) #(10 -5 3 0) 10007)
    (assert (equalp quot #(0 1)))
    (assert (equalp rem #(8 0 0 0 0 0))))
  ;; division by zero
  (signals division-by-zero (poly-floor! #(8 10 -5 3) #(0 0 0 0) 10007))
  (signals division-by-zero (poly-floor! #(8 10 -5 3) #() 10007))
  (signals division-by-zero (poly-floor! #() #() 10007))
  ;; null dividend
  (multiple-value-bind (quot rem) (poly-floor! #() #(10 -5 3 0) 10007)
    (assert (equalp quot #()))
    (assert (equalp rem #()))))
