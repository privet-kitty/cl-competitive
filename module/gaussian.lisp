(defpackage :cp/gaussian
  (:use :cl)
  (:export #:gaussian)
  (:documentation "Provides normal distribution using Box-Muller transform.

Approximate speed: 10^7 samples per second."))
(in-package :cp/gaussian)

(declaim (inline gaussian))
(defun gaussian ()
  "Returns a double-float that is sampled from the standard normal
distribution."
  (let* ((x (- 1d0 (random 1d0))) ; avoid zero
         (y (random 1d0)))
    (* (sqrt (the (double-float 0d0) (* -2d0 (log x))))
       (cos (* 2d0 pi y)))))
