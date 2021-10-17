(defpackage :cp/log-factorial
  (:use :cl)
  (:export #:log-factorial #:log-binomial)
  (:documentation "Provides approximate computation of log(n!) by asymptotic
expansion."))
(in-package :cp/log-factorial)

(defparameter *bernoulli*
  #.(coerce
     (mapcar (lambda (x) (float x 1d0))
             '(1 -1/2 1/6 0 -1/30 0 1/42 0 -1/30 0 5/66 0 -691/2730 0 7/6 0 -3617/510 0 43867/798 0 -174611/330 0 854513/138 0 -236364091/2730 0 8553103/6 0 -23749461029/870 0 8615841276005/14332 0))
     '(simple-array double-float (*)))
  "Bernoulli number")
(declaim ((simple-array double-float (*)) *bernoulli*)
         #+sbcl (sb-ext:always-bound *bernoulli*))

;; exact values for small input
(defparameter *factorial-table*
  (make-array 21
              :element-type 'double-float
              :initial-contents '(1d0 1d0 2d0 6d0 24d0 120d0 720d0 5040d0 40320d0 362880d0 3628800d0 39916800d0 479001600d0 6227020800d0 87178291200d0 1307674368000d0 20922789888000d0 355687428096000d0 6402373705728000d0 121645100408832000d0 2432902008176640000d0)))
(declaim ((simple-array double-float (*)) *factorial-table*)
         #+sbcl (sb-ext:always-bound *factorial-table*))

;; FIXME: Maybe we should rely on a convergent series instead of the asymptotic
;; expansion. See
;; https://en.wikipedia.org/wiki/Stirling%27s_approximation#A_convergent_version_of_Stirling's_formula
(declaim (ftype (function * (values double-float &optional)) log-factorial))
(defun log-factorial (n &optional (number-of-terms 4))
  "Returns log(n!). Note that the returned value is just an approximation."
  (declare (optimize (speed 3))
           ((integer 0) n)
           ((integer 0 10) number-of-terms))
  (if (<= n 20)
      (log (the (double-float 1d0) (aref *factorial-table* n)))
      (let ((n (float n 1d0)))
        (+ #.(log (sqrt (* 2d0 pi)))
           (* (+ n 0.5d0) (log n))
           (- n)
           (loop for i2 from 2 to (* number-of-terms 2) by 2
                 sum (/ (aref *bernoulli* i2)
                        (* i2 (- i2 1) (expt n (- i2 1))))
                 of-type double-float)))))

(defun log-binomial (n k &optional (number-of-terms 4))
  (declare ((integer 0) n k))
  (if (>= n k 0)
      (- (log-factorial n number-of-terms)
         (log-factorial (- n k) number-of-terms)
         (log-factorial k number-of-terms))
      sb-ext:double-float-negative-infinity))
