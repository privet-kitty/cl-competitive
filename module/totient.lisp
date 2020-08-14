(defpackage :cp/totient
  (:use :cl)
  (:export #:euler-phi))
(in-package :cp/totient)

(declaim (inline euler-phi))
(defun euler-phi (n)
  "Returns the number of positive integers in {1, 2, ..., N} that are relatively
prime to N."
  (declare ((integer 1 #.most-positive-fixnum) n))
  (let ((res n))
    (declare ((integer 0 #.most-positive-fixnum) res))
    (loop for i of-type (integer 2 #.(isqrt most-positive-fixnum)) from 2
          while (<= (* i i) n)
          when (zerop (mod n i))
          do (decf res (floor res i))
             (loop (multiple-value-bind (quot rem) (floor n i)
                     (if (zerop rem)
                         (setq n quot)
                         (return)))))
    (when (> n 1)
      (decf res (floor res n)))
    res))
