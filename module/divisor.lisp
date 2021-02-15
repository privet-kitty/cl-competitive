(defpackage :cp/divisor
  (:use :cl)
  (:export #:enum-divisors #:enum-ascending-divisors #:map-divisors))
(in-package :cp/divisor)

(declaim (ftype (function * (values (vector (integer 0 #.most-positive-fixnum)) &optional))
                enum-divisors))
(defun enum-divisors (x)
  "Enumerates all the divisors of X in O(sqrt(X)) time. Note that the resultant
vector is NOT sorted."
  (declare (optimize (speed 3))
           ((integer 1 #.most-positive-fixnum) x))
  (let* ((sqrt (isqrt x))
         ;; FIXME: Currently I set the initial size to x^1/4, but it's not based
         ;; on a proper reason.
         (result (make-array (isqrt sqrt)
                             :element-type '(integer 0 #.most-positive-fixnum)
                             :fill-pointer 0)))
    (loop for i from 1 to sqrt
          do (multiple-value-bind (quot rem) (floor x i)
               (when (zerop rem)
                 (vector-push-extend i result)
                 (unless (= i quot)
                   (vector-push-extend quot result)))))
    result))

(declaim (inline map-divisors))
(defun map-divisors (x function)
  "Applies FUNCTION to all the divisors of X. Note that the order of divisors
are not necessarily ascending."
  (declare ((integer 1) x))
  (let ((sqrt (isqrt x)))
    (loop for i from 1 to sqrt
          do (multiple-value-bind (quot rem) (floor x i)
               (when (zerop rem)
                 (funcall function i)
                 (unless (= i quot)
                   (funcall function quot)))))))

(defun enum-ascending-divisors (n)
  "Returns an ascending list of all the divisors of N."
  (declare (optimize (speed 3))
           ((integer 1 #.most-positive-fixnum) n))
  (if (= n 1)
      (list 1)
      (let* ((sqrt (isqrt n))
             (result (list 1)))
        (labels ((%enum (i first-half second-half)
                   (declare ((integer 1 #.most-positive-fixnum) i))
                   (cond ((or (< i sqrt)
                              (and (= i sqrt) (/= (* sqrt sqrt) n)))
                          (multiple-value-bind (quot rem) (floor n i)
                            (if (zerop rem)
                                (progn
                                  (setf (cdr first-half) (list i))
                                  (push quot second-half)
                                  (%enum (1+ i) (cdr first-half) second-half))
                                (%enum (1+ i) first-half second-half))))
                         ((= i sqrt) ; N is a square number here
                          (setf (cdr first-half) (cons i second-half)))
                         (t ; (> i sqrt)
                          (setf (cdr first-half) second-half)))))
          (%enum 2 result (list n))
          result))))

