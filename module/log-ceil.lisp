(defpackage :cp/log-ceil
  (:use :cl)
  (:export #:log2-ceil #:log-ceil))
(in-package :cp/log-ceil)

(declaim (inline log2-ceil))
(defun log2-ceil (x)
  "Rounds up log2(x). Special case: (log2-ceil 0) |-> 0"
  (let ((ceil (ceiling x)))
    (declare ((integer 0) ceil))
    (integer-length (- ceil 1))))

(declaim (inline log-ceil))
(defun log-ceil (x base)
  "Rounds up log(x). Signals DIVISION-BY-ZERO if X is zero."
  (declare (real x)
           ((integer 2) base))
  (when (zerop x)
    (error 'division-by-zero :operands (list 0 base) :operation 'log-ceil))
  (if (integerp x)
      (loop for i from 0
            for y = x then (ceiling y base)
            when (<= y 1)
            do (return i))
      (nth-value 0 (ceiling (log x base)))))
