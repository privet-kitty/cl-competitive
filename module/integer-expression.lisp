(defpackage :cp/integer-expression
  (:use :cl)
  (:export #:integer-reverse #:palindrome-integer-p #:integer-concat))
(in-package :cp/integer-expression)

(declaim (inline integer-reverse))
(defun integer-reverse (x &optional (base 10))
  "Returns the integer displayed in reversed order of X."
  (declare (integer x))
  (let ((sign (signum x))
        (x (abs x))
        (res 0))
    (loop (when (zerop x)
            (return (* res sign)))
          (multiple-value-bind (quot rem) (floor x base)
            (setq res (+ (* base res) rem)
                  x quot)))))

(declaim (inline palindrome-integer-p))
(defun palindrome-integer-p (x &optional (base 10))
  "Returns true iff X is palindromically displayed."
  (= x (integer-reverse x base)))

(declaim (inline integer-concat))
(defun integer-concat (x y &optional (base 10))
  (declare ((integer 0 #.most-positive-fixnum) x y))
  (let ((coef 1))
    (declare ((integer 0 #.most-positive-fixnum) base))
    (loop until (> coef y)
          do (setq coef (* coef base)))
    (+ (* x coef) y)))
