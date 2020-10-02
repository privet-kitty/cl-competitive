(defpackage :cp/sorting-number
  (:use :cl :cp/log-ceil)
  (:export #:calc-sorting-number #:calc-binary-entropy))
(in-package :cp/sorting-number)

;; A001855
(declaim (inline calc-sorting-number))
(defun calc-sorting-number (n)
  (declare ((integer 0) n))
  (let ((log2 (log2-ceil n)))
    (+ 1 (- (* n log2) (ash 1 log2)))))

;; A003314
(declaim (inline calc-binary-entropy))
(defun calc-binary-entropy (n)
  (declare ((integer 1) n))
  (+ (calc-sorting-number n) (- n 1)))
