(defpackage :cp/mod-binomial
  (:use :cl :cp/mod-inverse)
  (:export #:mod-binomial #:mod-multichoose))
(in-package :cp/mod-binomial)

(declaim (inline mod-binomial))
(defun mod-binomial (n k modulus)
  (declare ((integer 0 #.most-positive-fixnum) modulus))
  (if (or (< n k) (< n 0) (< k 0))
      0
      (let ((k (if (< k (- n k)) k (- n k)))
            (num 1)
            (denom 1))
        (declare ((integer 0) k num denom))
        (loop for x from n above (- n k)
              do (setq num (mod (* num x) modulus)))
        (loop for x from 1 to k
              do (setq denom (mod (* denom x) modulus)))
        (mod (* num (mod-inverse denom modulus)) modulus))))

(declaim (inline mod-multichoose))
(defun mod-multichoose (n k modulus)
  (mod-binomial (+ n k -1) k modulus))
