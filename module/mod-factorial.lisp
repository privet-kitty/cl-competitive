(defpackage :cp/mod-factorial
  (:use :cl)
  (:export #:mod-factorial))
(in-package :cp/mod-factorial)

(declaim (inline mod-factorial))
(defun mod-factorial (n modulus)
  "Returns N! mod MODULUS."
  (declare ((integer 0 #.most-positive-fixnum) n modulus))
  (labels ((recur (n result)
             (declare ((integer 0 #.most-positive-fixnum) n result))
             (if (zerop n)
                 result
                 (recur (- n 1) (mod (* result n) modulus)))))
    (recur n 1)))
