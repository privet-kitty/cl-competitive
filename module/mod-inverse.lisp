(defpackage :cp/mod-inverse
  (:use :cl)
  (:export #:mod-inverse))
(in-package :cp/mod-inverse)

;; TODO: signal DIVISION-BY-ZERO for non-co-prime input when safety >= 1
(declaim (inline mod-inverse)
         (ftype (function * (values (mod #.most-positive-fixnum) &optional)) mod-inverse))
(defun mod-inverse (a modulus)
  "Solves ax ≡ 1 mod m. A and M must be coprime."
  (declare ((integer 1 #.most-positive-fixnum) modulus))
  (let ((a (mod a modulus))
        (b modulus)
        (u 1)
        (v 0))
    (declare (fixnum a b u v))
    (loop until (zerop b)
          for quot = (floor a b)
          do (decf a (the fixnum (* quot b)))
             (rotatef a b)
             (decf u (the fixnum (* quot v)))
             (rotatef u v))
    (mod u modulus)))
