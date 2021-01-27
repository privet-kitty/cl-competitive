(defpackage :cp/binary-lca
  (:use :cl)
  (:export #:get-binary-lca))
(in-package :cp/binary-lca)

(declaim (inline get-binary-lca))
(defun get-binary-lca (x y)
  "Returns the LCA of two vertices X, Y, which are 1-based binary encoding of
(infinite) perfect binary tree."
  (declare ((integer 1) x y))
  (let* ((large (max x y))
         (small (min x y))
         (dif (- (integer-length large) (integer-length small)))
         (small-len (integer-length (logxor small (ash large (- dif)))))
         (large-len (+ small-len dif))
         (result (ash small (- small-len))))
    (if (= x large)
        (values result large-len small-len)
        (values result small-len large-len))))
