(defpackage :cp/test/trisect
  (:use :cl :fiveam :cp/trisect)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/trisect)
(in-suite base-suite)

(defun make-func (vec)
  (lambda (pos) (aref vec pos)))

(test trisect-left
  (assert (= 4 (trisect-left (make-func #(3 2 0 -1 -2 -2 -2 -1 0)) 0 8)))
  (assert (= 5 (trisect-left (make-func #(3 2 0 -1 -2 -2 -2 -1 0)) 5 8)))
  (assert (= 3 (trisect-left (make-func #(3 2 0 -1 -2 -2 -2 -1 0)) 0 3)))
  (assert (= 3 (trisect-left (make-func #(3 4 5 8 8 8 8 7 6 0)) 0 9 :order #'>)))
  (assert (= 2 (trisect-left (make-func #(3 4 5 8 8 8 8 7 6 0)) 0 2 :order #'>)))
  (assert (= 2 (trisect-left (make-func #(#\d #\c #\b #\b #\c #\z)) 0 5 :order #'char<))))
