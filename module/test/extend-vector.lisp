(defpackage :cp/test/extend-vector
  (:use :cl :fiveam :cp/extend-vector)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/extend-vector)
(in-suite base-suite)

(test extend-vectorf/hand
  (let ((vec (make-array 3 :element-type 'base-char :initial-element #\a)))
    (extend-vectorf vec 10 #\b)
    (is (= 16 (length vec)))
    (is (eql 'base-char (array-element-type vec)))
    (is (char= #\a (aref vec 0) (aref vec 2)))
    (is (char= #\b (aref vec 3) (aref vec 9))))
  ;; extend to the same length
  (let ((vec (make-array 3 :element-type 'base-char :initial-element #\a)))
    (extend-vectorf vec 3 #\b)
    (is (= 3 (length vec)))
    (is (eql 'base-char (array-element-type vec)))
    (is (char= #\a (aref vec 0) (aref vec 2))))
  ;; extend to a less length
  (let ((vec (make-array 3 :element-type 'base-char :initial-element #\a)))
    (extend-vectorf vec 2 #\b)
    (is (= 3 (length vec)))
    (is (eql 'base-char (array-element-type vec)))
    (is (char= #\a (aref vec 0) (aref vec 2))))
  ;; extend-vector creates a new object for a non-adjustable vector
  (let* ((orig-vec (make-array 3))
         (vec orig-vec))
    (extend-vectorf vec 10)
    (is (not (eq vec orig-vec))))
  ;; extend-vector doesn't create a new object for an adjustable vector
  (let* ((orig-vec (make-array 3 :adjustable t))
         (vec orig-vec))
    (extend-vectorf vec 10)
    (is (eq vec orig-vec))))
