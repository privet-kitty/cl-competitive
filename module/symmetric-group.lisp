;;;
;;; Some operations on symmetric group
;;;

(defpackage :cp/symmetric-group
  (:use :cl)
  (:export #:decompose-to-cycles #:perm* #:perm-inverse #:iota))
(in-package :cp/symmetric-group)

;; NOTE: Here the underlying set is 0-based: {0, 1, 2, ..., N-1}

(declaim (inline decompose-to-cycles))
(defun decompose-to-cycles (permutation)
  "Returns the list of all the cyclic permutations in PERMUTATION and returns
its the distance to the identity permutation, (0, 1, ..., N-1),
w.r.t. swapping."
  (declare (vector permutation))
  (let* ((n (length permutation))
         result
         (visited (make-array n :element-type 'bit :initial-element 0))
         (sign 0))
    (declare ((integer 0 #.most-positive-fixnum) sign))
    (dotimes (init n)
      (when (zerop (sbit visited init))
        (push (loop for x = init then (aref permutation x)
                    until (= (sbit visited x) 1)
                    collect x
                    do (setf (sbit visited x) 1)
                       (incf sign))
              result)
        (decf sign)))
    (values result sign)))

(declaim (inline perm*))
(defun perm* (perm1 perm2)
  "Composes two permutations. (Actually the arguments doesn't need to be
permutations. This is just a composition of two maps.)"
  (let* ((n (length perm1))
         (result (make-array n :element-type (array-element-type perm2))))
    (dotimes (i n)
      (setf (aref result i) (aref perm2 (aref perm1 i))))
    result))

(declaim (inline perm-inverse))
(defun perm-inverse (perm)
  "Returns the inverse of a given permutation."
  (let* ((n (length perm))
         (result (make-array n :element-type 'fixnum)))
    (dotimes (i n)
      (setf (aref result (aref perm i)) i))
    result))

(declaim (inline iota))
(defun iota (size)
  "Returns #(0 1 2 ... SIZE-1)."
  (declare ((integer 0 #.most-positive-fixnum) size))
  (let ((result (make-array size :element-type 'fixnum)))
    (dotimes (i size)
      (setf (aref result i) i))
    result))
