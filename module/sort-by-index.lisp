(defpackage :cp/sort-by-index
  (:use :cl :cp/symmetric-group)
  (:export #:sort-by-index!))
(in-package :cp/sort-by-index)

;; TODO: define-source-transform

(declaim (inline sort-by-index!))
(defun sort-by-index! (order &rest vectors)
  "Sorts VECTORS by ORDER and returns a corresponding vector of ordered
indices. ORDER is a function that takes two indices. (You can pass no vectors
and just get a vector of indices without any problems.)"
  (declare (inline sort))
  (when vectors
    (let* ((n (length (car vectors)))
           (perm (iota n))
           (tmp (make-array n :element-type t)))
      (sort perm order)
      (dolist (vector vectors)
        (dotimes (i n)
          (setf (aref tmp i) (aref vector (aref perm i))))
        (dotimes (i n)
          (setf (aref vector i) (aref tmp i))))
      perm)))
