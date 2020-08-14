(defpackage :cp/inverse-table
  (:use :cl)
  (:export #:make-inverse-table #:make-monotone-inverse-table!))
(in-package :cp/inverse-table)

(declaim (inline make-reverse-inverse-table))
(defun make-inverse-table (vector &key (test #'eql))
  "Returns a hash-table that assigns each value of the (usually sorted) VECTOR
of length n to the integers 0, ..., n-1."
  (let ((table (make-hash-table :test test :size (length vector))))
    (dotimes (i (length vector) table)
      (setf (gethash (aref vector i) table) i))))

(declaim (inline make-monotone-inverse-table!))
(defun make-monotone-inverse-table! (vector &key (test #'eql) (order #'<))
  "Sorts VECTOR, deletes all adjacent duplicates, and returns a hash-table that
assigns each value of the vector to the integers 0, 1, ..."
  (declare (function test order)
           (vector vector)
           (inline sort))
  (setq vector (sort vector order))
  (let ((table (make-hash-table :test test :size (length vector)))
        (index 0))
    (declare ((integer 0 #.most-positive-fixnum) index))
    (dotimes (pos (length vector))
      (when (or (zerop pos)
                (not (funcall test (aref vector pos) (aref vector (- pos 1)))))
        (setf (gethash (aref vector pos) table) index)
        (incf index)))
    (values table index)))
