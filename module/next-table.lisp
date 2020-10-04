(defpackage :cp/next-table
  (:use :cl)
  (:export #:make-next-table))
(in-package :cp/next-table)

(declaim (inline make-next-table))
(defun make-next-table (vector &key (test #'eql) (element-type 'fixnum) double)
  "Returns a vector of indices: Let the vector be NEXT; then each
VECTOR[NEXT[index]] is the first element which is equal to VECTOR[index] and is
located after the index.

Note that this function deals with VECTOR in a circular manner when DOUBLE is
false: e.g. #(3 2 3 1) |-> #(2 1 0 3). When DOUBLE is true, this function
traverses the concatenation of two VECTORs instead. #(3 2 3 1) |-> #(2 5 4 7)"
  (declare (vector vector))
  (let* ((n (length vector))
         (result (make-array n :element-type element-type))
         (table (make-hash-table :test test)))
    (if double
        (loop for i from (- (* 2 n) 1) downto n
              do (setf (gethash (aref vector (- i n)) table) i))
        (loop for i from (- n 1) downto 0
              do (setf (gethash (aref vector i) table) i)))
    (loop for i from (- n 1) downto 0
          do (setf (aref result i) (gethash (aref vector i) table)
                   (gethash (aref vector i) table) i))
    result))
