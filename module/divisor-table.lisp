(defpackage :cp/divisor-table
  (:use :cl)
  (:export #:make-divisors-table))
(in-package :cp/divisor-table)

(deftype uint () '(integer 0 #.most-positive-fixnum))

(declaim (ftype (function * (values (simple-array t (*)) &optional))
                %make-divisors-table))
(defun %make-divisors-list-table (max)
  "Returns a vector of length MAX+1 whose each cell, vector[X], is a list
containing all the divisors of X in ascending order. Time and space complexity
is O(Nlog(N)). Note that vector[0] = NIL."
  (declare (optimize (speed 3))
           (uint max)
           #+sbcl (sb-ext:muffle-conditions style-warning))
  (let ((result (make-array (+ 1 max) :element-type 'list))
        (tails (make-array (+ 1 max) :element-type 'list))) ; stores the last cons cell
    (declare (optimize (speed 3) (safety 0)))
    (loop for i from 1 to max
          for cell = (list 1)
          do (setf (aref result i) cell
                   (aref tails i) cell))
    (setf (aref result 0) nil)
    (loop for divisor from 2 to max
          do (loop for number from divisor to max by divisor
                   do (setf (cdr (aref tails number)) (list divisor)
                            (aref tails number) (cdr (aref tails number)))))
    result))

(declaim (ftype (function * (values (simple-array t (*)) &optional))
                %make-divisors-vector-table))
(defun %make-divisors-vector-table (max)
  "Returns a vector of length MAX+1 whose each cell, vector[X], is a vector
containing all the divisors of X in ascending order. Time and space complexity
is O(Nlog(N)). Note that vector[0] = #()."
  (declare (optimize (speed 3))
           (uint max))
  (let ((counter (make-array (+ max 1) :element-type 'uint :initial-element 1))
        (res (make-array (+ max 1) :element-type t)))
    (loop for d from 2 to max
          do (loop for x from d to max by d
                   do (incf (aref counter x))))
    (setf (aref res 0) (make-array 0 :element-type 'uint))
    (loop for i from 1 to max
          for vector = (make-array (aref counter i) :element-type 'uint)
          do (setf (aref vector 0) 1
                   (aref res i) vector
                   (aref counter i) 1))
    (loop for d from 2 to max
          do (loop for x from d to max by d
                   for end = (aref counter x)
                   for vector of-type (simple-array uint (*)) = (aref res x)
                   do (setf (aref counter x) (+ end 1)
                            (aref vector end) d)))
    res))

(declaim (ftype (function * (values (simple-array t (*)) &optional))
                make-divisors-table))
(defun make-divisors-table (max sequence-type)
  "Returns a vector of length MAX+1 whose each cell, vector[X], is a vector or
list containing all the divisors of X in ascending order. Time complexity is
O(Nlog(N)). Note that vector[0] is an empty sequence."
  (declare ((member list vector) sequence-type))
  (ecase sequence-type
    (list (%make-divisors-list-table max))
    (vector (%make-divisors-vector-table max))))
