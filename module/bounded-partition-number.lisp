(defpackage :cp/bounded-partition-number
  (:use :cl)
  (:export #:make-bpartition-table)
  (:documentation "P_m(n, k) is the number of ways of writing n as a sum of at most k integers
in {0, 1, ..., m}.

corner cases:
P_m(0, k) = 1
P_m(n, 0) = 0 (n != 0)
P_m(n, k) = P(n, n) (k > n)
"))
(in-package :cp/bounded-partition-number)

(defun make-bpartition-table (size max modulus)
  "Generates a table of partition numbers using the recurrence relation P_max(n,
k) = P_max(n, k-1) + P_max(n-k, k) + P_max(n-k-m, k-1)."
  (declare ((integer 0 #.most-positive-fixnum) size max)
           ((integer 1 #.most-positive-fixnum) modulus))
  (let ((table (make-array (list size size) :element-type '(integer 0 #.most-positive-fixnum))))
    (dotimes (k size)
      (setf (aref table 0 k) 1))
    (loop for n from 1 below size
          do (setf (aref table n 0) 0)
             (loop for k from 1 to n
                   do (setf (aref table n k)
                            (mod (if (< n (+ k max))
                                     (+ (aref table n (- k 1))
                                        (aref table (- n k) k))
                                     (- (+ (aref table n (- k 1))
                                           (aref table (- n k) k))
                                        (aref table (the fixnum (- n k max)) (- k 1))))
                                 modulus)))
             (loop for k from (+ n 1) below size
                   do (setf (aref table n k)
                            (aref table n n))))
    table))
