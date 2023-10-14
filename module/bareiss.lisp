(defpackage :cp/bareiss
  (:use :cl)
  (:export #:bareiss!)
  (:documentation "Provides Bareiss algorithm, a fraction-free polynomial algorithm for computing
matrix determinant."))
(in-package :cp/bareiss)

(declaim (ftype (function * (values integer
                                    (integer 0 #.most-positive-fixnum)
                                    (simple-array (integer 0 #.most-positive-fixnum) (*))
                                    &optional))
                bareiss!)
         (inline bareiss!))
(defun bareiss! (matrix)
  "Receives a m * n integer matrix such that m <= n, and returns its determinant,
rank, and a vector of linearly independent columns that were used to compute
determinant.

This function requires O(m^2n) arithmetic operations, but note that the integers
that appear in the computation can be as large as the determinant (or the
Hadamard bound, equivalently)."
  (destructuring-bind (m n) (array-dimensions matrix)
    (declare ((mod #.array-dimension-limit) m n))
    (assert (<= m n))
    (let ((prev-diag 1)
          (cols (make-array m :element-type '(integer 0 #.most-positive-fixnum)))
          (offset 0)
          (sign 1))
      (declare ((integer -1 1) sign)
               ((mod #.array-dimension-limit) offset)
               (integer prev-diag))
      (dotimes (k m)
        (loop when (= (+ k offset) n)
              do (return-from bareiss!
                   (values 0 k (subseq cols 0 k)))
              do (let ((nz-row (loop for i from k below m
                                     unless (zerop (the integer (aref matrix i (+ k offset))))
                                     do (return i))))
                   (when nz-row
                     (unless (= k nz-row)
                       (dotimes (j n)
                         (rotatef (aref matrix k j) (aref matrix nz-row j)))
                       (setq sign (- sign)))
                     (setf (aref cols k) (+ k offset))
                     (return)))
                 (incf offset))
        (let ((diag (aref matrix k (+ k offset))))
          (declare (integer diag))
          (loop
            for i from (+ k 1) below m
            do (loop
                 for j from (+ k offset 1) below n
                 do (setf (aref matrix i j)
                          (floor (- (* (the integer (aref matrix i j)) diag)
                                    (* (the integer (aref matrix i (+ k offset)))
                                       (the integer (aref matrix k j))))
                                 prev-diag))))
          (setq prev-diag diag)))
      (values (* sign prev-diag) m cols))))
