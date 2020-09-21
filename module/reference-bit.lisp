;;;
;;; This is an implementation of binary indexed tree, specialized for the
;;; ordinary `+' operation. You'd better use abstract-bit.lisp instead. I leave
;;; it just for my reference.
;;;

(defpackage :cp/reference-bit
  (:use :cl)
  (:export #:cp/bitree-inc! #:cp/bitree-sum #:coerce-to-bitree! #:count-inversions))
(in-package :cp/reference-bit)

(declaim (inline bitree-inc!))
(defun bitree-inc! (bitree index delta)
  "Destructively increments the vector: vector[INDEX] += DELTA"
  (let ((len (length bitree)))
    (do ((i index (logior i (+ i 1))))
        ((>= i len) bitree)
      (declare ((integer 0 #.most-positive-fixnum) i))
      (incf (aref bitree i) delta))))

(declaim (inline bitree-sum))
(defun bitree-sum (bitree end)
  "Returns the prefix sum: vector[0] + ... + vector[END-1]."
  (declare ((integer 0 #.most-positive-fixnum) end))
  (let ((res 0))
    (do ((i (- end 1) (- (logand i (+ i 1)) 1)))
        ((< i 0) res)
      (declare ((integer -1 #.most-positive-fixnum) i))
      (incf res (aref bitree i)))))

(defun coerce-to-bitree! (vector)
  "Destructively constructs BIT from VECTOR."
  (loop with len = (length vector)
        for i below len
        for dest-i = (logior i (+ i 1))
        when (< dest-i len)
        do (incf (aref vector dest-i) (aref vector i))
        finally (return vector)))

;; Example: count inversions in a sequence
(declaim (inline make-inverse-lookup-table))
(defun make-inverse-lookup-table (vector &key (test #'eql))
  "Assigns each value of the (usually sorted) VECTOR of length n to the integers
0, ..., n-1."
  (let ((table (make-hash-table :test test :size (length vector))))
    (dotimes (i (length vector) table)
      (setf (gethash (aref vector i) table) i))))

(defun count-inversions (vector &key (order #'<))
  (declare (vector vector))
  (let* ((len (length vector))
         (inv-lookup-table (make-inverse-lookup-table (sort (copy-seq vector) order)))
         (bitree (make-array len :element-type '(integer 0 #.most-positive-fixnum)))
         (inversion-number 0))
    (declare (integer inversion-number))
    (loop for j below len
          for element = (aref vector j)
          for compressed = (gethash element inv-lookup-table)
          for delta of-type integer = (- j (bitree-sum bitree (1+ compressed)))
          do (incf inversion-number delta)
             (bitree-inc! bitree compressed 1))
    inversion-number))

(progn
  (assert (= 3 (count-inversions #(2 4 1 3 5))))
  (assert (zerop (count-inversions #(0))))
  (assert (zerop (count-inversions #())))
  (assert (zerop (count-inversions #(1 2))))
  (assert (= 1 (count-inversions #(2 1)))))
