(defpackage :set-equal
  (:use :cl)
  (:export #:set-equal #:multiset-equal))
(in-package :set-equal)

(declaim (inline set-equal))
(defun set-equal (seq1 seq2 &key (test #'eql))
  "Returns true iff SEQ1 and SEQ2 are identical as a set."
  (let ((table (make-hash-table :test test)))
    (sequence:dosequence (elm seq1)
      (setf (gethash elm table) t))
    (sequence:dosequence (elm seq2)
      (unless (nth-value 1 (gethash elm table))
        (return-from set-equal nil))
      (setf (gethash elm table) nil))
    (loop for value being each hash-value of table
          always (null value))))

(declaim (inline multiset-equal))
(defun multiset-equal (seq1 seq2 &key (test #'eql))
  "Returns true iff SEQ1 and SEQ2 are identical as a multiset."
  (declare (sequence seq1 seq2))
  (let ((table (make-hash-table :test test)))
    (sequence:dosequence (elm seq1)
      (incf (the (integer 0 #.most-positive-fixnum) (gethash elm table 0))))
    (sequence:dosequence (elm seq2)
      (multiple-value-bind (count present-p) (gethash elm table)
        (declare ((or null (integer 0 #.most-positive-fixnum)) count))
        (cond ((not present-p)
               (return-from multiset-equal nil))
              ((eql 1 count)
               (remhash elm table))
              (t (setf (gethash elm table) (- count 1))))))
    (zerop (hash-table-count table))))
