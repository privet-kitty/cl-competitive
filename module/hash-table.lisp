(defpackage :cp/hash-table
  (:use :cl)
  (:export #:hash-table-keys #:hash-table-values #:alist-hash-table
           #:keys-hash-table #:hash-table-nunion #:hash-table-intersection #:hash-table-intersect-p))
(in-package :cp/hash-table)

(declaim (inline hash-table-keys))
(defun hash-table-keys (hash-table)
  (loop for key being each hash-key of hash-table
        collect key))

(declaim (inline hash-table-values))
(defun hash-table-values (hash-table)
  (loop for value being each hash-value of hash-table
        collect value))

(declaim (inline alist-hash-table))
(defun alist-hash-table (alist &key (test #'eql))
  (let ((table (make-hash-table :test test)))
    (dolist (pair alist table)
      (setf (gethash (car pair) table) (cdr pair)))))

(declaim (inline list-hash-table))
(defun keys-hash-table (seq &key (test #'eql) (initial-value t))
  (let ((table (make-hash-table :test test)))
    (sb-sequence:dosequence (key seq)
      (setf (gethash key table) initial-value))
    table))

(defun hash-table-nunion (hash-table1 hash-table2)
  (when (< (hash-table-count hash-table1) (hash-table-count hash-table2))
    (rotatef hash-table1 hash-table2))
  (maphash (lambda (key value)
             (setf (gethash key hash-table1) value))
           hash-table2)
  hash-table1)

(defun hash-table-intersection (hash-table1 hash-table2)
  (when (< (hash-table-count hash-table1) (hash-table-count hash-table2))
    (rotatef hash-table1 hash-table2))
  (let ((result (make-hash-table :test (hash-table-test hash-table1))))
    (maphash (lambda (key value)
               (when (gethash key hash-table1)
                 (setf (gethash key result) value)))
             hash-table2)
    result))

(defun hash-table-intersect-p (hash-table1 hash-table2)
  (when (< (hash-table-count hash-table1) (hash-table-count hash-table2))
    (rotatef hash-table1 hash-table2))
  (maphash (lambda (key value)
             (when (gethash key hash-table1)
               (return-from hash-table-intersect-p t)))
           hash-table2))
