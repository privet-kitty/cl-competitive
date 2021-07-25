(defpackage :cp/hash-table
  (:use :cl)
  (:export #:hash-table-keys #:hash-table-values #:alist-hash-table #:keys-hash-table
           #:hash-table-nunion #:hash-table-intersection #:hash-table-intersect-p
           #:hash-table-merge #:hash-table-set=))
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
    (loop for (key . value) in alist
          do (setf (gethash key table) value))
    table))

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
  "Returns a hash-table containing all the keys in both HASH-TABLE1 and
HASH-TABLE2. Note that the value assigned to a key may be from HASH-TABLE1 or
HASH-TABLE2. (In other words, it is assumed that HASH-TABLE1 and HASH-TABLE2
hold the same value for the same key.) This function is non-destructive."
  (declare (optimize (speed 3)))
  (when (< (hash-table-count hash-table1) (hash-table-count hash-table2))
    (rotatef hash-table1 hash-table2))
  (let ((result (make-hash-table :test (hash-table-test hash-table1))))
    (maphash (lambda (key value)
               (when (gethash key hash-table1)
                 (setf (gethash key result) value)))
             hash-table2)
    result))

(defun hash-table-intersect-p (hash-table1 hash-table2)
  (declare (optimize (speed 3)))
  (when (< (hash-table-count hash-table1) (hash-table-count hash-table2))
    (rotatef hash-table1 hash-table2))
  (maphash (lambda (key value)
             (declare (ignore value))
             (when (gethash key hash-table1)
               (return-from hash-table-intersect-p t)))
           hash-table2))

(declaim (inline hash-table-merge))
(defun hash-table-merge (hash-table1 hash-table2 &key (op #'+) (identity 0))
  (when (< (hash-table-count hash-table1) (hash-table-count hash-table2))
    (rotatef hash-table1 hash-table2))
  (maphash (lambda (key value2)
             (let ((value1 (gethash key hash-table1 identity)))
               (setf (gethash key hash-table1)
                     (funcall op value1 value2))))
           hash-table2)
  hash-table1)

(declaim (inline hash-table-set=))
(defun hash-table-set= (hash-table1 hash-table2)
  "Returns true iff HASH-TABLE1 and HASH-TABLE2 have identical set of keys."
  (and (loop for key1 being each hash-key of hash-table1
             always (nth-value 1 (gethash key1 hash-table2)))
       (loop for key2 being each hash-key of hash-table2
             always (nth-value 1 (gethash key2 hash-table1)))))
