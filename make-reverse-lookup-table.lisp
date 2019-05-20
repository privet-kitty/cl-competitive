(declaim (inline make-reverse-lookup-table))
(defun make-reverse-lookup-table (vector &key (test #'eql))
  "Assigns each value of the (usually sorted) VECTOR of length n to the integers
0, ..., n-1."
  (let ((table (make-hash-table :test test :size (length vector))))
    (dotimes (i (length vector) table)
      (setf (gethash (aref vector i) table) i))))
