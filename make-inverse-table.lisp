(declaim (inline make-reverse-inverse-table))
(defun make-inverse-table (vector &key (test #'eql))
  "Returns a hash-table that assigns each value of the (usually sorted) VECTOR
of length n to the integers 0, ..., n-1."
  (let ((table (make-hash-table :test test :size (length vector))))
    (dotimes (i (length vector) table)
      (setf (gethash (aref vector i) table) i))))

(declaim (inline make-ordered-inverse-table!))
(defun make-ordered-inverse-table! (vector &key (test #'eql) (order #'<))
  (setq vector (sort vector order))
  (let ((table (make-hash-table :test test :size (length vector)))
        (index 0))
    (dotimes (pos (length vector))
      (when (or (zerop pos)
                (not (funcall test (aref vector pos) (aref vector (- pos 1)))))
        (setf (gethash (aref vector pos) table) index)
        (incf index)))
    table))
