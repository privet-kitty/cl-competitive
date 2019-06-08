;; unfinished.

(defun find-optimum (sequence predicate &key (start 0) end)
  "Returns an index x that satisfies (NOT (FUNCALL PREDICATE SEQUENCE[y]
SEQUENCE[x])) (i.e. SEQUENCE[x] >= SEQUENCE[y]) for all the indices y and
returns SEQUENCE[x] as the second value."
  (declare ((or null (integer 0 #.most-positive-fixnum)) end)
           ((integer 0 #.most-positive-fixnum) start)
           (function predicate)
           (sequence sequence))
  (etypecase sequence
    (list (error "Not implemented yet."))
    (vector
     (let ((end (or end (length sequence))))
       (unless (<= start end)
         (error "Can't find optimal value in null interval [~A, ~A)" start end))
       (let ((optimum (aref sequence 0))
             (index 0))
         (dotimes (i (length sequence) (values index optimum))
           (unless (funcall predicate optimum (aref sequence i))
             (setq optimum (aref sequence i)
                   index i))))))))
