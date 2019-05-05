(declaim (inline find-optimum))
(defun find-optimum (sequence predicate &key (start 0) end)
  "Returns an index x that satisfies (FUNCALL PREDICATE SEQUENCE[x] SEQUENCE[y])
for all the indices y and returns SEQUENCE[x] as the second value."
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
       (let ((optimal (aref sequence 0))
             (index 0))
         (dotimes (i (length sequence) (values index optimal))
           (unless (funcall predicate optimal (aref sequence i))
             (setq optimal (aref sequence i)
                   index i))))))))

