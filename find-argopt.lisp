(declaim (inline find-argopt))
(defun find-argopt (sequence predicate &key (start 0) end (key #'identity))
  "Returns an index x at which SEQUENCE takes the minimal (or maximal, depending
on PREDICATE) value, and returns SEQUENCE[x] as the second value.

To explain the behaviour briefly, when (FUNCALL PREDICATE (AREF SEQUENCE
<index>) (AREF SEQUENCE <index of current optimum>)) holds, <index> and <index
of current optimum> are swapped."
  (declare ((or null (integer 0 #.most-positive-fixnum)) end)
           ((integer 0 #.most-positive-fixnum) start)
           (sequence sequence))
  (labels ((invalid-range-error ()
             (error "Can't find optimal value in null interval [~A, ~A) on ~A" start end sequence)))
    (etypecase sequence
      (list
       (let ((sequence (nthcdr start sequence))
             (end (or end most-positive-fixnum)))
         (when (or (null sequence)
                   (>= start end))
           (invalid-range-error))
         (let ((opt-element (car sequence))
               (opt-index start)
               (pos start))
           (dolist (x sequence)
             (when (>= pos end)
               (return-from find-argopt (values opt-index opt-element)))
             (when (funcall predicate (funcall key x) (funcall key opt-element))
               (setq opt-element x
                     opt-index pos))
             (incf pos))
           (values opt-index opt-element))))
      (vector
       (let ((end (or end (length sequence))))
         (when (or (>= start end)
                   (>= start (length sequence)))
           (invalid-range-error))
         (let ((opt-element (aref sequence start))
               (opt-index start))
           (loop for i from start below end
                 for x = (aref sequence i)
                 do (when (funcall predicate (funcall key x) (funcall key opt-element))
                      (setq opt-element x
                            opt-index i)))
           (values opt-index opt-element)))))))
