(declaim (inline find-argopt))
(defun find-argopt (iterable predicate &key start end (key #'identity))
  "Returns an index (or key) x at which ITERABLE takes the minimum (or maximum,
depending on PREDICATE), and returns ITERABLE[x] as the second value.

To explain the behaviour briefly, when (FUNCALL PREDICATE (AREF ITERABLE
<index>) (AREF ITERABLE <index of current optimum>)) holds, <index> and <index
of current optimum> are swapped.

When ITERABLE is a hash-table, START and END are ignored."
  (declare ((or null (integer 0 #.most-positive-fixnum)) start end)
           ((or hash-table sequence) iterable))
  (labels ((invalid-range-error ()
             (error "Can't find optimal value in null interval [~A, ~A) on ~A" start end iterable)))
    (etypecase iterable
      (list
       (let* ((start (or start 0))
              (end (or end most-positive-fixnum))
              (iterable (nthcdr start iterable)))
         (when (or (null iterable)
                   (>= start end))
           (invalid-range-error))
         (let ((opt-element (car iterable))
               (opt-index start)
               (pos start))
           (dolist (x iterable)
             (when (>= pos end)
               (return-from find-argopt (values opt-index opt-element)))
             (when (funcall predicate (funcall key x) (funcall key opt-element))
               (setq opt-element x
                     opt-index pos))
             (incf pos))
           (values opt-index opt-element))))
      (vector
       (let ((start (or start 0))
             (end (or end (length iterable))))
         (when (or (>= start end)
                   (>= start (length iterable)))
           (invalid-range-error))
         (let ((opt-element (aref iterable start))
               (opt-index start))
           (loop for i from start below end
                 for x = (aref iterable i)
                 do (when (funcall predicate (funcall key x) (funcall key opt-element))
                      (setq opt-element x
                            opt-index i)))
           (values opt-index opt-element))))
      (hash-table
       (assert (and (null start) (null end)))
       (when (zerop (hash-table-count iterable))
         (invalid-range-error))
       (let* ((opt-value (gensym))
              (opt-key opt-value))
         (maphash
          (lambda (hash-key x)
            (when (or (eq opt-key opt-value)
                      (funcall predicate (funcall key x) (funcall key opt-value)))
              (setq opt-value x
                    opt-key hash-key)))
          iterable)
         (values opt-key opt-value))))))

