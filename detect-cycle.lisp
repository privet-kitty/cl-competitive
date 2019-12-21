(declaim (ftype (function * (values list &optional)) detect-cycle))
(defun detect-cycle (graph &optional wrap)
  "Detects a simple cycle in GRAPH and returns a list of vertices. Note that
this function doesn't detect a cycle of length 1 (i.e. self-loop) or 2.

If WRAP is true, the first and the last vertices of the returned list are the
same."
  (let ((pivot -1)
        cycle
        (marked (make-array (length graph) :element-type 'bit :initial-element 0)))
    (labels ((recur (v prev)
               (declare ((integer -1 #.most-positive-fixnum) v prev))
               (when (= 1 (aref marked v))
                 (setq pivot v)
                 (when wrap
                   (push v cycle))
                 (return-from recur (list v)))
               (setf (aref marked v) 1)
               (dolist (neighbor (aref graph v))
                 (declare ((integer 0 #.most-positive-fixnum) neighbor))
                 (unless (or (= prev neighbor) (= neighbor v))
                   (recur neighbor v))
                 (when (>= pivot 0)
                   (when (= pivot v)
                     (return-from detect-cycle (cons pivot cycle)))
                   (push v cycle)
                   (return)))))
      (dotimes (v (length graph))
        (when (zerop (aref marked v))
          (recur v -1)))
      nil)))
