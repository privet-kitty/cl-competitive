(declaim (inline find-cycle))
(defun find-cycle (graph &key wrap undirected)
  "Finds a simple cycle in GRAPH and returns a list of vertices. The consequence
is undefined when GRAPH contains self-loop.

If WRAP is true, this function adds the same vertex to the first and the last of
the list. If UNDIRECTED is true, this function ignores cycles of length 2."
  (declare (vector graph))
  (let* ((n (length graph))
         (tmp-marked (make-array n :element-type 'bit :initial-element 0))
         (marked (make-array n :element-type 'bit :initial-element 0)))
    (labels ((visit (v prev path)
               (declare (fixnum v prev))
               (when (= 0 (aref marked v))
                 (when (= 1 (aref tmp-marked v))
                   (loop for suffix on (cdr path)
                         when (= (the fixnum (car suffix)) v)
                         do (loop-finish)
                         finally (setf (cdr suffix) nil))
                   (return-from find-cycle
                     (nreverse (if wrap path (cdr path)))))
                 (setf (aref tmp-marked v) 1)
                 (dolist (next (aref graph v))
                   (declare (fixnum next))
                   (unless (and undirected (= prev next))
                     (visit next v (cons next path))))
                 (setf (aref marked v) 1))))
      (dotimes (v n)
        (when (= 0 (aref marked v))
          (visit v -1 (list v)))))))
