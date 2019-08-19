;;;
;;; Strongly connected components (unfinished)
;;;

(defstruct (scc (:constructor %make-scc (graph revgraph posts result sizes count)))
  graph
  ;; graph of reversed edges
  revgraph
  ;; vertices in the post order
  posts
  ;; result[i] := strongly connected component of the i-th vertex
  (result nil :type (simple-array (integer 0 #.most-positive-fixnum) (*)))
  ;; sizes[i] := size of the i-th strongly connected component
  (sizes nil :type (simple-array (integer 0 #.most-positive-fixnum) (*)))
  ;; the number of strongly connected components
  (count 0 :type (integer 0 #.most-positive-fixnum)))

(declaim (inline %make-revgraph))
(defun %make-revgraph (graph)
  (let* ((n (length graph))
         (revgraph (make-array n :element-type 'list :initial-element nil)))
    (dotimes (i n)
      (dolist (dest (aref graph i))
        (push i (aref revgraph dest))))
    revgraph))

(defun make-scc (graph &optional revgraph)
  (declare (optimize (speed 3))
           (vector graph)
           ((or null vector) revgraph))
  (let* ((revgraph (or revgraph (%make-revgraph graph)))
         (n (length graph))
         (visited (make-array n :element-type 'bit :initial-element 0))
         (posts (make-array n :element-type '(integer 0 #.most-positive-fixnum)))
         (result (make-array n :element-type '(integer 0 #.most-positive-fixnum)))
         (sizes (make-array n :element-type '(integer 0 #.most-positive-fixnum)
                            :initial-element 0))
         (pointer 0)
         (ord 0) ; ordinal number for a strongly connected component
         )
    (declare ((integer 0 #.most-positive-fixnum) pointer ord))
    (assert (= n (length revgraph)))
    (labels ((dfs (v)
               (setf (aref visited v) 1)
               (dolist (neighbor (aref graph v))
                 (when (zerop (aref visited neighbor))
                   (dfs neighbor)))
               (setf (aref posts pointer) v)
               (incf pointer))
             (reversed-dfs (v ord)
               (setf (aref visited v) 1
                     (aref result v) ord)
               (incf (aref sizes ord))
               (dolist (neighbor (aref revgraph v))
                 (when (zerop (aref visited neighbor))
                   (reversed-dfs neighbor ord)))))
      (dotimes (v n)
        (when (zerop (aref visited v))
          (dfs v)))
      (fill visited 0)
      (loop for i from (- n 1) downto 0
            for v = (aref posts i)
            when (zerop (aref visited v))
            do (reversed-dfs v ord)
               (incf ord))
      (%make-scc graph revgraph posts result sizes ord))))
