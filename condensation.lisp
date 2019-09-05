;;;
;;; Strongly connected components of directed graph
;;;

(defstruct (scc (:constructor %make-scc (graph revgraph posts components sizes count)))
  (graph nil :type vector)
  ;; reversed graph
  (revgraph nil :type vector)
  ;; vertices by post-order DFS
  posts
  ;; components[i] := strongly connected component of the i-th vertex
  (components nil :type (simple-array (integer 0 #.most-positive-fixnum) (*)))
  ;; sizes[k] := size of the k-th strongly connected component
  (sizes nil :type (simple-array (integer 0 #.most-positive-fixnum) (*)))
  ;; the total number of strongly connected components
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
  "GRAPH := vector of adjacency lists
REVGRAPH := NIL | reversed graph of GRAPH"
  (declare (optimize (speed 3))
           (vector graph)
           ((or null vector) revgraph))
  (let* ((revgraph (or revgraph (%make-revgraph graph)))
         (n (length graph))
         (visited (make-array n :element-type 'bit :initial-element 0))
         (posts (make-array n :element-type '(integer 0 #.most-positive-fixnum)))
         (components (make-array n :element-type '(integer 0 #.most-positive-fixnum)))
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
                     (aref components v) ord)
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
      (%make-scc graph revgraph posts components sizes ord))))

(declaim (ftype (function * (values (simple-array t (*)) &optional)) make-condensed-graph))
(defun make-condensed-graph (scc)
  "Does graph condensation.

This function is non-destructive. The resultant graph doesn't contain self-loops
even if the given graph does."
  (declare (optimize (speed 3)))
  (let* ((graph (scc-graph scc))
         (n (length graph))
         (comp-n (scc-count scc))
         (components (scc-components scc))
         (condensed (make-array comp-n :element-type t)))
    (dotimes (i comp-n)
      (setf (aref condensed i) (make-hash-table :test #'eql)))
    (dotimes (i n)
      (let ((i-comp (aref components i)))
        (dolist (neighbor (aref graph i))
          (let ((neighbor-comp (aref components neighbor)))
            (unless (= i-comp neighbor-comp)
              (setf (gethash neighbor-comp (aref condensed i-comp)) t))))))
    (dotimes (i comp-n)
      (setf (aref condensed i)
            (loop for x being each hash-key of (aref condensed i) collect x)))
    condensed))
