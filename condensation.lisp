;;;
;;; Strongly connected components of directed graph, 2-SAT
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

;;;
;;; 2-SAT
;;;

(defstruct (2sat (:constructor make-2sat
                     (size
                      &aux
                      (graph (make-array (* 2 size) :element-type 'list :initial-element nil)))))
  (size 0 :type (integer 0 #.most-positive-fixnum))
  (graph nil :type (simple-array list (*)))
  (scc nil :type (or null scc)))

(declaim (inline negate))
(defun negate (p)
  (- -1 p))

(declaim (inline add-implication))
(defun add-implication (2sat p q)
  "Adds `P => Q' to 2SAT."
  (declare (fixnum p q))
  (let ((size (2sat-size 2sat))
        (graph (2sat-graph 2sat)))
    (when (< p 0)
      (setq p (+ size (- -1 p))))
    (when (< q 0)
      (setq q (+ size (- -1 q))))
    (push q (aref graph p))
    2sat))

(declaim (inline add-disjunction))
(defun add-disjunction (2sat p q)
  "Adds `P or Q' to 2SAT."
  (declare (fixnum p q))
  (add-implication 2sat (negate p) q)
  (add-implication 2sat (negate q) p)
  2sat)

(declaim (inline 2sat-solve))
(defun 2sat-solve (2sat)
  "Solves 2-SAT and returns a simple bit vector expressing the boolean of each
variable if it is feasible, otherwise returns NIL."
  (let* ((size (2sat-size 2sat))
         (graph (2sat-graph 2sat))
         (scc (make-scc graph))
         (components (scc-components scc))
         (result (make-array size :element-type 'bit :initial-element 0)))
    (setf (2sat-scc 2sat) scc)
    (loop for v below size
          for v-comp = (aref components v)
          for neg-comp = (aref components (+ v size))
          do (cond ((> v-comp neg-comp)
                    (setf (sbit result v) 1))
                   ((= v-comp neg-comp)
                    (return-from 2sat-solve nil))))
    result))
