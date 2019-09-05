;;;
;;; Biconnected components, bridges, and block-cut tree of undirected graph
;;;

;; PAY ATTENTION TO THE STACK SIZE! MAKE-BCC does DFS.

(defstruct (bcc (:constructor %make-bcc (graph preorders components sizes count bridges)))
  ;; undirected graph (as a vector of adjacency lists)
  (graph nil :type vector)
  ;; preorders[i] := the ordinal number of the i-th vertex by pre-order DFS 
  (preorders nil :type (simple-array fixnum (*)))
  ;; components[i] := the biconnected component to which the i-th vertex of
  ;; GRAPH belongs
  (components nil :type (simple-array (integer 0 #.most-positive-fixnum) (*)))
  ;; sizes[k] := size of the k-th biconnected component
  (sizes nil :type (simple-array (integer 0 #.most-positive-fixnum) (*)))
  ;; the total number of biconnected components
  (count 0 :type (integer 0 #.most-positive-fixnum))
  ;; the list of bridges between two biconnected components (Each edge is stored
  ;; as a cons cell (vertex1 . vertex2), where vertex1 < vertex2 holds.)
  (bridges nil :type list))

(defun make-bcc (graph)
  "GRAPH := vector of adjacency lists

- This function doesn't check if GRAPH is really undirected.
- A multi-edge is regarded as a single edge."
  (declare (optimize (speed 3))
           (vector graph))
  (let* ((n (length graph))
         (preorders (make-array n :element-type 'fixnum :initial-element -1))
         (preord 0)
         (components (make-array n :element-type '(integer 0 #.most-positive-fixnum)))
         (bcc-ord 0) ; is always equal to the current number of the components
         (sizes (make-array n :element-type '(integer 0 #.most-positive-fixnum)))
         stack
         (in-stack (make-array n :element-type 'bit :initial-element 0))
         roots
         bridges)
    (declare ((integer 0 #.most-positive-fixnum) preord bcc-ord))
    (labels ((dfs (v prev)
               (declare (fixnum v prev))
               (setf (aref preorders v) preord)
               (incf preord)
               ;; push current vertex to stack
               (push v stack)
               (setf (aref in-stack v) 1)
               (push v roots)
               (dolist (neighbor (aref graph v))
                 (cond ((= -1 (aref preorders neighbor))
                        ;; not yet visited
                        (dfs neighbor v))
                       ((and (/= neighbor prev)
                             (= 1 (sbit in-stack neighbor)))
                        ;; When a backward edge to a not yet classified vertex
                        ;; exists, then these vertices comprise a biconnected
                        ;; component.
                        (loop while (> (aref preorders (car roots))
                                       (aref preorders neighbor))
                              do (pop roots)))))
               ;; add a new component
               (when (= v (the fixnum (car roots)))
                 (unless (= prev -1)
                   (push (if (<= prev v) (cons prev v) (cons v prev))
                         bridges))
                 (let ((size 0)) ; size of the component
                   (declare ((integer 0 #.most-positive-fixnum) size))
                   (loop
                     (let ((node (pop stack)))
                       (setf (aref components node) bcc-ord)
                       (setf (sbit in-stack node) 0)
                       (incf size)
                       (when (= node v)
                         (return))))
                   (setf (aref sizes bcc-ord) size)
                   (incf bcc-ord)
                   (pop roots)))))
      (dotimes (v n)
        (when (= -1 (aref preorders v))
          (dfs v -1)))
      (%make-bcc graph preorders components sizes bcc-ord bridges))))

(declaim (ftype (function * (values (simple-array t (*)) &optional)) make-block-cut))
(defun make-block-cut (bcc)
  "Makes a block-cut tree (or forest, if not connected). Returns the vector of
adjacency lists."
  (declare (optimize (speed 3)))
  (let* ((graph (bcc-graph bcc))
         (n (length graph))
         (comp-n (bcc-count bcc))
         (components (bcc-components bcc))
         (tree (make-array comp-n :element-type t)))
    (dotimes (i comp-n)
      (setf (aref tree i) (make-hash-table :test #'eql)))
    (dotimes (i n)
      (let ((i-comp (aref components i)))
        (dolist (neighbor (aref graph i))
          (let ((neighbor-comp (aref components neighbor)))
            (unless (= i-comp neighbor-comp)
              (setf (gethash neighbor-comp (aref tree i-comp)) t))))))
    (dotimes (i comp-n)
      (setf (aref tree i)
            (loop for x being each hash-key of (aref tree i) collect x)))
    tree))
