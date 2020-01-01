;;;
;;; Lowest common ancestor of tree (or forest) by binary lifting
;;; build: O(nlog(n))
;;; query: O(log(n))
;;;

;; PAY ATTENTION TO THE STACK SIZE! THE CONSTRUCTOR DOES DFS.

(deftype lca-vertex-number () '(signed-byte 32))

(defstruct (lca-table
            (:constructor %make-lca-table
                (size
                 &aux
                 ;; requires 1 + log_2{size-1}
                 (max-level (+ 1 (integer-length (- size 2))))
                 (depths (make-array size
                                     :element-type 'lca-vertex-number
                                     :initial-element -1))
                 (parents (make-array (list size max-level)
                                      :element-type 'lca-vertex-number))))
            (:conc-name lca-))
  (max-level nil :type (integer 0 #.most-positive-fixnum))
  (depths nil :type (simple-array lca-vertex-number (*)))
  (parents nil :type (simple-array lca-vertex-number (* *))))

(defun make-lca-table (graph &key root (key #'identity))
  "GRAPH := vector of adjacency lists
ROOT := null | non-negative fixnum

If ROOT is null, this function traverses each connected component of GRAPH from
an arbitrarily picked vertex. Otherwise this function traverses GRAPH only from
ROOT; GRAPH must be tree in the latter case."
  (declare (optimize (speed 3))
           (vector graph)
           (function key)
           ((or null (integer 0 #.most-positive-fixnum)) root))
  (let* ((size (length graph))
         (lca-table (%make-lca-table size))
         (depths (lca-depths lca-table))
         (parents (lca-parents lca-table))
         (max-level (lca-max-level lca-table)))
    (labels ((dfs (v prev-v depth)
               (declare (lca-vertex-number v prev-v))
               (setf (aref depths v) depth)
               (setf (aref parents v 0) prev-v)
               (dolist (node (aref graph v))
                 (let ((dest (funcall key node)))
                   (declare (lca-vertex-number dest))
                   (unless (= dest prev-v)
                     (dfs dest v (+ 1 depth)))))))
      (if root
          (dfs root -1 0)
          (dotimes (v size)
            (when (= (aref depths v) -1)
              (dfs v -1 0))))
      (dotimes (k (- max-level 1))
        (dotimes (v size)
          (if (= -1 (aref parents v k))
              (setf (aref parents v (+ k 1)) -1)
              (setf (aref parents v (+ k 1))
                    (aref parents (aref parents v k) k)))))
      lca-table)))

(define-condition two-vertices-disconnected-error (error)
  ((lca-table :initarg :lca-table :accessor two-vertices-disconnected-error-lca-table)
   (vertex1 :initarg :vertex1 :accessor two-vertices-disconnected-error-vertex1)
   (vertex2 :initarg :vertex2 :accessor two-vertices-disconnected-error-vertex2))
  (:report
   (lambda (c s)
     (format s "~W and ~W are disconnected on lca-table ~W"
             (two-vertices-disconnected-error-vertex1 c)
             (two-vertices-disconnected-error-vertex2 c)
             (two-vertices-disconnected-error-lca-table c)))))

(declaim (ftype (function * (values (integer 0 #.most-positive-fixnum) &optional))
                get-lca))
(defun get-lca (vertex1 vertex2 lca-table)
  "Returns the lowest common ancestor of the vertices VERTEX1 and VERTEX2."
  (declare (optimize (speed 3))
           ((and lca-vertex-number (integer 0)) vertex1 vertex2))
  (let* ((u vertex1)
         (v vertex2)
         (depths (lca-depths lca-table))
         (parents (lca-parents lca-table))
         (max-level (lca-max-level lca-table)))
    (declare (lca-vertex-number u v))
    ;; Ensures depth[u] <= depth[v]
    (when (> (aref depths u) (aref depths v))
      (rotatef u v))
    (dotimes (k max-level)
      (when (logbitp k (- (aref depths v) (aref depths u)))
        (setf v (aref parents v k))))
    (if (= u v)
        u
        (loop for k from (- max-level 1) downto 0
              unless (= (aref parents u k) (aref parents v k))
              do (setq u (aref parents u k)
                       v (aref parents v k))
              finally (if (= (aref parents u 0) -1)
                          (error 'two-vertices-disconnected-error
                                 :lca-table lca-table
                                 :vertex1 vertex1
                                 :vertex2 vertex2)
                          (return (aref parents u 0)))))))

(declaim (inline distance-on-tree))
(defun distance-on-tree (u v lca-table)
  "Returns the distance between two vertices U and V."
  (declare (optimize (speed 3)))
  (let ((depths (lca-depths lca-table))
        (lca (get-lca u v lca-table)))
    (+ (- (aref depths u) (aref depths lca))
       (- (aref depths v) (aref depths lca)))))
