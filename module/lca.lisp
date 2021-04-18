(defpackage :cp/lca
  (:use :cl)
  (:export #:lca-table #:lca-table-p
           #:make-lca-table #:lca-max-level #:lca-depths #:lca-parents
           #:two-vertices-disconnected-error #:lca-get-lca #:lca-distance
           #:lca-ascend #:lca-jump)
  (:documentation
   "Provides lowest common ancestor of tree (or forest) by binary lifting.

build: O(nlog(n))
query: O(log(n))"))
(in-package :cp/lca)

;; PAY ATTENTION TO THE STACK SIZE! THE CONSTRUCTOR DOES DFS.

(deftype lca-int () '(signed-byte 32))
(deftype lca-uint () '(and lca-int (integer 0)))

(defstruct (lca-table
            (:constructor %make-lca-table
                (size
                 &aux
                 ;; requires 1 + log_2{size-1}
                 (max-level (+ 1 (integer-length (- size 2))))
                 (depths (make-array size
                                     :element-type 'lca-int
                                     :initial-element -1))
                 (parents (make-array (list size max-level)
                                      :element-type 'lca-int))))
            (:conc-name lca-)
            (:copier nil))
  (max-level nil :type (integer 0 #.most-positive-fixnum))
  (depths nil :type (simple-array lca-int (*)))
  (parents nil :type (simple-array lca-int (* *))))

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
    (labels ((dfs (v parent depth)
               (declare (lca-int v parent))
               (setf (aref depths v) depth
                     (aref parents v 0) parent)
               (dolist (edge (aref graph v))
                 (let ((dest (funcall key edge)))
                   (declare (lca-uint dest))
                   (unless (= dest parent)
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
                lca-get-lca))
(defun lca-get-lca (lca-table vertex1 vertex2)
  "Returns the lowest common ancestor of the vertices VERTEX1 and VERTEX2."
  (declare (optimize (speed 3))
           (lca-uint vertex1 vertex2))
  (let* ((u vertex1)
         (v vertex2)
         (depths (lca-depths lca-table))
         (parents (lca-parents lca-table))
         (max-level (lca-max-level lca-table)))
    (declare (lca-int u v))
    ;; Ensures depth[u] <= depth[v]
    (when (> (aref depths u) (aref depths v))
      (rotatef u v))
    (dotimes (k max-level)
      (when (logbitp k (- (aref depths v) (aref depths u)))
        (setq v (aref parents v k))))
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

(declaim (ftype (function * (values lca-uint &optional)) lca-distance))
(defun lca-distance (lca-table vertex1 vertex2)
  "Returns the distance between two vertices."
  (declare (optimize (speed 3))
           (lca-uint vertex1 vertex2))
  (let ((depths (lca-depths lca-table))
        (lca (lca-get-lca lca-table vertex1 vertex2)))
    (+ (- (aref depths vertex1) (aref depths lca))
       (- (aref depths vertex2) (aref depths lca)))))

;; not tested
(declaim (ftype (function * (values lca-uint &optional)) lca-ascend))
(defun lca-ascend (lca-table vertex delta)
  "Returns the DELTA-th ancestor of VERTEX. (0-th ancestor is VERTEX itself.)"
  (declare (optimize (speed 3))
           (lca-uint vertex)
           (integer delta))
  (let ((depths (lca-depths lca-table))
        (parents (lca-parents lca-table))
        (max-level (lca-max-level lca-table)))
    (unless (<= 0 delta (aref depths vertex))
      (error "~D-th ancestor of vertex ~D doesn't exist, whose depth is ~D"
             delta vertex (aref depths vertex)))
    (dotimes (k max-level)
      (when (logbitp k delta)
        (setq vertex (aref parents vertex k))))
    vertex))

;; not tested
(declaim (ftype (function * (values lca-uint &optional)) lca-jump))
(defun lca-jump (lca-table start end delta)
  "Returns the vertex which is on the path between START and END and is located
at distance DELTA from START."
  (declare (lca-uint start end delta))
  (let ((lca (lca-get-lca lca-table start end))
        (depths (lca-depths lca-table)))
    (cond ((= lca end)
           (lca-ascend lca-table start delta))
          ((= lca start)
           (lca-ascend lca-table
                       end
                       (- (aref depths end)
                          (aref depths lca)
                          delta)))
          ((>= (- (aref depths start) (aref depths lca))
               delta)
           (lca-ascend lca-table start delta))
          (t
           (lca-ascend lca-table
                       end
                       (- (+ (aref depths end) (aref depths start))
                          (* 2 (aref depths lca))
                          delta))))))
