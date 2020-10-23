;;;
;;; LCA and path queries on a tree with weighted edge (binary lifting)
;;; build: O(nlog(n))
;;; query: O(log(n))
;;;

(defpackage :cp/lca-edge
  (:use :cl)
  (:export #:lca-table #:lca-table-p
           #:make-lca-table #:lca-max-level #:lca-depths #:lca-parents
           #:two-vertices-disconnected-error #:lca-get-lca #:lca-distance
           #:lca-fold))
(in-package :cp/lca-edge)

;; TODO: abstraction
;; PAY ATTENTION TO THE STACK SIZE! THE CONSTRUCTOR DOES DFS.

(deftype lca-vertex-number () '(signed-byte 32))

(sb-int:defconstant-eqx +identity+ "" #'equal)

(declaim (inline op))
(defun op (x y) (concatenate 'simple-base-string x y))

(deftype lca-element-type () 'string)

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
                                      :element-type 'lca-vertex-number))
                 (table-forth (make-array (list size max-level)
                                          :element-type 'lca-element-type
                                          :initial-element +identity+))
                 (table-back (make-array (list size max-level)
                                         :element-type 'lca-element-type
                                         :initial-element +identity+))))
            (:conc-name lca-)
            (:copier nil))
  (max-level nil :type (integer 0 #.most-positive-fixnum))
  (depths nil :type (simple-array lca-vertex-number (*)))
  (parents nil :type (simple-array lca-vertex-number (* *)))
  (table-forth nil :type (simple-array lca-element-type (* *)))
  (table-back nil :type (simple-array lca-element-type (* *))))

(defun make-lca-table (graph &key root (vertex-key #'identity) (weight-key #'identity))
  "GRAPH := vector of adjacency lists
ROOT := null | non-negative fixnum

If ROOT is null, this function traverses each connected component of GRAPH from
an arbitrarily picked vertex. Otherwise this function traverses GRAPH only from
ROOT; GRAPH must be tree in the latter case."
  (declare (optimize (speed 3))
           (vector graph)
           (function vertex-key weight-key)
           ((or null (integer 0 #.most-positive-fixnum)) root))
  (let* ((size (length graph))
         (lca-table (%make-lca-table size))
         (depths (lca-depths lca-table))
         (parents (lca-parents lca-table))
         (max-level (lca-max-level lca-table))
         (table-forth (lca-table-forth lca-table))
         (table-back (lca-table-back lca-table)))
    (labels ((dfs (v parent depth)
               (declare (lca-vertex-number v parent))
               (setf (aref depths v) depth
                     (aref parents v 0) parent)
               (dolist (edge (aref graph v))
                 (let ((child (funcall vertex-key edge)))
                   (declare (lca-vertex-number child))
                   (unless (= child parent)
                     (let ((value (funcall weight-key edge)))
                       (setf (aref table-forth child 0) value
                             (aref table-back child 0) value))
                     (dfs child v (+ 1 depth)))))))
      (if root
          (dfs root -1 0)
          (dotimes (v size)
            (when (= (aref depths v) -1)
              (dfs v -1 0))))
      (dotimes (k (- max-level 1))
        (dotimes (v size)
          (if (= -1 (aref parents v k))
              (setf (aref parents v (+ k 1)) -1)
              (let ((parent (aref parents v k)))
                (setf (aref parents v (+ k 1)) (aref parents parent k)
                      (aref table-forth v (+ k 1)) (op (aref table-forth v k)
                                                       (aref table-forth parent k))
                      (aref table-back v (+ k 1)) (op (aref table-back parent k)
                                                      (aref table-back v k)))))))
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

(declaim (ftype (function * (values (mod #.array-total-size-limit) &optional)) lca-get-lca))
(defun lca-get-lca (lca-table vertex1 vertex2)
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

(declaim (ftype (function * (values lca-element-type &optional)) lca-fold))
(defun lca-fold (lca-table vertex1 vertex2)
  "Folds edge weight along the path from VERTEX1 to VERTEX2."
  (declare (optimize (speed 3))
           ((and lca-vertex-number (integer 0)) vertex1 vertex2))
  (let* ((u vertex1)
         (v vertex2)
         (depths (lca-depths lca-table))
         (parents (lca-parents lca-table))
         (table-forth (lca-table-forth lca-table))
         (table-back (lca-table-back lca-table))
         (max-level (lca-max-level lca-table))
         (res1 +identity+)
         (res2 +identity+))
    (declare (lca-vertex-number u v)
             (lca-element-type res1 res2))
    (if (<= (aref depths u) (aref depths v))
        (dotimes (k max-level)
          (when (logbitp k (- (aref depths v) (aref depths u)))
            (setq res2 (op (aref table-back v k) res2)
                  v (aref parents v k))))
        (dotimes (k max-level)
          (when (logbitp k (- (aref depths u) (aref depths v)))
            (setq res1 (op res1 (aref table-forth u k))
                  u (aref parents u k)))))
    (unless (= u v)
      (loop for k from (- max-level 1) downto 0
            unless (= (aref parents u k) (aref parents v k))
            do (setq res1 (op res1 (aref table-forth u k))
                     res2 (op (aref table-back v k) res2)
                     u (aref parents u k)
                     v (aref parents v k))
            finally (when (= (aref parents u 0) -1)
                      (error 'two-vertices-disconnected-error
                             :lca-table lca-table
                             :vertex1 vertex1
                             :vertex2 vertex2))
                    (setq res1 (op res1 (aref table-forth u 0))
                          res2 (op (aref table-back v 0) res2))))
    (op res1 res2)))

(declaim (inline lca-distance))
(defun lca-distance (lca-table u v)
  "Returns the distance between two vertices U and V."
  (declare (optimize (speed 3)))
  (let ((depths (lca-depths lca-table))
        (lca (lca-get-lca lca-table u v)))
    (+ (- (aref depths u) (aref depths lca))
       (- (aref depths v) (aref depths lca)))))
