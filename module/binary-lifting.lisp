(defpackage :cp/binary-lifting
  (:use :cl)
  (:export #:two-vertices-disconnected-error #:define-binary-lifting)
  (:documentation
   "Provides LCA and path queries on a static tree or forest with
weighted edge or vertex.

build: O(nlog(n))
query: O(log(n))"))
(in-package :cp/binary-lifting)

;; PAY ATTENTION TO THE STACK SIZE! THE CONSTRUCTOR DOES DFS.

(deftype vertex-integer () '(signed-byte 32))

(define-condition two-vertices-disconnected-error (error)
  ((struct :initarg :struct :accessor two-vertices-disconnected-error-struct)
   (vertex1 :initarg :vertex1 :accessor two-vertices-disconnected-error-vertex1)
   (vertex2 :initarg :vertex2 :accessor two-vertices-disconnected-error-vertex2))
  (:report
   (lambda (c s)
     (format s "~W and ~W are disconnected on structure ~W"
             (two-vertices-disconnected-error-vertex1 c)
             (two-vertices-disconnected-error-vertex2 c)
             (two-vertices-disconnected-error-struct c)))))

;; TODO: binary search (four kinds?)
(defmacro define-binary-lifting
    (name type &key op identity element-type)
  "Defines a structure to deal with LCA and path query on a tree or forest with
weighted edge or vertex.

TYPE := :VERTEX | :EDGE
OP := operator comprising a monoid
IDENTITY := identity element of the monoid"
  (assert (member type '(:vertex :edge)))
  (let ((%constructor (intern (format nil "%MAKE-~A" name)))
        (constructor (intern (format nil "MAKE-~A" name)))
        (conc-name (intern (format nil "%~A-" name)))
        (%lca-depths (intern (format nil "%~A-DEPTHS" name)))
        (%lca-parents (intern (format nil "%~A-PARENTS" name)))
        (%lca-table-out (intern (format nil "%~A-TABLE-OUT" name)))
        (%lca-table-in (intern (format nil "%~A-TABLE-IN" name)))
        (%lca-max-level (intern (format nil "%~A-MAX-LEVEL" name)))
        (lca-get-lca (intern (format nil "~A-GET-LCA" name)))
        (lca-fold (intern (format nil "~A-FOLD" name)))
        (lca-distance (intern (format nil "~A-DISTANCE" name))))
    `(progn
       (defstruct (,name
                   (:constructor ,%constructor
                       (size
                        &aux
                        ;; requires 1 + log_2{size-1}
                        (max-level (+ 1 (integer-length (- size 2))))
                        (depths (make-array size
                                            :element-type 'vertex-integer
                                            :initial-element -1))
                        (parents (make-array (list size max-level)
                                             :element-type 'vertex-integer))
                        (table-out (make-array (list size max-level)
                                               :element-type ',element-type
                                               :initial-element ,identity))
                        (table-in (make-array (list size max-level)
                                              :element-type ',element-type
                                              :initial-element ,identity))))
                   (:conc-name ,conc-name)
                   (:copier nil)
                   (:predicate nil))
         (max-level nil :type (mod #.array-dimension-limit))
         (depths nil :type (simple-array vertex-integer (*)))
         (parents nil :type (simple-array vertex-integer (* *)))
         (table-out nil :type (simple-array ,element-type (* *)))
         (table-in nil :type (simple-array ,element-type (* *))))

       (defun ,constructor (graph weights &key root (vertex-key #'identity))
         "GRAPH := vector of adjacency lists
ROOT := null | non-negative fixnum
WEIGHTS[v] := weight assigned to vertex v if type of query is :VERTEX. If type
of query is :edge, the weight is considered to be assigned to the edge between v
and its parent.

If ROOT is null, this function traverses each connected component from the
vertex with the lowest index. Otherwise this function traverses GRAPH only from
ROOT; GRAPH must be tree in the latter case."
         (declare (optimize (speed 3))
                  (vector graph)
                  (function vertex-key)
                  ((or null (mod #.array-dimension-limit)) root))
         (let* ((size (length graph))
                (,name (,%constructor size))
                (depths (,%lca-depths ,name))
                (parents (,%lca-parents ,name))
                (max-level (,%lca-max-level ,name))
                (table-out (,%lca-table-out ,name))
                (table-in (,%lca-table-in ,name)))
           (labels ((dfs (v parent depth)
                      (declare (vertex-integer v parent))
                      (setf (aref depths v) depth
                            (aref parents v 0) parent)
                      (dolist (edge (aref graph v))
                        (let ((child (funcall vertex-key edge)))
                          (declare (vertex-integer child))
                          (unless (= child parent)
                            (dfs child v (+ 1 depth)))))))
             (dotimes (v size)
               (let ((weight (aref weights v)))
                 (setf (aref table-in v 0) weight
                       (aref table-out v 0) weight)))
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
                       (setf (aref parents v (+ k 1))
                             (aref parents parent k)
                             (aref table-out v (+ k 1))
                             (funcall ,op
                                      (aref table-out v k)
                                      (aref table-out parent k))
                             (aref table-in v (+ k 1))
                             (funcall ,op
                                      (aref table-in parent k)
                                      (aref table-in v k)))))))
             ,name)))

       (declaim (ftype (function * (values (mod #.array-dimension-limit) &optional))
                       ,lca-get-lca))
       (defun ,lca-get-lca (,name vertex1 vertex2)
         "Returns the lowest common ancestor of the vertices VERTEX1 and VERTEX2."
         (declare (optimize (speed 3))
                  ((and vertex-integer (integer 0)) vertex1 vertex2))
         (let* ((u vertex1)
                (v vertex2)
                (depths (,%lca-depths ,name))
                (parents (,%lca-parents ,name))
                (max-level (,%lca-max-level ,name)))
           (declare (vertex-integer u v))
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
                                        :struct ,name
                                        :vertex1 vertex1
                                        :vertex2 vertex2)
                                 (return (aref parents u 0)))))))

       (declaim (ftype (function * (values ,element-type &optional)) ,lca-fold))
       (defun ,lca-fold (,name vertex1 vertex2)
         "Folds edge or vertex weight along the path from VERTEX1 to VERTEX2."
         (declare (optimize (speed 3))
                  ((and vertex-integer (integer 0)) vertex1 vertex2))
         (let* ((u vertex1)
                (v vertex2)
                (depths (,%lca-depths ,name))
                (parents (,%lca-parents ,name))
                (table-out (,%lca-table-out ,name))
                (table-in (,%lca-table-in ,name))
                (max-level (,%lca-max-level ,name))
                (res1 ,identity)
                (res2 ,identity))
           (declare (vertex-integer u v)
                    (,element-type res1 res2))
           (if (<= (aref depths u) (aref depths v))
               (dotimes (k max-level)
                 (when (logbitp k (- (aref depths v) (aref depths u)))
                   (setq res2 (funcall ,op (aref table-in v k) res2)
                         v (aref parents v k))))
               (dotimes (k max-level)
                 (when (logbitp k (- (aref depths u) (aref depths v)))
                   (setq res1 (funcall ,op res1 (aref table-out u k))
                         u (aref parents u k)))))
           (unless (= u v)
             (loop for k from (- max-level 1) downto 0
                   unless (= (aref parents u k) (aref parents v k))
                   do (setq res1 (funcall ,op res1 (aref table-out u k))
                            res2 (funcall ,op (aref table-in v k) res2)
                            u (aref parents u k)
                            v (aref parents v k))
                   finally (when (= (aref parents u 0) -1)
                             (error 'two-vertices-disconnected-error
                                    :struct ,name
                                    :vertex1 vertex1
                                    :vertex2 vertex2))
                           (setq res1 (funcall ,op res1 (aref table-out u 0))
                                 res2 (funcall ,op (aref table-in v 0) res2)
                                 u (aref parents u 0)
                                 v (aref parents v 0))))
           ,@(when (eql type :vertex)
               `((setq res1 (funcall ,op res1 (aref table-out u 0)))))
           (funcall ,op res1 res2)))

       (declaim (inline ,lca-distance)
                (ftype (function * (values (mod #.array-dimension-limit) &optional))
                       ,lca-distance))
       (defun ,lca-distance (,name vertex1 vertex2)
         "Returns the distance between two vertices."
         (let* ((depths (,%lca-depths ,name))
                (lca (,lca-get-lca ,name vertex1 vertex2)))
           (+ (the (mod #.array-dimension-limit)
                   (- (aref depths vertex1) (aref depths lca)))
              (the (mod #.array-dimension-limit)
                   (- (aref depths vertex2) (aref depths lca)))))))))

#+(or)
(define-binary-lifting lca :edge
  :op #'max
  :identity most-negative-fixnum
  :element-type fixnum)
