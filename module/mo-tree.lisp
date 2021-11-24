(defpackage :cp/mo-tree
  (:use :cl)
  (:export #:mo-tree #:mo-tree-p #:make-mo-tree #:mo-tree-get-lca
           #:mo-tree-get-current #:mo-tree-process2)
  (:documentation "Provides Mo's algorithm for paths on a tree with weighted
vertex.

Reference:
https://ei1333.hateblo.jp/entry/2017/09/11/211011 (Japanese)"))
(in-package :cp/mo-tree)

;; not tested

(defstruct (mo-tree (:constructor %make-mo-tree)
                    (:conc-name %mo-tree-))
  ;; LCA part
  (graph nil :type (simple-array list (*)))
  (max-level nil :type (integer 0 #.most-positive-fixnum))
  (depths nil :type (simple-array fixnum (*)))
  (parents nil :type (simple-array fixnum (* *)))
  (euler-tour nil :type (simple-array (integer 0 #.most-positive-fixnum) (*)))
  ;; Mo part
  (flags nil :type simple-bit-vector)
  (lefts nil :type (simple-array (integer 0 #.most-positive-fixnum) (*)))
  (rights nil :type (simple-array (integer 0 #.most-positive-fixnum) (*)))
  (orders nil :type (simple-array (integer 0 #.most-positive-fixnum) (*)))
  (lcas nil :type (simple-array (integer 0 #.most-positive-fixnum) (*)))
  (index 0 :type (integer 0 #.most-positive-fixnum))
  (posl 0 :type (integer 0 #.most-positive-fixnum))
  (posr 0 :type (integer 0 #.most-positive-fixnum)))

(defun mo-tree-get-lca (mo vertex1 vertex2)
  "Returns the lowest common ancestor of the two vertices."
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) vertex1 vertex2))
  (let* ((u vertex1)
         (v vertex2)
         (depths (%mo-tree-depths mo))
         (parents (%mo-tree-parents mo))
         (max-level (%mo-tree-max-level mo)))
    (declare (fixnum u v))
    ;; Ensure depth[u] <= depth[v]
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
              finally (return (aref parents u 0))))))

(defun make-mo-tree (graph bucket-width lefts rights)
  "LEFTS := vector of vertices of an end of queries (inclusive)
RIGHTS := vector of vertices of another end of queries (inclusive)

BUCKET-WIDTH would be better set to 2N/sqrt(Q) where N is the width of the
universe and Q is the number of queries."
  (declare (optimize (speed 3))
           (vector lefts rights)
           ((integer 0 #.most-positive-fixnum) bucket-width)
           (inline sort))
  (let* ((graph (coerce graph '(simple-array list (*))))
         (n (length graph))
         (max-level (+ 1 (integer-length (- n 2))))
         (depths (make-array n :element-type 'fixnum :initial-element -1))
         (parents (make-array (list n max-level) :element-type 'fixnum))
         (euler-tour (make-array (- (* 2 n) 1) :element-type '(integer 0 #.most-positive-fixnum)))
         (ins (make-array n :element-type '(integer 0 #.most-positive-fixnum)))
         (end 0))
    (declare ((integer 0 #.most-positive-fixnum) n max-level end))
    ;; LCA part
    (labels ((dfs (v parent depth)
               (declare (fixnum v parent))
               (setf (aref ins v) end
                     (aref depths v) depth
                     (aref parents v 0) parent
                     (aref euler-tour end) v
                     end (+ end 1))
               (dolist (child (aref graph v))
                 (unless (= child parent)
                   (dfs child v (+ depth 1))
                   (setf (aref euler-tour end) child
                         end (+ end 1))))))
      (dfs 0 -1 0)
      (dotimes (k (- max-level 1))
        (dotimes (v n)
          (setf (aref parents v (+ k 1))
                (if (= -1 (aref parents v k))
                    -1
                    (aref parents (aref parents v k) k)))))
      ;; Mo part
      (let* ((q (length lefts))
             (res-lefts (make-array q :element-type '(integer 0 #.most-positive-fixnum)))
             (res-rights (make-array q :element-type '(integer 0 #.most-positive-fixnum)))
             (orders (make-array q :element-type '(integer 0 #.most-positive-fixnum)))
             (lcas (make-array q :element-type '(integer 0 #.most-positive-fixnum)))
             (mo (%make-mo-tree :graph graph
                                :max-level max-level
                                :depths depths
                                :parents parents
                                :euler-tour euler-tour
                                :flags (make-array n :element-type 'bit :initial-element 0)
                                :lefts res-lefts
                                :rights res-rights
                                :orders orders
                                :lcas lcas)))
        (assert (= q (length rights)))
        (dotimes (i q)
          (let ((l (aref lefts i))
                (r (aref rights i)))
            (declare ((integer 0 #.most-positive-fixnum) l r))
            (when (> l r) (rotatef l r))
            (setf (aref res-lefts i) (+ 1 (aref ins l))
                  (aref res-rights i) (+ 1 (aref ins r))
                  (aref lcas i) (mo-tree-get-lca mo l r))))
        (dotimes (i q)
          (setf (aref orders i) i))
        (setf (%mo-tree-orders mo)
              (sort orders
                    (lambda (x y)
                      (if (= (floor (aref res-lefts x) bucket-width)
                             (floor (aref res-lefts y) bucket-width))
                          ;; Even number [Odd number] block is in ascending
                          ;; [desceding] order w.r.t. the right end.
                          (if (evenp (floor (aref res-lefts x) bucket-width))
                              (< (aref res-rights x) (aref res-rights y))
                              (> (aref res-rights x) (aref res-rights y)))
                          (< (aref res-lefts x) (aref res-lefts y))))))
        mo))))

(declaim (inline mo-tree-get-current))
(defun mo-tree-get-current (mo)
  "Returns the original index of the current (not yet processed) query."
  (aref (%mo-tree-orders mo) (%mo-tree-index mo)))

(declaim (inline mo-tree-process2))
(defun mo-tree-process2 (mo extend shrink)
  "Processes the next query. EXTEND and SHRINK take an argument: the index to be
added or removed."
  (declare (function extend shrink))
  (let* ((orders (%mo-tree-orders mo))
         (index (%mo-tree-index mo))
         (ord (aref orders index))
         (lcas (%mo-tree-lcas mo))
         (left (aref (%mo-tree-lefts mo) ord))
         (right (aref (%mo-tree-rights mo) ord))
         (posl (%mo-tree-posl mo))
         (posr (%mo-tree-posr mo))
         (flags (%mo-tree-flags mo))
         (euler-tour (%mo-tree-euler-tour mo)))
    (declare ((integer 0 #.most-positive-fixnum) posl posr))
    (labels ((frob (vertex)
               (declare ((integer 0 #.most-positive-fixnum) vertex))
               (if (zerop (aref flags vertex))
                   (funcall extend vertex)
                   (funcall shrink vertex))
               (setf (aref flags vertex)
                     (logxor 1 (aref flags vertex)))))
      ;; remove LCA of the previous query
      (unless (zerop index)
        (frob (aref lcas (aref orders (- index 1)))))
      (loop while (< left posl)
            do (decf posl)
               (frob (aref euler-tour posl)))
      (loop while (< posr right)
            do (frob (aref euler-tour posr))
               (incf posr))
      (loop while (< posl left)
            do (frob (aref euler-tour posl))
               (incf posl))
      (loop while (< right posr)
            do (decf posr)
               (frob (aref euler-tour posr)))
      ;; process LCA separately
      (frob (aref lcas (aref orders index))))
    (setf (%mo-tree-posl mo) posl
          (%mo-tree-posr mo) posr)
    (incf (%mo-tree-index mo))
    ord))
