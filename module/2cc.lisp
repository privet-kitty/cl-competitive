(defpackage :cp/2cc
  (:use :cl)
  (:export #:2cc #:2cc-graph #:2cc-preorders #:2cc-components #:2cc-sizes #:2cc-count
           #:2cc-bridges #:make-2cc #:make-bridge-tree)
  (:documentation "Provides decomposition to two-edge connected components,
bridge detection, and construction of bridge-block tree."))
(in-package :cp/2cc)

(defstruct (2cc (:constructor %make-2cc (graph preorders components sizes count bridges)))
  ;; undirected graph (as a vector of adjacency lists)
  (graph nil :type vector)
  ;; preorders[i] := pre-order number of vertex i
  (preorders nil :type (simple-array fixnum (*)))
  ;; components[i] := two-edge connected component to which the i-th vertex of
  ;; GRAPH belongs
  (components nil :type (simple-array (integer 0 #.most-positive-fixnum) (*)))
  ;; sizes[k] := size of the k-th two-edge connected component
  (sizes nil :type (simple-array (integer 0 #.most-positive-fixnum) (*)))
  ;; the total number of two-edge connected components
  (count 0 :type (integer 0 #.most-positive-fixnum))
  ;; the list of bridges between two two-edge connected components (Each edge is stored
  ;; as a cons cell (vertex1 . vertex2), where vertex1 < vertex2 holds.)
  (bridges nil :type list))

(defun make-2cc (graph)
  "GRAPH := vector of adjacency lists

- This function doesn't check if GRAPH is really undirected.
- Pay attention to the stack size."
  (declare (optimize (speed 3))
           (vector graph))
  (let* ((n (length graph))
         (preorders (make-array n :element-type 'fixnum :initial-element -1))
         (preord 0)
         (components (make-array n :element-type '(integer 0 #.most-positive-fixnum)))
         (2cc-ord 0) ; is always equal to the current number of the components
         (sizes (make-array n :element-type '(integer 0 #.most-positive-fixnum)))
         stack
         (in-stack (make-array n :element-type 'bit :initial-element 0))
         roots
         bridges)
    (declare ((mod #.array-dimension-limit) preord 2cc-ord))
    (labels ((dfs (v prev)
               (declare (fixnum v prev))
               (setf (aref preorders v) preord)
               (incf preord)
               ;; push current vertex to stack
               (push v stack)
               (setf (aref in-stack v) 1)
               (push v roots)
               (let (prev-visited)
                 (dolist (neighbor (aref graph v))
                   (cond ((= -1 (aref preorders neighbor))
                          ;; not yet visited
                          (dfs neighbor v))
                         ((and (/= neighbor v)
                               (= 1 (sbit in-stack neighbor)))
                          (if (and (= neighbor prev)
                                   (not prev-visited))
                              (setq prev-visited t)
                              ;; When a backward edge to a not yet classified
                              ;; vertex exists, then these vertices comprise a
                              ;; two-edge connected component.
                              (loop while (> (aref preorders (car roots))
                                             (aref preorders neighbor))
                                    do (pop roots)))))))
               ;; add a new component
               (when (= v (the fixnum (car roots)))
                 (unless (= prev -1)
                   (push (if (<= prev v) (cons prev v) (cons v prev))
                         bridges))
                 (let ((size 0)) ; size of the component
                   (declare ((mod #.array-dimension-limit) size))
                   (loop for node = (pop stack)
                         do (setf (aref components node) 2cc-ord)
                            (setf (sbit in-stack node) 0)
                            (incf size)
                         until (= node v))
                   (setf (aref sizes 2cc-ord) size)
                   (incf 2cc-ord)
                   (pop roots)))))
      (dotimes (v n)
        (when (= -1 (aref preorders v))
          (dfs v -1)))
      (%make-2cc graph preorders components (subseq sizes 0 2cc-ord) 2cc-ord bridges))))

(declaim (ftype (function * (values (simple-array t (*)) &optional))
                make-bridge-tree))
(defun make-bridge-tree (2cc)
  "Makes a bridge-block tree (or forest, if not connected). Returns a vector of
adjacency lists."
  (declare (optimize (speed 3)))
  (let* ((graph (2cc-graph 2cc))
         (n (length graph))
         (comp-n (2cc-count 2cc))
         (sizes (2cc-sizes 2cc))
         (components (2cc-components 2cc))
         (tree (make-array comp-n :element-type t)))
    (dotimes (comp comp-n)
      (setf (aref tree comp)
            ;; Resorting to EQ is substandard, though I use it here for
            ;; efficiency.
            (make-hash-table :size (aref sizes comp) :test #'eq)))
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
