;;;
;;; Maximum bipartite matching (Hopcroft-Karp, O(E sqrt(V)))
;;;

(defpackage :cp/hopcroft-karp
  (:use :cl)
  (:export #:bipartite-graph #:make-bgraph #:bipartite-graph-p #:coerce-to-bgraph
           #:bgraph-matching1 #:bgraph-matching2 #:bgraph-size1 #:bgraph-size2 #:bgraph-graph1
           #:bgraph-add-edge! #:bgraph-build-matching! #:bgraph-decompose))
(in-package :cp/hopcroft-karp)

;; NOTE: The number of elements in the graph must be less than 2^32-1 as we use
;; (UNSIGNED-BYTE 32) here for efficiency.

;; NOTE: Pay attention to the stack size!

(defconstant +graph-inf-distance+ #xffffffff)

(defstruct (bipartite-graph
            (:constructor make-bgraph
                (size1
                 size2
                 &aux
                 (graph1 (make-array size1 :element-type 'list :initial-element nil))
                 (matching1 (make-array size1 :element-type 'fixnum :initial-element -1))
                 (matching2 (make-array size2 :element-type 'fixnum :initial-element -1))))
            (:conc-name bgraph-)
            (:copier nil))
  (size1 0 :type (unsigned-byte 32))
  (size2 0 :type (unsigned-byte 32))
  (graph1 nil :type (simple-array list (*)))
  (matching1 nil :type (simple-array fixnum (*)))
  (matching2 nil :type (simple-array fixnum (*))))

(declaim (inline bgraph-add-edge!))
(defun bgraph-add-edge! (bgraph vertex1 vertex2)
  (push vertex2 (aref (bgraph-graph1 bgraph) vertex1))
  bgraph)

(defun %fill-levels (bgraph levels1 levels2 queue)
  "Does BFS and fills LEVELS."
  (declare (optimize (speed 3) (safety 0))
           ((simple-array (unsigned-byte 32) (*)) levels1 levels2 queue))
  (let ((graph1 (bgraph-graph1 bgraph))
        (matching1 (bgraph-matching1 bgraph))
        (matching2 (bgraph-matching2 bgraph))
        (q-front 0)
        (q-end 0)
        (found nil))
    (declare ((integer 0 #.most-positive-fixnum) q-front q-end))
    (labels ((enqueue (obj)
               (setf (aref queue q-end) obj)
               (incf q-end))
             (dequeue ()
               (prog1 (aref queue q-front)
                 (incf q-front))))
      (declare (inline enqueue dequeue))
      (fill levels1 +graph-inf-distance+)
      (fill levels2 +graph-inf-distance+)
      (dotimes (i (bgraph-size1 bgraph))
        (when (= -1 (aref matching1 i))
          (setf (aref levels1 i) 0)
          (enqueue i)))
      (loop until (= q-front q-end)
            for vertex = (dequeue)
            do (dolist (next (aref graph1 vertex))
                 (when (= +graph-inf-distance+ (aref levels2 next))
                   (setf (aref levels2 next) (+ 1 (aref levels1 vertex)))
                   (let ((partner (aref matching2 next)))
                     (when (= -1 partner)
                       (setq found t)
                       (return))
                     (setf (aref levels1 partner) (+ 1 (aref levels2 next)))
                     (enqueue partner))))))
    found))

(defun %find-matching (bgraph src levels1 levels2)
  "Does DFS and makes matching greedily on the residual network."
  (declare (optimize (speed 3) (safety 0))
           ((integer 0 #.most-positive-fixnum) src)
           ((simple-array (unsigned-byte 32) (*)) levels1 levels2))
  (let ((matching1 (bgraph-matching1 bgraph))
        (matching2 (bgraph-matching2 bgraph))
        (graph1 (bgraph-graph1 bgraph)))
    (labels ((dfs (v)
               (declare ((integer 0 #.most-positive-fixnum) v))
               (dolist (next (aref graph1 v))
                 (when (= (aref levels2 next) (+ 1 (aref levels1 v)))
                   (setf (aref levels2 next) +graph-inf-distance+) ; mark visited
                   (let ((partner (aref matching2 next)))
                     (when (or (= -1 partner) (dfs partner))
                       (setf (aref matching1 v) next
                             (aref matching2 next) v
                             (aref levels1 v) +graph-inf-distance+ ; mark visited
                             )
                       (return-from dfs t)))))
               (setf (aref levels1 v) +graph-inf-distance+) ; mark visited
               nil ; not matched
               ))
      (dfs src))))

(declaim (ftype (function * (values (unsigned-byte 32) &optional)) bgraph-build-matching!))
(defun bgraph-build-matching! (bgraph)
  "Makes a maximum bipartite matching and returns two vectors: correspondence
from group 1 to group 2, and correspondence from group 2 to group 1. At an
unmatched vertex, -1 is stored."
  (declare (optimize (speed 3)))
  (let* ((size1 (bgraph-size1 bgraph))
         (size2 (bgraph-size2 bgraph))
         (matching1 (bgraph-matching1 bgraph))
         (levels1 (make-array size1 :element-type '(unsigned-byte 32)))
         (levels2 (make-array size2 :element-type '(unsigned-byte 32)))
         (queue (make-array (+ size1 size2) :element-type '(unsigned-byte 32)))
         (count 0))
    (declare ((integer 0 #.most-positive-fixnum) count))
    (loop while (%fill-levels bgraph levels1 levels2 queue)
          do (dotimes (v size1)
               (when (and (= -1 (aref matching1 v))
                          (%find-matching bgraph v levels1 levels2))
                 (incf count))))
    count))

;; not tested
(defun coerce-to-bgraph (graph)
  "Converts adjacency lists representation of undirected graph to
BIPARTITE-GRAPH.

GRAPH := vector of adjacency lists"
  (declare (vector graph))
  (let* ((n (length graph))
         (visited (make-array n :element-type 'bit :initial-element 0))
         (colors (make-array n :element-type 'bit :initial-element 0))
         (nums (make-array n :element-type 'fixnum))
         (size0 0)
         (size1 0))
    (declare ((integer 0 #.most-positive-fixnum) size0 size1))
    (labels ((dfs (vertex color)
               (cond ((zerop (aref visited vertex))
                      (setf (aref visited vertex) 1
                            (aref colors vertex) color)
                      (if (zerop color)
                          (setf (aref nums vertex) size0
                                size0 (+ size0 1))
                          (setf (aref nums vertex) size1
                                size1 (+ size1 1)))
                      (if (= color 1)
                          (dolist (neighbor (aref graph vertex))
                            (dfs neighbor 0))
                          (dolist (neighbor (aref graph vertex))
                            (dfs neighbor 1))))
                     ((/= color (aref colors vertex))
                      (error "Not bipartite.")))))
      (dotimes (i n)
        (when (zerop (aref visited i))
          (dfs i 1)))
      (let ((bgraph (make-bgraph size0 size1)))
        (dotimes (i n)
          (when (zerop (aref colors i))
            (let ((i-num (aref nums i)))
              (dolist (j (aref graph i))
                (let ((j-num (aref nums j)))
                  (bgraph-add-edge! bgraph i-num j-num))))))
        bgraph))))

;; not tested
(declaim (ftype (function * (values (simple-array (integer 0 #.most-positive-fixnum) (*))
                                    (simple-array (integer 0 #.most-positive-fixnum) (*))
                                    &optional))
                bgraph-decompose!))
(defun bgraph-decompose (bgraph)
  "Decomposes a residual network to strongly connected components by Tarjan's
algorithm. BGRAPH-BUILD-MATCHING! must be called beforehand."
  (declare (optimize (speed 3)))
  (let* ((size1 (bgraph-size1 bgraph))
         (size2 (bgraph-size2 bgraph))
         (total-size (+ size1 size2 2))
         (source (+ size1 size2))
         (sink (+ size1 size2 1))
         (graph1 (bgraph-graph1 bgraph))
         (matching1 (bgraph-matching1 bgraph))
         (matching2 (bgraph-matching2 bgraph))
         (ord 0) ; in-order
         (ords (make-array total-size :element-type 'fixnum :initial-element -1))
         (lowlinks (make-array total-size :element-type 'fixnum))
         (components (make-array total-size
                                 :element-type '(integer 0 #.most-positive-fixnum)))
         (comp-index 0) ; index number of component
         (sizes (make-array total-size
                            :element-type '(integer 0 #.most-positive-fixnum)
                            :initial-element 0))
         (stack (make-array total-size :element-type '(integer 0 #.most-positive-fixnum)))
         (end 0) ; stack pointer
         (in-stack (make-array total-size :element-type 'bit :initial-element 0)))
    (declare ((integer 0 #.most-positive-fixnum) ord end comp-index source sink total-size))
    (labels ((%push (v)
               (setf (aref stack end) v
                     (aref in-stack v) 1)
               (incf end))
             (%pop ()
               (decf end)
               (let ((v (aref stack end)))
                 (setf (aref in-stack v) 0)
                 v))
             (frob (v next)
               (cond ((= -1 (aref ords next))
                      (visit next)
                      (setf (aref lowlinks v)
                            (min (aref lowlinks v) (aref lowlinks next))))
                     ((= 1 (aref in-stack next))
                      (setf (aref lowlinks v)
                            (min (aref lowlinks v) (aref ords next))))))
             (visit (v)
               (setf (aref ords v) ord
                     (aref lowlinks v) ord)
               (incf ord)
               (%push v)
               (cond ((= v source)
                      (loop for next below size1
                            when (= -1 (aref matching1 next))
                            do (frob v next)))
                     ((= v sink)
                      (loop for next below size2
                            unless (= -1 (aref matching2 next))
                            do (frob v (+ size1 next))))
                     ((and (< v size1) (= -1 (aref matching1 v)))
                      (dolist (next (aref graph1 v))
                        (declare ((integer 0 #.most-positive-fixnum) next))
                        (frob v (+ next size1))))
                     ((and (< v size1) (/= -1 (aref matching1 v)))
                      (frob v source)
                      (dolist (next (aref graph1 v))
                        (declare ((integer 0 #.most-positive-fixnum) next))
                        (unless (= next (aref matching1 v))
                          (frob v (+ next size1)))))
                     ((= -1 (aref matching2 (- v size1)))
                      (frob v sink))
                     (t
                      ;; (assert (/= -1 (aref matching2 (- v size1))))
                      (frob v (aref matching2 (- v size1)))))
               (when (= (aref lowlinks v) (aref ords v))
                 (loop for size of-type (integer 0 #.most-positive-fixnum) from 1
                       for w = (%pop)
                       do (setf (aref components w) comp-index)
                       until (= v w)
                       finally (setf (aref sizes comp-index) size)
                               (incf comp-index)))))
      (dotimes (v total-size)
        (when (= -1 (aref ords v))
          (visit v)))
      (values (subseq components 0 size1)
              (subseq components size1 (+ size1 size2))))))
