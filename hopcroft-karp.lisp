;;;
;;; Maximum bipartite matching (Hopcroft-Karp, O(E sqrt(V)))
;;;

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
                 (levels1 (make-array size1 :element-type '(unsigned-byte 32)))
                 (levels2 (make-array size2 :element-type '(unsigned-byte 32)))
                 (matching1 (make-array size1 :element-type 'fixnum :initial-element -1))
                 (matching2 (make-array size2 :element-type 'fixnum :initial-element -1))
                 (queue (make-array (+ size1 size2) :element-type '(unsigned-byte 32)))))
            (:conc-name bgraph-))
  (size1 0 :type (unsigned-byte 32))
  (size2 0 :type (unsigned-byte 32))
  (graph1 nil :type (simple-array list (*)))
  (levels1 nil :type (simple-array (unsigned-byte 32) (*)))
  (levels2 nil :type (simple-array (unsigned-byte 32) (*)))
  (matching1 nil :type (simple-array fixnum (*)))
  (matching2 nil :type (simple-array fixnum (*)))
  (queue nil :type (simple-array (unsigned-byte 32) (*))))

(declaim (inline bgraph-add-edge!))
(defun bgraph-add-edge! (bgraph vertex1 vertex2)
  (push vertex2 (aref (bgraph-graph1 bgraph) vertex1))
  bgraph)

(defun %fill-levels (bgraph)
  "Does BFS and fills LEVELS."
  (declare (optimize (speed 3) (safety 0)))
  (let ((graph1 (bgraph-graph1 bgraph))
        (matching1 (bgraph-matching1 bgraph))
        (matching2 (bgraph-matching2 bgraph))
        (levels1 (bgraph-levels1 bgraph))
        (levels2 (bgraph-levels2 bgraph))
        (queue (bgraph-queue bgraph))
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

(defun %find-matching (bgraph src)
  "Does DFS and makes matching greedily on the residual network."
  (declare (optimize (speed 3) (safety 0))
           ((integer 0 #.most-positive-fixnum) src))
  (let ((matching1 (bgraph-matching1 bgraph))
        (matching2 (bgraph-matching2 bgraph))
        (levels1 (bgraph-levels1 bgraph))
        (levels2 (bgraph-levels2 bgraph))
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

(declaim (ftype (function * (values (simple-array fixnum (*)) (simple-array fixnum (*))))
                bgraph-max-matching))
(defun bgraph-compute-max-matching (bgraph)
  "Makes a maximum bipartite matching and returns two vectors: correspondence
from group 1 to group 2, and correspondence from group 2 to group 1."
  (declare (optimize (speed 3)))
  (let* ((size1 (bgraph-size1 bgraph))
         (matching1 (bgraph-matching1 bgraph))
         (count 0))
    (declare ((integer 0 #.most-positive-fixnum) count))
    (loop while (%fill-levels bgraph)
          do (dotimes (v size1)
               (when (and (= -1 (aref matching1 v))
                          (%find-matching bgraph v))
                 (incf count))))
    count))
