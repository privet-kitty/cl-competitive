;;;
;;; Maximum bipartite matching (Hopcroft-Karp, O(E sqrt(V)))
;;;

;; NOTE: The number of elements in the graph must be less than 2^32-1 as we use
;; (UNSIGNED-BYTE 32) here for efficiency.

;; NOTE: Pay attention to the stack size!

(defconstant +graph-inf-distance+ #xffffffff)

(defun %fill-levels (graph boundary levels matching queue)
  "Does BFS and fills LEVELS."
  (declare (optimize (speed 3) (safety 0))
           ((simple-array list (*)) graph)
           ((simple-array (unsigned-byte 32) (*)) levels queue)
           ((simple-array fixnum (*)) matching)
           ((integer 0 #.most-positive-fixnum) boundary))
  (let ((q-front 0)
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
      (fill levels +graph-inf-distance+)
      (dotimes (i boundary)
        (when (= -1 (aref matching i))
          (setf (aref levels i) 0)
          (enqueue i)))
      (loop until (= q-front q-end)
            for vertex = (dequeue)
            do (dolist (next (aref graph vertex))
                 (when (= +graph-inf-distance+ (aref levels next))
                   (setf (aref levels next) (+ 1 (aref levels vertex)))
                   (let ((partner (aref matching next)))
                     (if (= -1 partner)
                         (progn (setq found t)
                                (return))
                         (when (= +graph-inf-distance+ (aref levels partner))
                           (setf (aref levels partner) (+ 1 (aref levels next)))
                           (enqueue partner))))))))
    found))

(defun %find-matching (src graph levels matching)
  "Does DFS and makes matching greedily on the residual network."
  (declare (optimize (speed 3) (safety 0))
           ((simple-array list (*)) graph)
           ((simple-array (unsigned-byte 32) (*)) levels)
           ((simple-array fixnum (*)) matching)
           ((integer 0 #.most-positive-fixnum) src))
  (labels ((dfs (v)
             (declare ((integer 0 #.most-positive-fixnum) v))
             (dolist (next (aref graph v))
               (when (= (aref levels next) (+ 1 (aref levels v)))
                 (setf (aref levels next) +graph-inf-distance+) ; mark visited
                 (let ((partner (aref matching next)))
                   (when (or (= -1 partner) (dfs partner))
                     (setf (aref matching v) next
                           (aref matching next) v
                           (aref levels v) +graph-inf-distance+ ; mark visited
                           )
                     (return-from dfs t)))))
             (setf (aref levels v) +graph-inf-distance+) ; mark visited
             nil ; not matched
             ))
    (dfs src)))

(declaim (ftype (function * (values (or null (simple-array fixnum (*))) (integer 0 #.most-positive-fixnum) &optional)) find-matching))
(defun find-matching (graph boundary)
  "Takes an adjacency list and returns the maximal bipartite matching. GRAPH[0],
GRAPH[1] ..., GRAPH[BOUNDARY-1] must belong to one group and GRAPH[BOUNDARY],
GRAPH[BOUNDARY+1], ... must belong to the other group.

This function returns two values: the vector of matching and the number of
pairs.  Note that this function doesn't check if GRAPH is truly bipartite."
  (declare (optimize (speed 3))
           ((simple-array list (*)) graph)
           ((integer 0 #.most-positive-fixnum) boundary))
  (let* ((n (length graph))
         (queue (make-array n :element-type '(unsigned-byte 32)))
         (levels (make-array n :element-type '(unsigned-byte 32)))
         (matching (make-array n :element-type 'fixnum :initial-element -1))
         (count 0))
    (declare ((integer 0 #.most-positive-fixnum) count))
    (assert (<= boundary n))
    (loop while (%fill-levels graph boundary levels matching queue)
          do (dotimes (v boundary)
               (when (and (= -1 (aref matching v))
                          (%find-matching v graph levels matching))
                 (incf count))))
    (values matching count)))
