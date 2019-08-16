;;;
;;; Max flow (Dinic's algorithm)
;;;

(setf *print-circle* t)

(defconstant +graph-inf-distance+ #xffffffff)

(define-condition max-flow-overflow (simple-error)
  ((graph :initarg :graph :reader max-flow-overflow-graph))
  (:report
   (lambda (condition stream)
     (let ((*print-circle* t))
       (format stream "MOST-POSITIVE-FIXNUM or more units can flow on graph ~W."
               (max-flow-overflow-graph condition))))))

(defstruct (edge (:constructor %make-edge
                     (to capacity reversed
                      &aux (default-capacity capacity))))
  (to nil :type (integer 0 #.most-positive-fixnum))
  (capacity 0 :type (integer 0 #.most-positive-fixnum))
  (default-capacity 0 :type (integer 0 #.most-positive-fixnum))
  (reversed nil :type (or null edge)))

(defmethod print-object ((edge edge) stream)
  (let ((*print-circle* t))
    (call-next-method)))

(defun push-edge (from-idx to-idx capacity graph &key bidirectional)
  "FROM-IDX, TO-IDX := index of vertex
GRAPH := vector of lists of all the edges that goes from each vertex

If BIDIRECTIONAL is true, PUSH-EDGE adds the reversed edge of the same
capacity in addition."
  (declare (optimize (speed 3))
           ((simple-array list (*)) graph))
  (let* ((dep (%make-edge to-idx capacity nil))
         (ret (%make-edge from-idx
                          (if bidirectional capacity 0)
                          dep)))
    (setf (edge-reversed dep) ret)
    (push dep (aref graph from-idx))
    (push ret (aref graph to-idx))))

(defun %fill-dist-table (src graph dist-table queue)
  "Does BFS and sets DIST-TABLE to the distance between SRC and each vertex of
GRAPH, where an edge of zero capacity is regarded as disconnected."
  (declare (optimize (speed 3) (safety 0))
           ((integer 0 #.most-positive-fixnum) src)
           ((simple-array list (*)) graph)
           ((simple-array (unsigned-byte 32) (*)) dist-table queue))
  (let* ((q-front 0)
         (q-end 0))
    (declare ((integer 0 #.most-positive-fixnum) q-front q-end))
    (labels ((enqueue (obj)
               (setf (aref queue q-end) obj)
               (incf q-end))
             (dequeue ()
               (prog1 (aref queue q-front)
                 (incf q-front))))
      (declare (inline enqueue dequeue))
      (fill dist-table +graph-inf-distance+)
      (setf (aref dist-table src) 0)
      (enqueue src)
      (loop until (= q-front q-end)
            for vertex = (dequeue)
            do (dolist (edge (aref graph vertex))
                 (let ((neighbor (edge-to edge)))
                   (when (and (> (edge-capacity edge) 0)
                              (= +graph-inf-distance+ (aref dist-table neighbor)))
                     (setf (aref dist-table neighbor)
                           (+ 1 (aref dist-table vertex)))
                     (enqueue neighbor)))))))
  dist-table)

(declaim (ftype (function * (values (integer 0 #.most-positive-fixnum) &optional)) %find-path))
(defun %find-path (src dest tmp-graph dist-table)
  "Finds an augmenting path, sends the maximal flow through it, and returns the
amount of the flow."
  (declare (optimize (speed 3) (safety 0))
           ((integer 0 #.most-positive-fixnum) src dest)
           ((simple-array list (*)) tmp-graph)
           ((simple-array (unsigned-byte 32) (*)) dist-table))
  (labels ((dfs (v flow)
             (declare ((integer 0 #.most-positive-fixnum) v flow))
             (when (= v dest)
               (return-from dfs flow))
             (loop
               (unless (aref tmp-graph v)
                 (return 0))
               (let ((edge (car (aref tmp-graph v))))
                 (when (and (> (edge-capacity edge) 0)
                            (< (aref dist-table v) (aref dist-table (edge-to edge))))
                   (let ((result (dfs (edge-to edge) (min flow (edge-capacity edge)))))
                     (declare ((integer 0 #.most-positive-fixnum) result))
                     (when (> result 0)
                       (decf (edge-capacity edge) result)
                       (incf (edge-capacity (edge-reversed edge)) result)
                       (return result)))))
               (setf (aref tmp-graph v)
                     (cdr (aref tmp-graph v))))))
    (dfs src most-positive-fixnum)))

(declaim (ftype (function * (values (mod #.most-positive-fixnum) &optional)) max-flow!))
(defun max-flow! (src dest graph)
  "Destructively sends the maximal flow from SRC to DEST and returns the amount
of the flow. This function signals MAX-FLOW-OVERFLOW error when an infinite
flow (to be precise, >= MOST-POSITIVE-FIXNUM) is possible."
  (declare #+sbcl (muffle-conditions style-warning)
           ((integer 0 #.most-positive-fixnum) src dest)
           ((simple-array list (*)) graph))
  (let* ((n (length graph))
         (dist-table (make-array n :element-type '(unsigned-byte 32)))
         (queue (make-array n :element-type '(unsigned-byte 32)))
         (tmp-graph (make-array n :element-type 'list))
         (result 0))
    (declare ((integer 0 #.most-positive-fixnum) result))
    (loop
      (%fill-dist-table src graph dist-table queue)
      (when (= (aref dist-table dest) +graph-inf-distance+) ; not (or no longer) connected
        (return result))
      (dotimes (i n)
        (setf (aref tmp-graph i) (aref graph i)))
      (loop for delta = (%find-path src dest tmp-graph dist-table)
            until (zerop delta)
            do (when (>= (+ result delta) most-positive-fixnum)
                 (error 'max-flow-overflow :graph graph))
               (incf result delta)))))

(declaim (inline reinitialize-flow-network))
(defun reinitialize-flow-network (graph)
  (loop for edges across graph
        do (dolist (edge edges)
             (setf (edge-capacity edge) (edge-default-capacity edge)))))
