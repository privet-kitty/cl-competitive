;;;
;;; Ford-Fulkerson
;;; (better to use Dinic's algorithm. I leave it just for my reference.)
;;;

(define-condition max-flow-overflow (error)
  ((graph :initarg :graph :reader max-flow-overflow-graph))
  (:report
   (lambda (condition stream)
     (format stream "MOST-POSITIVE-FIXNUM or more units can flow on graph ~W."
             (max-flow-overflow-graph condition)))))

(defstruct (edge (:constructor %make-edge
                     (to capacity reversed &aux (default-capacity capacity))))
  (to nil :type (integer 0 #.most-positive-fixnum))
  (capacity 0 :type (integer 0 #.most-positive-fixnum))
  (default-capacity 0 :type (integer 0 #.most-positive-fixnum))
  (reversed nil :type (or null edge)))

(defmethod print-object ((edge edge) stream)
  (let ((*print-circle* t))
    (call-next-method)))

(defun add-edge (graph from-idx to-idx capacity &key bidirectional)
  "FROM-IDX, TO-IDX := index of vertex
GRAPH := vector of lists of all the edges that goes from each vertex

If BIDIRECTIONAL is true, ADD-EDGE adds the reversed edge of the same
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

(declaim (ftype (function * (values (integer 0 #.most-positive-fixnum) &optional)) %find-flow))
(defun %find-flow (graph src dest checked)
  "DFS"
  (declare (optimize (speed 3) (safety 0))
           ((integer 0 #.most-positive-fixnum) src dest)
           (simple-bit-vector checked)
           ((simple-array list (*)) graph))
  (fill checked 0)
  (labels ((dfs (vertex flow)
             (declare ((integer 0 #.most-positive-fixnum) flow))
             (setf (aref checked vertex) 1)
             (if (= vertex dest)
                 flow
                 (dolist (edge (aref graph vertex) 0)
                   (when (and (zerop (aref checked (edge-to edge)))
                              (> (edge-capacity edge) 0))
                     (let ((flow (dfs (edge-to edge) (min flow (edge-capacity edge)))))
                       (declare ((integer 0 #.most-positive-fixnum) flow))
                       (unless (zerop flow)
                         (decf (edge-capacity edge) flow)
                         (incf (edge-capacity (edge-reversed edge)) flow)
                         (return flow))))))))
    (dfs src most-positive-fixnum)))

(declaim (ftype (function * (values (mod #.most-positive-fixnum) &optional)) max-flow!))
(defun max-flow! (graph src dest)
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) src dest)
           ((simple-array list (*)) graph))
  (let ((checked (make-array (length graph) :element-type 'bit :initial-element 0))
        (result 0))
    (declare ((integer 0 #.most-positive-fixnum) result))
    (loop
      (let ((increment (%find-flow graph src dest checked)))
        (cond ((zerop increment)
               (return result))
              ((>= (+ result increment) most-positive-fixnum)
               (error 'max-flow-overflow :graph graph))
              (t
               (incf result increment)))))))

(declaim (inline reinitialize-flow-network))
(defun reinitialize-flow-network (graph)
  "Sets the current CAPACITY of every edge in GRAPH to the default
capacity. That is, this function reinitialize the graph network to the state
prior to sending flow."
  (loop for edges across graph
        do (dolist (edge edges)
             (setf (edge-capacity edge) (edge-default-capacity edge)))))
