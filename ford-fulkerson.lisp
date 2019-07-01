;;;
;;; Ford-Fulkerson
;;;

(setf *print-circle* t)

(defstruct (edge (:constructor %make-edge))
  (to nil :type (integer 0 #.most-positive-fixnum))
  (capacity 0 :type (integer 0 #.most-positive-fixnum))
  (reversed nil :type (or null edge)))

(defun push-edge (from-idx to-idx capacity graph &key bidirectional)
  "FROM-IDX, TO-IDX := index of vertex
GRAPH := vector of lists of all the edges that goes from each vertex

If BIDIRECTIONAL is true, PUSH-EDGE adds the reversed edge of the same
capacity in addition."
  (declare (optimize (speed 3))
           ((simple-array list (*)) graph))
  (let* ((dep (%make-edge :to to-idx :capacity capacity))
         (ret (%make-edge :to from-idx
                          :capacity (if bidirectional capacity 0)
                          :reversed dep)))
    (setf (edge-reversed dep) ret)
    (push dep (aref graph from-idx))
    (push ret (aref graph to-idx))))

(declaim (ftype (function * (values (integer 0 #.most-positive-fixnum) &optional)) %find-flow))
(defun %find-flow (src-idx dest-idx graph checked)
  "DFS"
  (declare (optimize (speed 3) (safety 0))
           ((integer 0 #.most-positive-fixnum) src-idx dest-idx)
           (simple-bit-vector checked)
           ((simple-array list (*)) graph))
  (fill checked 0)
  (labels ((dfs (vertex flow)
             (declare ((integer 0 #.most-positive-fixnum) flow))
             (setf (aref checked vertex) 1)
             (if (= vertex dest-idx)
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
    (dfs src-idx most-positive-fixnum)))

(define-condition max-flow-overflow (simple-error)
  ((graph :initarg :graph :reader max-flow-overflow-graph))
  (:report
   (lambda (condition stream)
     (let ((*print-circle* t))
       (format stream "MOST-POSITIVE-FIXNUM or more units can flow on graph ~W."
               (max-flow-overflow-graph condition))))))

(defun max-flow! (src-idx dest-idx graph)
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) src-idx dest-idx)
           ((simple-array list (*)) graph))
  (let ((checked (make-array (length graph) :element-type 'bit :initial-element 0))
        (result 0))
    (declare ((integer 0 #.most-positive-fixnum) result))
    (loop
      (let ((increment (%find-flow src-idx dest-idx graph checked)))
        (cond ((zerop increment)
               (return result))
              ((>= (+ result increment) most-positive-fixnum)
               (error 'max-flow-overflow :graph graph))
              (t
               (incf result increment)))))))
