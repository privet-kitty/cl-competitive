(defpackage :cp/max-flow
  (:use :cl)
  (:export #:edge #:add-edge #:reinitialize-flow-network #:max-flow-overflow
           #:edge-to #:edge-capacity #:edge-default-capacity #:edge-reversed))
(in-package :cp/max-flow)

(define-condition max-flow-overflow (error)
  ((graph :initarg :graph :reader max-flow-overflow-graph))
  (:report
   (lambda (condition stream)
     (format stream "MOST-POSITIVE-FIXNUM or more units can flow on graph ~W."
             (max-flow-overflow-graph condition)))))

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

(declaim (inline reinitialize-flow-network))
(defun reinitialize-flow-network (graph)
  "Sets the current CAPACITY of every edge in GRAPH to the default
capacity. That is, this function reinitialize the graph network to the state
prior to sending flow."
  (loop for edges across graph
        do (dolist (edge edges)
             (setf (edge-capacity edge) (edge-default-capacity edge)))))
