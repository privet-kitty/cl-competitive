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

If BIDIRECTIONAL is true, this function simultaneously adds the reversed edge of
the same capacity."
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

;; not tested
(defun simplify-network! (graph)
  "Eliminates multi-edges. This function is destructive."
  (declare (optimize (speed 3))
           (vector graph))
  (let* ((n (length graph))
         (table (make-array n :element-type t :initial-element nil))
         (res (make-array n :element-type 'list :initial-element nil)))
    (dotimes (v (length graph))
      (dolist (edge (aref graph v))
        (let ((to (edge-to edge))
              (redge (edge-reversed edge)))
          (when (< v to)
            (let* ((cum-edge (aref table to)))
              (if cum-edge
                  (let ((cum-redge (edge-reversed cum-edge)))
                    (incf (edge-capacity cum-edge) (edge-capacity edge))
                    (incf (edge-default-capacity cum-edge) (edge-default-capacity edge))
                    (incf (edge-capacity cum-redge) (edge-capacity redge))
                    (incf (edge-default-capacity cum-redge) (edge-default-capacity redge)))
                  (progn
                    (setf (aref table to) edge)
                    (push edge (aref res v))
                    (push redge (aref res to))))))))
      (dolist (edge (aref graph v))
        (let ((to (edge-to edge)))
          (when (< v to)
            (setf (aref table to) nil)))))
    (dotimes (v n)
      (setf (aref graph v) (aref res v)))
    graph))
