;;;
;;; Minimum cost flow (O(FEV))
;;;

(setf *print-circle* t)

(defconstant +inf-distance+ most-positive-fixnum)

(defstruct (edge (:constructor %make-edge))
  (to nil :type fixnum)
  (capacity 0 :type fixnum)
  (cost 0 :type fixnum)
  (reversed nil :type (or null edge)))

(defun push-edge (from-idx to-idx capacity cost graph)
  "FROM-IDX, TO-IDX := index of vertex
GRAPH := vector of list of all the edges that goes from the vertex"
  (declare ((simple-array list (*)) graph))
  (let* ((dep (%make-edge :to to-idx :capacity capacity :cost cost))
         (ret (%make-edge :to from-idx :capacity 0 :cost (- cost) :reversed dep)))
    (setf (edge-reversed dep) ret)
    (push dep (aref graph from-idx))
    (push ret (aref graph to-idx))))

(define-condition not-enough-capacity-error (error)
  ((graph :initarg :graph :reader not-enough-capacity-error-graph)
   (flow :initarg :flow :reader not-enough-capacity-error-flow))
  (:report
   (lambda (c s)
     (format s "Cannot send ~A units of flow on graph ~A due to not enough capacity."
             (not-enough-capacity-error-flow c)
             (not-enough-capacity-error-graph c)))))

(defun min-cost-flow! (src-idx dest-idx flow graph)
  "Returns the minimum cost to send FLOW units from SRC-IDX to DEST-IDX in
GRAPH. Destructively modifies GRAPH."
  (declare (fixnum flow)
           ((simple-array list (*)) graph))
  (let* ((size (length graph))
         (prev-vertices (make-array size :element-type 'fixnum :initial-element 0))
         (prev-edges (make-array size :element-type 'edge))
         (dist (make-array size :element-type 'fixnum))
         (res 0))
    (declare (fixnum res))
    (loop while (> flow 0)
          for updated = t
          do (fill dist +inf-distance+)
             (setf (aref dist src-idx) 0)
             (loop while updated
                   do (setf updated nil)
                      (dotimes (v size)
                        (unless (= (aref dist v) +inf-distance+)
                          (dolist (edge (aref graph v))
                            (let ((cost-sum (+ (aref dist v) (edge-cost edge))))
                              (when (and (> (edge-capacity edge) 0)
                                         (> (aref dist (edge-to edge)) cost-sum))
                                (setf (aref dist (edge-to edge)) cost-sum
                                      (aref prev-vertices (edge-to edge)) v
                                      (aref prev-edges (edge-to edge)) edge
                                      updated t)))))))
             (when (= (aref dist dest-idx) +inf-distance+)
               (error 'not-enough-capacity-error :flow flow :graph graph))
             (let ((max-flow flow))
               (declare (fixnum max-flow))
               (do ((v dest-idx (aref prev-vertices v)))
                   ((= v src-idx))
                 (setf max-flow (min max-flow (edge-capacity (aref prev-edges v)))))
               (decf flow max-flow)
               (incf res (the fixnum (* max-flow (aref dist dest-idx))))
               (do ((v dest-idx (aref prev-vertices v)))
                   ((= v src-idx))
                 (decf (edge-capacity (aref prev-edges v)) max-flow)
                 (incf (edge-capacity (edge-reversed (aref prev-edges v))) max-flow))))
    res))

;; For test
(progn
  (defparameter *graph* (make-array '(5) :element-type 'edge :initial-element nil))
  (push-edge 0 1 10 2 *graph*)
  (push-edge 0 2 2 4 *graph*)
  (push-edge 1 2 6 6 *graph*)
  (push-edge 1 3 6 2 *graph*)
  (push-edge 3 2 3 3 *graph*)
  (push-edge 3 4 8 6 *graph*)
  (push-edge 2 4 5 2 *graph*)
  (assert (= 80 (min-cost-flow! 0 4 9 *graph*))))
