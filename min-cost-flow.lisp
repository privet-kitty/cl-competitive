;;;
;;; Minimum cost flow (SSP)
;;;

;; COST-TYPE and +INF-COST+ may be changed. (A supposed use case is to adopt
;; bignum).
(deftype cost-type () 'fixnum)
(defconstant +inf-cost+ most-positive-fixnum)
(assert (and (typep +inf-cost+ 'cost-type)
             (subtypep 'cost-type 'integer)))

(defstruct (edge (:constructor %make-edge))
  (to nil :type (integer 0 #.most-positive-fixnum))
  (capacity 0 :type (integer 0 #.most-positive-fixnum))
  (cost 0 :type cost-type)
  (reversed nil :type (or null edge)))

(defmethod print-object ((edge edge) stream)
  (let ((*print-circle* t))
    (call-next-method)))

(defun add-edge! (from-idx to-idx capacity cost graph)
  "FROM-IDX, TO-IDX := index of vertex
GRAPH := vector of list of all the edges that goes from the vertex"
  (declare ((simple-array list (*)) graph)
           (cost-type cost))
  (let* ((dep (%make-edge :to to-idx :capacity capacity :cost cost))
         (ret (%make-edge :to from-idx :capacity 0 :cost (- cost) :reversed dep)))
    (setf (edge-reversed dep) ret)
    (push dep (aref graph from-idx))
    (push ret (aref graph to-idx))))

;; binary heap for Dijkstra's algorithm
(defstruct (fheap (:constructor make-fheap
                          (size
                           &aux (costs (make-array (1+ size) :element-type 'cost-type))
                                (vertices (make-array (1+ size) :element-type 'fixnum)))))
  (costs nil :type (simple-array cost-type (*)))
  (vertices nil :type (simple-array fixnum (*)))
  (position 1 :type (integer 1 #.most-positive-fixnum)))

(defun fheap-push (cost vertex fheap)
  (declare (optimize (speed 3)))
  (symbol-macrolet ((position (fheap-position fheap)))
    (when (>= position (length (fheap-costs fheap)))
      (setf (fheap-costs fheap)
            (adjust-array (fheap-costs fheap) (* position 2))
            (fheap-vertices fheap)
            (adjust-array (fheap-vertices fheap) (* position 2))))
    (let ((costs (fheap-costs fheap))
          (vertices (fheap-vertices fheap)))
      (labels ((update (pos)
                 (declare (optimize (safety 0)))
                 (unless (= pos 1)
                   (let ((parent-pos (ash pos -1)))
                     (when (< (aref costs pos) (aref costs parent-pos))
                       (rotatef (aref costs pos) (aref costs parent-pos))
                       (rotatef (aref vertices pos) (aref vertices parent-pos))
                       (update parent-pos))))))
        (setf (aref costs position) cost
              (aref vertices position) vertex)
        (update position)
        (incf position)
        fheap))))

(defun fheap-pop (fheap)
  (declare (optimize (speed 3)))
  (symbol-macrolet ((position (fheap-position fheap)))
    (let ((costs (fheap-costs fheap))
          (vertices (fheap-vertices fheap)))
      (labels ((update (pos)
                 (declare (optimize (safety 0))
                          ((integer 1 #.most-positive-fixnum) pos))
                 (let* ((child-pos1 (+ pos pos))
                        (child-pos2 (1+ child-pos1)))
                   (when (<= child-pos1 position)
                     (if (<= child-pos2 position)
                         (if (< (aref costs child-pos1) (aref costs child-pos2))
                             (unless (< (aref costs pos) (aref costs child-pos1))
                               (rotatef (aref costs pos) (aref costs child-pos1))
                               (rotatef (aref vertices pos) (aref vertices child-pos1))
                               (update child-pos1))
                             (unless (< (aref costs pos) (aref costs child-pos2))
                               (rotatef (aref costs pos) (aref costs child-pos2))
                               (rotatef (aref vertices pos) (aref vertices child-pos2))
                               (update child-pos2)))
                         (unless (< (aref costs pos) (aref costs child-pos1))
                           (rotatef (aref costs pos) (aref costs child-pos1))
                           (rotatef (aref vertices pos) (aref vertices child-pos1))))))))
        (multiple-value-prog1 (values (aref costs 1) (aref vertices 1))
          (decf position)
          (setf (aref costs 1) (aref costs position)
                (aref vertices 1) (aref vertices position))
          (update 1))))))

(declaim (inline fheap-empty-p))
(defun fheap-empty-p (fheap)
  (= (fheap-position fheap) 1))

(declaim (inline fheap-reinitialize))
(defun fheap-reinitialize (heap)
  (setf (fheap-position heap) 1)
  heap)

(define-condition not-enough-capacity-error (error)
  ((graph :initarg :graph :reader not-enough-capacity-error-graph)
   (flow :initarg :flow :reader not-enough-capacity-error-flow)
   (score :initarg :score :reader not-enough-capacity-error-score))
  (:report
   (lambda (c s)
     (format s "Cannot send ~A units of flow on graph ~A due to not enough capacity."
             (not-enough-capacity-error-flow c)
             (not-enough-capacity-error-graph c)))))

(defun min-cost-flow! (src-idx dest-idx flow graph &key edge-count)
  "Returns the minimum cost to send FLOW units from SRC-IDX to DEST-IDX in
GRAPH. Destructively modifies GRAPH.

DENSITY := initial reserved size for heap (it should be the number of edges)"
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) flow)
           ((simple-array list (*)) graph))
  (macrolet ((the-cost-type (form)
               (reduce (lambda (x y) `(,(car form) (the cost-type ,x) (the cost-type ,y)))
		       (cdr form))))
    (let* ((size (length graph))
           (density (or density (* size 2)))
           (prev-vertices (make-array size :element-type 'fixnum :initial-element 0))
           (prev-edges (make-array size :element-type 'edge))
           (potential (make-array size :element-type 'cost-type :initial-element 0))
           (dist (make-array size :element-type 'cost-type))
           (pqueue (make-fheap density))
           (res 0))
      (declare (fixnum density)
               (cost-type res))
      ;; FIXME: Actually we must do Bellman-Ford here to handle negative edges
      ;; properly. Currently this function returns a correct result also for a
      ;; graph containing negative edges, if no negative **cycles** are
      ;; contained. In this case, however, the worst-case time complexity is
      ;; exponential. If the input network is for a weighted bipartite matching
      ;; containing negative weights, this function completely works without any
      ;; problems.
      (loop while (> flow 0)
            do (fill dist +inf-cost+)
               (setf (aref dist src-idx) 0)
               (fheap-reinitialize pqueue)
               (fheap-push 0 src-idx pqueue)
               (loop until (fheap-empty-p pqueue)
                     do (multiple-value-bind (cost v) (fheap-pop pqueue)
                          (declare (cost-type cost)
                                   (fixnum v))
                          (when (<= cost (aref dist v))
                            (dolist (edge (aref graph v))
                              (let* ((next-v (edge-to edge))
                                     (next-cost (the-cost-type
                                                 (+ (aref dist v)
                                                    (edge-cost edge)
                                                    (aref potential v)
                                                    (- (aref potential next-v))))))
                                (when (and (> (edge-capacity edge) 0)
                                           (> (aref dist next-v) next-cost))
                                  (setf (aref dist next-v) next-cost
                                        (aref prev-vertices next-v) v
                                        (aref prev-edges next-v) edge)
                                  (fheap-push next-cost next-v pqueue)))))))
               (when (= (aref dist dest-idx) +inf-cost+)
                 (error 'not-enough-capacity-error :flow flow :graph graph :score res))
               (let ((max-flow flow))
                 (declare (fixnum max-flow))
                 (dotimes (v size)
                   (setf (aref potential v)
                         (min +inf-cost+
                              (+ (aref potential v) (aref dist v)))))
                 (do ((v dest-idx (aref prev-vertices v)))
                     ((= v src-idx))
                   (setf max-flow (min max-flow (edge-capacity (aref prev-edges v)))))
                 (decf flow max-flow)
                 (incf res (the cost-type (* max-flow (aref potential dest-idx))))
                 (do ((v dest-idx (aref prev-vertices v)))
                     ((= v src-idx))
                   (decf (edge-capacity (aref prev-edges v)) max-flow)
                   (incf (edge-capacity (edge-reversed (aref prev-edges v))) max-flow))))
      res)))
