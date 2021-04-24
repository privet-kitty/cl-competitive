(defpackage :cp/dinic
  (:use :cl :cp/max-flow)
  (:export #:max-flow!)
  (:documentation "Provides Dinic's algorithm. Time complexity: O(EV^2)."))
(in-package :cp/dinic)

(defconstant +graph-inf-distance+ #xffffffff)

(defun %fill-dist-table (graph src dist-table queue)
  "Does BFS and sets DIST-TABLE to the distance between SRC and each vertex of
GRAPH, where an edge of zero capacity is regarded as disconnected."
  (declare (optimize (speed 3) (safety 0))
           ((mod #.array-dimension-limit) src)
           ((simple-array list (*)) graph)
           ((simple-array (unsigned-byte 32) (*)) dist-table queue))
  (let* ((q-front 0)
         (q-end 0))
    (declare ((mod #.array-dimension-limit) q-front q-end))
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

(declaim (ftype (function * (values (integer 0 #.most-positive-fixnum) &optional))
                %find-path))
(defun %find-path (src dest tmp-graph dist-table)
  "Finds an augmenting path, sends the maximum flow through it, and returns the
amount of the flow."
  (declare (optimize (speed 3) (safety 0))
           ((mod #.array-dimension-limit) src dest)
           ((simple-array list (*)) tmp-graph)
           ((simple-array (unsigned-byte 32) (*)) dist-table))
  (labels ((dfs (v flow)
             (declare ((mod #.array-dimension-limit) v)
                      ((integer 0 #.most-positive-fixnum) flow))
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
               (pop (aref tmp-graph v)))))
    (dfs src most-positive-fixnum)))

(declaim (ftype (function * (values (mod #.most-positive-fixnum) &optional))
                max-flow!))
(defun max-flow! (graph src dest)
  "Destructively sends the maximum flow from SRC to DEST and returns the amount
of the flow. This function signals MAX-FLOW-OVERFLOW error when an infinite
flow (to be precise, >= MOST-POSITIVE-FIXNUM) is possible."
  (declare #+sbcl (sb-ext:muffle-conditions style-warning)
           ((mod #.array-dimension-limit) src dest)
           ((simple-array list (*)) graph))
  (let* ((n (length graph))
         (dist-table (make-array n :element-type '(unsigned-byte 32)))
         (queue (make-array n :element-type '(unsigned-byte 32)))
         (tmp-graph (make-array n :element-type 'list))
         (result 0))
    (declare ((integer 0 #.most-positive-fixnum) result))
    (loop
      (%fill-dist-table graph src dist-table queue)
      (when (= (aref dist-table dest) +graph-inf-distance+)
        ;; SRC and DEST are not connected on the current residual network.
        (return result))
      (dotimes (i n)
        (setf (aref tmp-graph i) (aref graph i)))
      (loop for delta = (%find-path src dest tmp-graph dist-table)
            until (zerop delta)
            do (when (>= (+ result delta) most-positive-fixnum)
                 (error 'max-flow-overflow :graph graph))
               (incf result delta)))))
