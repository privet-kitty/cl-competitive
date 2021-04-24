(defpackage :cp/shortest-augmenting-path
  (:use :cl :cp/max-flow)
  (:export #:max-flow!)
  (:documentation
   "Provides shortest augmenting path algorithm for maximum flow problem. Time
complexity is O(EV^2).

Reference:
Ahuja, Magnanti, Orlin. Network flows

NOTE: Not tested. Please use cp/dinic instead."))
(in-package :cp/shortest-augmenting-path)

(defconstant +graph-inf-distance+ #xffffffff)

(defun %make-distance-labels (graph dest)
  "Makes initial distance labels to DEST."
  (declare (optimize (speed 3))
           ((simple-array list (*)) graph))
  (let* ((n (length graph))
         (que (make-array n :element-type '(unsigned-byte 32)))
         (dists (make-array n :element-type '(unsigned-byte 32)
                              :initial-element +graph-inf-distance+))
         (front 0)
         (end 0))
    (declare ((mod #.array-dimension-limit) front end))
    (labels ((enqueue (x)
               (setf (aref que end) x)
               (incf end))
             (dequeue ()
               (prog1 (aref que front)
                 (incf front))))
      (setf (aref dists dest) 0)
      (enqueue dest)
      (loop until (= front end)
            for v = (dequeue)
            for dist = (aref dists v)
            do (dolist (edge (aref graph v))
                 (let ((to (edge-to edge)))
                   (when (and (> (edge-capacity (edge-reversed edge)) 0)
                              (= +graph-inf-distance+ (aref dists to)))
                     (setf (aref dists to) (+ dist 1))
                     (enqueue to)))))
      dists)))

(defun max-flow! (graph src sink)
  "Destructively sends the maximum flow from SRC to SINK and returns the amount
of the flow. This function signals MAX-FLOW-OVERFLOW error when an infinite
flow (to be precise, >= MOST-POSITIVE-FIXNUM) is possible."
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) src sink)
           ((simple-array list (*)) graph))
  (assert (/= src sink))
  (let* ((n (length graph))
         (dists (%make-distance-labels graph sink))
         (nums (make-array (+ n 1) :element-type '(unsigned-byte 32) :initial-element 0))
         (preds (make-array n :element-type '(unsigned-byte 32)))
         (subgraph (copy-seq graph))
         (node src)
         (flow 0))
    (declare ((simple-array (unsigned-byte 32) (*)) dists)
             ((integer 0 #.most-positive-fixnum) flow))
    (dotimes (i n)
      (when (< (aref dists i) +graph-inf-distance+)
        (incf (aref nums (aref dists i)))))
    (labels ((admissible-p (node edge)
               (and (> (edge-capacity edge) 0)
                    (= (aref dists node)
                       (+ 1 (aref dists (edge-to edge)))))))
      (loop
        (loop for edge = (car (aref subgraph node))
              while edge
              when (admissible-p node edge)
              do (let ((to (edge-to edge)))
                   ;; advance
                   (setf (aref preds to) node
                         node to)
                   (when (= node sink)
                     ;; send as much flow as possible along the augmenting path
                     (let ((delta
                             (loop with v = sink
                                   do (setq v (aref preds v))
                                   minimize (edge-capacity (car (aref subgraph v)))
                                   until (= v src))))
                       (declare ((integer 0 #.most-positive-fixnum) delta))
                       (when (>= (+ flow delta) most-positive-fixnum)
                         (error 'max-flow-overflow :graph graph))
                       (incf flow delta)
                       (loop with v = sink
                             do (setq v (aref preds v))
                                (let ((edge (car (aref subgraph v))))
                                  (decf (edge-capacity edge) delta)
                                  (incf (edge-capacity (edge-reversed edge)) delta))
                             until (= v src)))
                     (setq node src)))
                 (return)
              else do (pop (aref subgraph node))
              finally ;; do relabeling when there're no admissible arcs
                 (let ((min-label +graph-inf-distance+)
                       (current-label (aref dists node)))
                   (declare ((unsigned-byte 32) min-label))
                   (when (or (= current-label +graph-inf-distance+)
                             (= 1 (aref nums current-label)))
                     (return-from max-flow! flow))
                   (dolist (edge (aref graph node))
                     (when (> (edge-capacity edge) 0)
                       (setq min-label
                             (min min-label (aref dists (edge-to edge))))))
                   ;; return if
                   ;; 1. a gap occurs in distance labels or
                   ;; 2. there are no more admissible arcs from source
                   (when (= min-label +graph-inf-distance+)
                     (return-from max-flow! flow))
                   (decf (aref nums current-label))
                   (incf (aref nums (+ 1 min-label)))
                   (setf (aref dists node) (+ 1 min-label)))
                 (setf (aref subgraph node) (aref graph node))
                 (unless (= node src)
                   (setq node (aref preds node))))))))
