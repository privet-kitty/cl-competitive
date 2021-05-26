(defpackage :cp/ssp-slow
  (:use :cl :cp/min-cost-flow)
  (:export #:min-cost-flow!)
  (:documentation "Provides successive shortest path algorithm for minimum cost
flow problem. Please use cp/ssp instead. I keep this for future reference."))
(in-package :cp/ssp-slow)

(defun min-cost-flow! (graph src-idx dest-idx flow)
  "Returns the minimum cost to send FLOW units from SRC-IDX to DEST-IDX in
GRAPH. Destructively modifies GRAPH."
  (declare (fixnum flow)
           ((simple-array list (*)) graph))
  (let* ((size (length graph))
         (prev-vertices (make-array size :element-type 'fixnum :initial-element 0))
         (prev-edges (make-array size :element-type 'cedge))
         (dist (make-array size :element-type 'fixnum))
         (res 0))
    (declare (fixnum res))
    (loop while (> flow 0)
          for updated = t
          do (fill dist +inf-cost+)
             (setf (aref dist src-idx) 0)
             (loop while updated
                   do (setf updated nil)
                      (dotimes (v size)
                        (unless (= (aref dist v) +inf-cost+)
                          (dolist (edge (aref graph v))
                            (let ((cost-sum (+ (aref dist v) (cedge-cost edge))))
                              (when (and (> (cedge-capacity edge) 0)
                                         (> (aref dist (cedge-to edge)) cost-sum))
                                (setf (aref dist (cedge-to edge)) cost-sum
                                      (aref prev-vertices (cedge-to edge)) v
                                      (aref prev-edges (cedge-to edge)) edge
                                      updated t)))))))
             (when (= (aref dist dest-idx) +inf-cost+)
               (error 'not-enough-capacity-error :flow flow :graph graph))
             (let ((max-flow flow))
               (declare (fixnum max-flow))
               (do ((v dest-idx (aref prev-vertices v)))
                   ((= v src-idx))
                 (setf max-flow (min max-flow (cedge-capacity (aref prev-edges v)))))
               (decf flow max-flow)
               (incf res (the fixnum (* max-flow (aref dist dest-idx))))
               (do ((v dest-idx (aref prev-vertices v)))
                   ((= v src-idx))
                 (decf (cedge-capacity (aref prev-edges v)) max-flow)
                 (incf (cedge-capacity (cedge-reversed (aref prev-edges v))) max-flow))))
    res))
