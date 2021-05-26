(defpackage :cp/min-cost-flow
  (:use :cl :cp/max-flow)
  (:export #:cedge #:cedge-p #:copy-cedge #:add-cedge #:+inf-cost+ #:cost-type
           #:cedge-reversed #:cedge-cost #:cedge-capacity #:cedge-to #:cedge-default-capacity
           #:not-enough-capacity-error #:not-enough-capacity-error-graph
           #:not-enough-capacity-error-flow #:not-enough-capacity-error-score)
  (:documantation "Provides data structure for minimum cost flow."))
(in-package :cp/min-cost-flow)

;; COST-TYPE and +INF-COST+ may be changed. (A supposed use case is to adopt
;; bignum).

(deftype cost-type () 'fixnum)
(defconstant +inf-cost+ most-positive-fixnum)
(assert (and (typep +inf-cost+ 'cost-type)
             (subtypep 'cost-type 'integer)))

(defstruct (cedge (:constructor %make-cedge)
                  (:include edge))
  (cost 0 :type cost-type))

(define-condition not-enough-capacity-error (error)
  ((graph :initarg :graph :reader not-enough-capacity-error-graph)
   (flow :initarg :flow :reader not-enough-capacity-error-flow)
   (score :initarg :score :reader not-enough-capacity-error-score))
  (:report
   (lambda (c s)
     (format s "Cannot send ~A units of flow on graph ~A due to not enough capacity."
             (not-enough-capacity-error-flow c)
             (not-enough-capacity-error-graph c)))))

(defmethod print-object ((cedge cedge) stream)
  (let ((*print-circle* t))
    (call-next-method)))

(defun add-cedge (graph from-idx to-idx cost capacity)
  "FROM-IDX, TO-IDX := index of vertex
GRAPH := vector of list of all the edges that goes from the vertex"
  (declare ((simple-array list (*)) graph)
           (cost-type cost))
  (let* ((dep (%make-cedge :to to-idx :capacity capacity :cost cost))
         (ret (%make-cedge :to from-idx :capacity 0 :cost (- cost) :reversed dep)))
    (setf (cedge-reversed dep) ret)
    (push dep (aref graph from-idx))
    (push ret (aref graph to-idx))))
