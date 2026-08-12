(defpackage :cp/min-cost-bflow
  (:use :cl)
  (:export #:bflow-cost #:make-bflow-cost #:make-linear-bflow-cost
           #:bflow-cost-lo #:bflow-cost-value-at-lo #:bflow-cost-segments
           #:bflow-cost-hi #:bflow-cost-value #:bflow-cost-subdiff
           #:bflow-arc #:make-bflow-arc
           #:bflow-arc-tail #:bflow-arc-head #:bflow-arc-cost
           #:bflow-problem #:make-bflow-problem
           #:bflow-problem-num-nodes #:bflow-problem-balances #:bflow-problem-arcs
           #:bflow-solution #:make-bflow-solution
           #:bflow-solution-objective #:bflow-solution-flows
           #:bflow-solution-potentials
           #:bflow-infeasible-error #:bflow-infeasible-error-problem)
  (:documentation "Provides problem and solution types for minimum cost b-flow
on a directed multigraph: every node carries a required net outflow (supply
positive, demand negative) and every arc a convex piecewise-linear cost with a
compact flow range [LO, HI], where a negative LO allows reverse flow. The
classical s-t minimum cost flow is the special case of two nonzero balances
and linear costs on [0, capacity].

This module defines only the data structures; solvers (e.g.
cp/series-parallel-mcf) consume them."))
(in-package :cp/min-cost-bflow)

;; MOST-NEGATIVE-FIXNUM and MOST-POSITIVE-FIXNUM act as the -inf/+inf
;; sentinels of BFLOW-COST-VALUE and BFLOW-COST-SUBDIFF.

(defstruct (bflow-cost (:constructor %make-bflow-cost (lo value-at-lo segments))
                       (:copier nil)
                       (:predicate nil))
  "Convex piecewise-linear arc cost with the compact effective domain
[LO, HI = LO + sum of widths] -- the arc's flow range. Stored as the value at
the left domain end plus the (slope . width) segments in strictly increasing
slope order; SEGMENTS is empty for a fixed-flow arc (x = LO)."
  (lo 0 :type fixnum :read-only t)
  (value-at-lo 0 :type fixnum :read-only t)
  (segments nil :type list :read-only t))

(defun make-bflow-cost (lo value-at-lo segments)
  "Validating constructor: SEGMENTS must be a list of (slope . width) conses
with positive widths and strictly increasing slopes."
  (declare (fixnum lo value-at-lo)
           (list segments))
  (let ((prev-slope nil))
    (loop for (slope . width) in segments
          do (check-type slope fixnum)
             (check-type width fixnum)
             (unless (>= width 1)
               (error "make-bflow-cost: segment width ~A is not positive" width))
             (when (and prev-slope (<= slope (the fixnum prev-slope)))
               (error "make-bflow-cost: slopes must be strictly increasing"))
             (setq prev-slope slope)))
  (%make-bflow-cost lo value-at-lo segments))

(defun make-linear-bflow-cost (rate lo hi)
  "Classical linear cost with a flow range: c(x) = RATE*x on [LO, HI].
LO = HI gives a fixed-flow cost."
  (declare (fixnum rate lo hi))
  (when (< hi lo)
    (error "make-linear-bflow-cost: HI = ~A < LO = ~A" hi lo))
  (%make-bflow-cost lo (the fixnum (* rate lo))
                    (when (< lo hi) (list (cons rate (- hi lo))))))

(declaim (ftype (function * (values fixnum &optional)) bflow-cost-hi))
(defun bflow-cost-hi (cost)
  "Returns the right end of the flow range."
  (let ((x (bflow-cost-lo cost)))
    (declare (fixnum x))
    (loop for (nil . width) in (bflow-cost-segments cost)
          do (incf x (the fixnum width)))
    x))

(declaim (ftype (function * (values fixnum &optional)) bflow-cost-value))
(defun bflow-cost-value (cost x)
  "Returns the cost at X; MOST-POSITIVE-FIXNUM outside [LO, HI]."
  (declare (fixnum x))
  (let ((left (bflow-cost-lo cost))
        (value (bflow-cost-value-at-lo cost)))
    (declare (fixnum left value))
    (when (< x left)
      (return-from bflow-cost-value most-positive-fixnum))
    (loop for (slope . width) in (bflow-cost-segments cost)
          do (locally (declare (fixnum slope width))
               (let ((right (+ left width)))
                 (when (<= x right)
                   (return-from bflow-cost-value
                     (the fixnum (+ value (the fixnum (* slope (- x left)))))))
                 (incf value (the fixnum (* slope width)))
                 (setq left right))))
    (if (= x left) value most-positive-fixnum)))

(declaim (ftype (function * (values fixnum fixnum &optional)) bflow-cost-subdiff))
(defun bflow-cost-subdiff (cost x)
  "Returns the subdifferential at X as (values left-slope right-slope), with
MOST-NEGATIVE-FIXNUM/MOST-POSITIVE-FIXNUM sentinels at the domain boundaries
and both-sentinel outside the domain."
  (declare (fixnum x))
  (let ((lo (bflow-cost-lo cost))
        (hi (bflow-cost-hi cost)))
    (cond ((< x lo) (values most-negative-fixnum most-negative-fixnum))
          ((< hi x) (values most-positive-fixnum most-positive-fixnum))
          (t
           (let ((left-slope most-negative-fixnum)
                 (right-slope most-positive-fixnum)
                 (left lo))
             (declare (fixnum left-slope right-slope left))
             (loop for (slope . width) in (bflow-cost-segments cost)
                   do (locally (declare (fixnum slope width))
                        (let ((right (+ left width)))
                          (when (and (< left x) (<= x right))
                            (setq left-slope slope))
                          (when (and (<= left x) (< x right))
                            (setq right-slope slope)
                            (return))
                          (setq left right))))
             (values left-slope right-slope))))))

(defstruct (bflow-arc (:constructor make-bflow-arc (tail head cost))
                      (:copier nil)
                      (:predicate nil))
  "Directed arc of the multigraph; parallel arcs and self-loops are allowed.
Flow is positive in the tail -> head direction."
  (tail 0 :type (integer 0 #.most-positive-fixnum) :read-only t)
  (head 0 :type (integer 0 #.most-positive-fixnum) :read-only t)
  (cost nil :type bflow-cost :read-only t))

(defstruct (bflow-problem (:constructor %make-bflow-problem
                              (num-nodes balances arcs))
                          (:copier nil)
                          (:predicate nil))
  "Minimum cost b-flow instance: find per-arc flows within the flow ranges
whose net outflow at every node equals its balance, minimizing the total arc
cost."
  (num-nodes 0 :type (integer 0 #.most-positive-fixnum) :read-only t)
  ;; Required net outflow per node (supply positive, demand negative).
  (balances #() :type (simple-array fixnum (*)) :read-only t)
  ;; Vector of BFLOW-ARC.
  (arcs #() :type simple-vector :read-only t))

(defun make-bflow-problem (num-nodes balances arcs)
  "Validating constructor. BALANCES and ARCS may be any sequences."
  (declare ((integer 0 #.most-positive-fixnum) num-nodes))
  (let ((balances (coerce balances '(simple-array fixnum (*))))
        (arcs (coerce arcs 'simple-vector)))
    (unless (= (length balances) num-nodes)
      (error "make-bflow-problem: ~A balances for ~A nodes"
             (length balances) num-nodes))
    (loop for arc across arcs
          do (check-type arc bflow-arc)
             (unless (and (< (bflow-arc-tail arc) num-nodes)
                          (< (bflow-arc-head arc) num-nodes))
               (error "make-bflow-problem: arc endpoint out of range")))
    (%make-bflow-problem num-nodes balances arcs)))

(defstruct (bflow-solution (:constructor make-bflow-solution
                               (objective flows potentials))
                           (:copier nil)
                           (:predicate nil))
  "Optimal solution with a self-contained optimality certificate: FLOWS are
feasible and the POTENTIALS satisfy pi(head) - pi(tail) in subdiff c_a(x_a)
for every arc, which certifies global optimality of a feasible convex-cost
flow."
  (objective 0 :type fixnum :read-only t)
  ;; Per-arc flows, indexed like BFLOW-PROBLEM-ARCS.
  (flows #() :type (simple-array fixnum (*)) :read-only t)
  ;; Per-node potentials, indexed like BFLOW-PROBLEM-BALANCES.
  (potentials #() :type (simple-array fixnum (*)) :read-only t))

(define-condition bflow-infeasible-error (error)
  ((problem :initarg :problem :reader bflow-infeasible-error-problem))
  (:report
   (lambda (c s)
     (format s "Minimum cost b-flow problem ~A is infeasible."
             (bflow-infeasible-error-problem c)))))
