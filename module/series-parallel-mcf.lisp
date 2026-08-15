(defpackage :cp/series-parallel-mcf
  (:use :cl :cp/min-cost-bflow :cp/series-parallel-decomposition
        :cp/multi-slope-trick :cp/multi-slope-trick-rollback)
  (:export #:solve-bflow
           #:not-series-parallel-error #:not-series-parallel-error-problem)
  (:documentation "Provides an exact minimum cost b-flow solver for
generalized series-parallel multigraphs (graphs of treewidth <= 2, e.g. any
graph built from single edges by series and parallel composition), in
expected O(N log N) time for total input size N (nodes + arcs + cost
segments). Negative costs are fine without any precaution: negative-cost
cycles are saturated optimally.

Three passes over the series-parallel decomposition: each component carries
its cost profile -- the minimum internal cost as a convex piecewise-linear
function of the flow it delivers out of its second terminal -- represented as
a multi-slope-trick function; parallel merges are infimal convolutions and
series merges are translated pointwise sums, journaled with rollback tokens;
closures pin or minimize the profile; a reverse sweep then rolls the merges
back and splits each pin between the children by subdifferential queries,
yielding per-arc flows and per-node potentials."))
(in-package :cp/series-parallel-mcf)

(define-condition not-series-parallel-error (error)
  ((problem :initarg :problem :reader not-series-parallel-error-problem))
  (:report
   (lambda (c s)
     (format s "The multigraph of b-flow problem ~A is not generalized ~
                series-parallel."
             (not-series-parallel-error-problem c)))))

(declaim (inline finite-p))
(defun finite-p (slope)
  (< +negative-inf+ slope +positive-inf+))

(defun split-subgradient (p-c sub-p-l sub-p-r sub-q-l sub-q-r)
  "Splits P-C, a subgradient of the sum of two convex functions at a common
point, into (values p-p p-q) with P-P in [SUB-P-L, SUB-P-R], P-Q = P-C - P-P
in [SUB-Q-L, SUB-Q-R]. The feasible interval for P-P is [max(SUB-P-L, P-C -
SUB-Q-R), min(SUB-P-R, P-C - SUB-Q-L)], nonempty by convexity; bounds are
assembled only from finite parts -- the subdifferentials carry
+NEGATIVE-INF+/+POSITIVE-INF+ sentinels at domain boundaries, and arithmetic
on those would wrap."
  (declare (optimize (speed 3))
           (fixnum p-c sub-p-l sub-p-r sub-q-l sub-q-r))
  (let ((lo (if (finite-p sub-p-l) sub-p-l nil))
        (hi (if (finite-p sub-p-r) sub-p-r nil)))
    (when (finite-p sub-q-r)
      (let ((cand (- p-c sub-q-r)))
        (setq lo (if lo (max (the fixnum lo) cand) cand))))
    (when (finite-p sub-q-l)
      (let ((cand (- p-c sub-q-l)))
        (setq hi (if hi (min (the fixnum hi) cand) cand))))
    (let ((p-p (cond ((and lo hi) (min (the fixnum lo) (the fixnum hi)))
                     (lo lo)
                     (hi hi)
                     ;; Both subdifferentials are all of the line (two point
                     ;; domains).
                     (t 0))))
      (declare (fixnum p-p))
      (values p-p (- p-c p-p)))))

(defun build-leaf (cost forward)
  "Builds an arc's cost profile in its assigned orientation. Forward:
phi = c on [LO, HI]. Backward: phi(x) = c(-x) on [-HI, -LO] -- anchored at
(-HI, c(HI)) with the segment list reversed and the slopes negated."
  (declare (optimize (speed 3)))
  (if forward
      (let ((f (make-mstrick (bflow-cost-lo cost) (bflow-cost-value-at-lo cost))))
        (loop for (slope . width) in (bflow-cost-segments cost)
              do (mstrick-insert-segment f slope width))
        f)
      (let ((hi (bflow-cost-lo cost))
            (value-at-hi (bflow-cost-value-at-lo cost)))
        (declare (fixnum hi value-at-hi))
        (loop for (slope . width) in (bflow-cost-segments cost)
              do (locally (declare (fixnum slope width))
                   (incf hi width)
                   (incf value-at-hi (the fixnum (* slope width)))))
        (let ((f (make-mstrick (- hi) value-at-hi)))
          (loop for (slope . width) in (reverse (bflow-cost-segments cost))
                do (mstrick-insert-segment f (- (the fixnum slope)) width))
          f))))

(defun solve-bflow (problem)
  "Solves the minimum cost b-flow PROBLEM and returns a BFLOW-SOLUTION whose
potentials satisfy pi(head) - pi(tail) in subdiff c_a(x_a) for every arc -- a
self-contained optimality certificate.

Signals NOT-SERIES-PARALLEL-ERROR when some connected component of the
multigraph is not generalized series-parallel, and BFLOW-INFEASIBLE-ERROR
when no feasible flow exists."
  (declare (optimize (speed 3)))
  (let* ((num-nodes (bflow-problem-num-nodes problem))
         (arcs (bflow-problem-arcs problem))
         (num-arcs (length arcs))
         (arc-tails (make-array num-arcs :element-type 'fixnum))
         (arc-heads (make-array num-arcs :element-type 'fixnum)))
    (dotimes (i num-arcs)
      (let ((arc (aref arcs i)))
        (setf (aref arc-tails i) (bflow-arc-tail arc)
              (aref arc-heads i) (bflow-arc-head arc))))
    (let ((decomp (sp-decompose num-nodes arc-tails arc-heads)))
      (unless decomp
        (error 'not-series-parallel-error :problem problem))
      (let* ((num-comps (sp-decomposition-num-comps decomp))
             (ops (sp-decomposition-ops decomp))
             (o-tails (sp-decomposition-tails decomp))
             (o-heads (sp-decomposition-heads decomp))
             (balance (copy-seq (bflow-problem-balances problem)))
             (interior (make-array num-comps :element-type 'fixnum
                                             :initial-element 0))
             (slab (make-array num-comps :initial-element nil))
             (journal (make-array (length ops) :initial-element nil)))
        (flet ((infeasible ()
                 (error 'bflow-infeasible-error :problem problem)))
          (dotimes (i num-arcs)
            (setf (aref slab i)
                  (build-leaf (bflow-arc-cost (aref arcs i))
                              (= (aref o-tails i) (aref arc-tails i)))))
          ;; Forward pass: bottom-up evaluation over the op log.
          (loop
            for op-idx from 0 below (length ops)
            for op = (aref ops op-idx)
            do (etypecase op
                 (sp-parallel
                  (let* ((result (sp-parallel-result op))
                         (first (sp-parallel-first op))
                         (second (sp-parallel-second op))
                         (f (aref slab first))
                         (g (aref slab second)))
                    (setf (aref journal op-idx)
                          (mstrick-inf-conv-with-rollback f g)
                          (aref interior result)
                          (the fixnum (+ (aref interior first)
                                         (aref interior second)))
                          (aref slab result) f)))
                 (sp-series
                  (let* ((result (sp-series-result op))
                         (first (sp-series-first op))
                         (second (sp-series-second op))
                         (mid (sp-series-mid op))
                         ;; The P-role child delivers into the merged-away
                         ;; node MID -- its assigned orientation is
                         ;; (. , MID); the Q-role child delivers the merged
                         ;; component's flow out of its second terminal.
                         (p (if (= (aref o-heads first) mid) first second))
                         (q (if (= p first) second first))
                         (fp (aref slab p))
                         (fq (aref slab q))
                         ;; Conservation at MID shifts the through-flow:
                         ;; phi_C(x) = phi_P(x - delta) + phi_Q(x) with
                         ;; delta = B_Q + b(MID).
                         (delta (the fixnum (+ (aref interior q)
                                               (aref balance mid)))))
                    (mstrick-translate fp delta)
                    (when (or (< (mstrick-dom-max fp) (mstrick-dom-min fq))
                              (< (mstrick-dom-max fq) (mstrick-dom-min fp)))
                      (infeasible))
                    (setf (aref journal op-idx)
                          (cons delta (mstrick-pointwise-add-with-rollback fp fq))
                          (aref interior result)
                          (the fixnum (+ (aref interior first)
                                         (aref interior second)
                                         (aref balance mid)))
                          (aref slab result) fp)))
                 (sp-pendant
                  (let* ((comp (sp-pendant-comp op))
                         (pendant (sp-pendant-node op))
                         (survivor (sp-pendant-survivor op))
                         (func (aref slab comp))
                         ;; Conservation at the pendant pins the delivered
                         ;; flow.
                         (x-pin (- (aref balance pendant))))
                    (when (or (< x-pin (mstrick-dom-min func))
                              (< (mstrick-dom-max func) x-pin))
                      (infeasible))
                    (incf (aref balance survivor)
                          (the fixnum (+ (aref balance pendant)
                                         (aref interior comp))))
                    (setf (aref journal op-idx) (cons func x-pin)
                          (aref slab comp) nil)))
                 (sp-loop
                  (let* ((comp (sp-loop-comp op))
                         (node (sp-loop-node op))
                         (func (aref slab comp))
                         ;; The loop flow is an unconstrained circulation:
                         ;; record a minimizer for recovery.
                         (x-min (mstrick-arg-subdiff func 0)))
                    (incf (aref balance node) (aref interior comp))
                    (setf (aref journal op-idx) (cons func x-min)
                          (aref slab comp) nil)))
                 (sp-node-check
                  (unless (zerop (aref balance (sp-node-check-node op)))
                    (infeasible))))))
        ;; Reverse pass: roll the merges back and split each component's pin
        ;; (x_C, p_C) -- flow delivered out of its second terminal and
        ;; potential rise across it, with p_C a subgradient of the profile
        ;; at x_C -- between the children.
        (let ((pin-xs (make-array num-comps :element-type 'fixnum
                                            :initial-element 0))
              (pin-ps (make-array num-comps :element-type 'fixnum
                                            :initial-element 0))
              (potentials (make-array num-nodes :element-type 'fixnum
                                                :initial-element 0)))
          (loop
            for op-idx from (1- (length ops)) downto 0
            for op = (aref ops op-idx)
            do (etypecase op
                 (sp-node-check
                  ;; Seed: one isolated node ends each connected component.
                  (setf (aref potentials (sp-node-check-node op)) 0))
                 (sp-pendant
                  (let* ((comp (sp-pendant-comp op))
                         (func (car (aref journal op-idx)))
                         (x-pin (cdr (aref journal op-idx))))
                    ;; Canonical finite subgradient at the pin: right slope
                    ;; if finite, else left, else zero (point domain).
                    (multiple-value-bind (sub-l sub-r)
                        (mstrick-subdiff func x-pin)
                      (let ((p (cond ((finite-p sub-r) sub-r)
                                     ((finite-p sub-l) sub-l)
                                     (t 0))))
                        (setf (aref potentials (sp-pendant-node op))
                              (the fixnum
                                   (+ (aref potentials (sp-pendant-survivor op))
                                      p))
                              (aref pin-xs comp) x-pin
                              (aref pin-ps comp) p
                              (aref slab comp) func)))))
                 (sp-loop
                  ;; Zero is a valid subgradient at an argmin, and the loop's
                  ;; terminals coincide, so the potential rise is zero.
                  (let ((comp (sp-loop-comp op)))
                    (setf (aref pin-xs comp) (cdr (aref journal op-idx))
                          (aref pin-ps comp) 0
                          (aref slab comp) (car (aref journal op-idx)))))
                 (sp-parallel
                  (let* ((result (sp-parallel-result op))
                         (first (sp-parallel-first op))
                         (second (sp-parallel-second op))
                         (x-c (aref pin-xs result))
                         (p-c (aref pin-ps result))
                         (f (aref slab result))
                         (g (mstrick-inf-conv-rollback f (aref journal op-idx))))
                    ;; Both children inherit p_C; the flow splits inside the
                    ;; children's argmin intervals at p_C (all finite:
                    ;; compact domains).
                    (multiple-value-bind (l-p r-p) (mstrick-arg-subdiff f p-c)
                      (let* ((l-q (mstrick-arg-subdiff g p-c))
                             (x-p (max l-p (min r-p (- x-c l-q))))
                             (x-q (- x-c x-p)))
                        (setf (aref pin-xs first) x-p
                              (aref pin-ps first) p-c
                              (aref pin-xs second) x-q
                              (aref pin-ps second) p-c
                              (aref slab first) f
                              (aref slab second) g)))))
                 (sp-series
                  (let* ((result (sp-series-result op))
                         (first (sp-series-first op))
                         (second (sp-series-second op))
                         (mid (sp-series-mid op))
                         (x-c (aref pin-xs result))
                         (p-c (aref pin-ps result))
                         (p (if (= (aref o-heads first) mid) first second))
                         (q (if (= p first) second first))
                         (delta (car (aref journal op-idx)))
                         (fp (aref slab result))
                         (fq (mstrick-pointwise-add-rollback
                              fp (cdr (aref journal op-idx)))))
                    (declare (fixnum delta))
                    ;; FP is still the translated psi_P(x) = phi_P(x - delta),
                    ;; so both subdifferentials are read at x_C.
                    (multiple-value-bind (sub-p-l sub-p-r)
                        (mstrick-subdiff fp x-c)
                      (multiple-value-bind (sub-q-l sub-q-r)
                          (mstrick-subdiff fq x-c)
                        (multiple-value-bind (p-p p-q)
                            (split-subgradient p-c sub-p-l sub-p-r
                                               sub-q-l sub-q-r)
                          (setf (aref potentials mid)
                                (the fixnum
                                     (+ (aref potentials (aref o-tails result))
                                        p-p)))
                          (mstrick-translate fp (- delta))
                          (setf (aref pin-xs p) (- x-c delta)
                                (aref pin-ps p) p-p
                                (aref pin-xs q) x-c
                                (aref pin-ps q) p-q
                                (aref slab p) fp
                                (aref slab q) fq))))))))
          ;; Extract per-arc flows; the objective is evaluated on them.
          (let ((flows (make-array num-arcs :element-type 'fixnum))
                (objective 0))
            (declare (fixnum objective))
            (dotimes (i num-arcs)
              (let ((x (if (= (aref o-tails i) (aref arc-tails i))
                           (aref pin-xs i)
                           (- (aref pin-xs i)))))
                (setf (aref flows i) x)
                (incf objective
                      (bflow-cost-value (bflow-arc-cost (aref arcs i)) x))))
            (make-bflow-solution objective flows potentials)))))))
