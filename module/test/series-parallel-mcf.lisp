(defpackage :cp/test/series-parallel-mcf
  (:use :cl :fiveam :cp/min-cost-bflow :cp/series-parallel-mcf)
  (:import-from :cp/min-cost-flow #:add-cedge #:not-enough-capacity-error)
  (:import-from :cp/ssp)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/series-parallel-mcf)
(in-suite base-suite)

(defun finite-p (slope)
  (< most-negative-fixnum slope most-positive-fixnum))

(defun check-certificate (problem sol)
  "End-to-end certificate check: conservation, flow-range feasibility,
objective consistency, and the per-arc subgradient optimality condition
pi(head) - pi(tail) in subdiff c_a(x_a). Feasibility plus the subgradient
condition certify global optimality, so a passing solution is verified
without any oracle."
  (let* ((n (bflow-problem-num-nodes problem))
         (arcs (bflow-problem-arcs problem))
         (flows (bflow-solution-flows sol))
         (potentials (bflow-solution-potentials sol))
         (net (make-array n :initial-element 0))
         (objective 0))
    (assert (= (length flows) (length arcs)))
    (assert (= (length potentials) n))
    (loop
      for arc across arcs
      for x across flows
      do (let ((cost (bflow-arc-cost arc)))
           (unless (<= (bflow-cost-lo cost) x (bflow-cost-hi cost))
             (error "flow ~A outside [~A, ~A]"
                    x (bflow-cost-lo cost) (bflow-cost-hi cost)))
           (incf (aref net (bflow-arc-tail arc)) x)
           (decf (aref net (bflow-arc-head arc)) x)
           (incf objective (bflow-cost-value cost x))
           (let ((p (- (aref potentials (bflow-arc-head arc))
                       (aref potentials (bflow-arc-tail arc)))))
             (multiple-value-bind (sub-l sub-r) (bflow-cost-subdiff cost x)
               (when (and (finite-p sub-l) (< p sub-l))
                 (error "subgradient violated: p = ~A below left slope ~A"
                        p sub-l))
               (when (and (finite-p sub-r) (< sub-r p))
                 (error "subgradient violated: p = ~A above right slope ~A"
                        p sub-r))))))
    (dotimes (v n)
      (unless (= (aref net v) (aref (bflow-problem-balances problem) v))
        (error "conservation violated at node ~A: net ~A, balance ~A"
               v (aref net v) (aref (bflow-problem-balances problem) v))))
    (unless (= objective (bflow-solution-objective sol))
      (error "objective mismatch: flows cost ~A, reported ~A"
             objective (bflow-solution-objective sol))))
  t)

(defun try-solve (problem)
  "Returns the certificate-checked optimal objective, or NIL when the solver
reports infeasibility."
  (handler-case
      (let ((sol (solve-bflow problem)))
        (check-certificate problem sol)
        (bflow-solution-objective sol))
    (bflow-infeasible-error () nil)))

(defun solve-mcf-ssp (problem)
  "Independent oracle, deliberately method-disjoint from the solver: expands
each convex segment into a parallel linear arc, shifts out lower bounds and
saturates negative-cost segments by the standard adjust-balances transforms
\(convexity keeps the segment-wise relaxation exact), then routes the residual
balances from a super source to a super sink with cp/ssp -- all expanded
costs are nonnegative, so plain Dijkstra applies. Returns the optimal
objective, or NIL when the instance is infeasible. Works on any multigraph."
  (let* ((n (bflow-problem-num-nodes problem))
         (source n)
         (sink (+ n 1))
         (graph (make-array (+ n 2) :element-type 'list :initial-element nil))
         (base-cost 0)
         (residual (copy-seq (bflow-problem-balances problem)))
         (supply 0)
         (demand 0))
    (loop
      for arc across (bflow-problem-arcs problem)
      do (let ((tail (bflow-arc-tail arc))
               (head (bflow-arc-head arc))
               (cost (bflow-arc-cost arc)))
           ;; Shift out the lower bound: x = lo + sum of y_j, y_j in [0, w_j].
           (incf base-cost (bflow-cost-value-at-lo cost))
           (decf (aref residual tail) (bflow-cost-lo cost))
           (incf (aref residual head) (bflow-cost-lo cost))
           (loop for (slope . width) in (bflow-cost-segments cost)
                 do (cond ((< slope 0)
                           ;; Saturate the negative-cost segment and add the
                           ;; reversed arc; conservation shifts to the
                           ;; balances.
                           (incf base-cost (* slope width))
                           (decf (aref residual tail) width)
                           (incf (aref residual head) width)
                           (add-cedge graph head tail (- slope) width))
                          (t
                           (add-cedge graph tail head slope width))))))
    (dotimes (v n)
      (let ((b (aref residual v)))
        (cond ((> b 0)
               (add-cedge graph source v 0 b)
               (incf supply b))
              ((< b 0)
               (add-cedge graph v sink 0 (- b))
               (incf demand (- b))))))
    (if (/= supply demand)
        nil
        (handler-case
            (+ base-cost (cp/ssp:min-cost-flow! graph source sink supply))
          (not-enough-capacity-error () nil)))))

(defun brute-force-bflow (problem)
  "Exhaustive minimum over all integer arc flows within the flow ranges;
returns NIL when infeasible. Shares nothing with either solver."
  (let* ((n (bflow-problem-num-nodes problem))
         (arcs (bflow-problem-arcs problem))
         (balances (bflow-problem-balances problem))
         (net (make-array n :initial-element 0))
         (best nil))
    (labels ((recur (i acc)
               (if (= i (length arcs))
                   (when (and (loop for v below n
                                    always (= (aref net v) (aref balances v)))
                              (or (null best) (< acc best)))
                     (setq best acc))
                   (let* ((arc (aref arcs i))
                          (cost (bflow-arc-cost arc))
                          (tail (bflow-arc-tail arc))
                          (head (bflow-arc-head arc)))
                     (loop for x from (bflow-cost-lo cost) to (bflow-cost-hi cost)
                           do (incf (aref net tail) x)
                              (decf (aref net head) x)
                              (recur (1+ i) (+ acc (bflow-cost-value cost x)))
                              (decf (aref net tail) x)
                              (incf (aref net head) x))))))
      (recur 0 0))
    best))

(defun random-gsp-edges (edges-per-component components)
  "Random generalized series-parallel multigraph built by construction:
series subdivision, parallel duplication, pendant attachment, self-loops,
random directions. Returns (values num-nodes edges) with EDGES a list of
\(from . to)."
  (let ((num-nodes 0)
        (all-edges nil))
    (dotimes (_ components)
      (let ((comp-nodes (list num-nodes (+ num-nodes 1)))
            (comp-edges (list (cons num-nodes (+ num-nodes 1))))
            (comp-count 1))
        (incf num-nodes 2)
        (loop while (< comp-count edges-per-component)
              do (case (random 10)
                   ((0 1 2 3)
                    (let* ((cell (nth (random comp-count) comp-edges))
                           (u (car cell))
                           (v (cdr cell)))
                      (unless (= u v)
                        (let ((w num-nodes))
                          (incf num-nodes)
                          (push w comp-nodes)
                          (setf (cdr cell) w)
                          (push (cons w v) comp-edges)
                          (incf comp-count)))))
                   ((4 5 6)
                    (let ((cell (nth (random comp-count) comp-edges)))
                      (unless (= (car cell) (cdr cell))
                        (push (cons (car cell) (cdr cell)) comp-edges)
                        (incf comp-count))))
                   ((7 8)
                    (let ((v (nth (random (length comp-nodes)) comp-nodes))
                          (w num-nodes))
                      (incf num-nodes)
                      (push w comp-nodes)
                      (push (cons v w) comp-edges)
                      (incf comp-count)))
                   (t
                    (let ((v (nth (random (length comp-nodes)) comp-nodes)))
                      (push (cons v v) comp-edges)
                      (incf comp-count)))))
        (dolist (edge comp-edges)
          (push (if (zerop (random 2))
                    (cons (cdr edge) (car edge))
                    (cons (car edge) (cdr edge)))
                all-edges))))
    (values num-nodes all-edges)))

(defun random-cost (&key small)
  "Random convex piecewise-linear cost with a small integral domain that may
span negative flows; SMALL keeps the flow range narrow enough for exhaustive
enumeration."
  (if small
      (let* ((lo (- (random 3)))
             (n-segs (random 3))
             (slope (- (random 9) 5))
             (segments nil))
        (dotimes (_ n-segs)
          (push (cons slope (+ 1 (random 2))) segments)
          (incf slope (+ 1 (random 3))))
        (make-bflow-cost lo (- (random 11) 5) (nreverse segments)))
      (let* ((lo (- (random 5)))
             (n-segs (random 4))
             (slope (- (random 9) 6))
             (segments nil))
        (dotimes (_ n-segs)
          (push (cons slope (+ 1 (random 3))) segments)
          (incf slope (+ 1 (random 3))))
        (make-bflow-cost lo (- (random 11) 5) (nreverse segments)))))

(defun random-gsp-problem (edges-per-component components &key small)
  "Random generalized-SP instance with balances sampled from a random
feasible flow, so the instance is feasible by construction."
  (multiple-value-bind (n edges) (random-gsp-edges edges-per-component components)
    (let* ((arcs (loop for (u . v) in edges
                       collect (make-bflow-arc u v (random-cost :small small))))
           (balances (make-array n :element-type 'fixnum :initial-element 0)))
      (dolist (arc arcs)
        (let* ((cost (bflow-arc-cost arc))
               (lo (bflow-cost-lo cost))
               (x (+ lo (random (+ 1 (- (bflow-cost-hi cost) lo))))))
          (incf (aref balances (bflow-arc-tail arc)) x)
          (decf (aref balances (bflow-arc-head arc)) x)))
      (make-bflow-problem n balances arcs))))

(defun reflect-cost (cost)
  "The reflected cost c'(x) = c(-x): domain [-HI, -LO], anchored at c(HI),
segments reversed with negated slopes."
  (make-bflow-cost (- (bflow-cost-hi cost))
                   (bflow-cost-value cost (bflow-cost-hi cost))
                   (loop for (slope . width) in (reverse (bflow-cost-segments cost))
                         collect (cons (- slope) width))))

(test series-parallel-mcf/hand
  ;; One arc 0 -> 1 with cost 2x on [0, 5]; b = (3, -3) pins x = 3.
  (let* ((problem (make-bflow-problem
                   2 '(3 -3)
                   (list (make-bflow-arc 0 1 (make-linear-bflow-cost 2 0 5)))))
         (sol (solve-bflow problem)))
    (is (= 6 (bflow-solution-objective sol)))
    (is (equalp #(3) (bflow-solution-flows sol)))
    (is (check-certificate problem sol)))
  ;; Domain spans negative flow; b = (-2, 2) forces x = -2.
  (let* ((problem (make-bflow-problem
                   2 '(-2 2)
                   (list (make-bflow-arc 0 1 (make-linear-bflow-cost 3 -4 4)))))
         (sol (solve-bflow problem)))
    (is (= -6 (bflow-solution-objective sol)))
    (is (equalp #(-2) (bflow-solution-flows sol)))
    (is (check-certificate problem sol)))
  ;; Two parallel arcs 0 -> 1, rates 1 and 3, caps 2 each; demand 3 sends 2
  ;; on the cheap arc and 1 on the expensive one.
  (let* ((problem (make-bflow-problem
                   2 '(3 -3)
                   (list (make-bflow-arc 0 1 (make-linear-bflow-cost 1 0 2))
                         (make-bflow-arc 0 1 (make-linear-bflow-cost 3 0 2)))))
         (sol (solve-bflow problem)))
    (is (= 5 (bflow-solution-objective sol)))
    (is (equalp #(2 1) (bflow-solution-flows sol)))
    (is (check-certificate problem sol)))
  ;; Path 0 -> 1 -> 2 with b = (1, 2, -3): the second arc carries the
  ;; interior node's supply on top of the through-flow.
  (let* ((problem (make-bflow-problem
                   3 '(1 2 -3)
                   (list (make-bflow-arc 0 1 (make-linear-bflow-cost 1 0 5))
                         (make-bflow-arc 1 2 (make-linear-bflow-cost 2 0 5)))))
         (sol (solve-bflow problem)))
    (is (= 7 (bflow-solution-objective sol)))
    (is (equalp #(1 3) (bflow-solution-flows sol)))
    (is (check-certificate problem sol)))
  ;; 2-cycle with negative total cost and zero balances: the optimal
  ;; circulation saturates the cycle at the joint capacity 2.
  (let* ((problem (make-bflow-problem
                   2 '(0 0)
                   (list (make-bflow-arc 0 1 (make-linear-bflow-cost -3 0 2))
                         (make-bflow-arc 1 0 (make-linear-bflow-cost 1 0 3)))))
         (sol (solve-bflow problem)))
    (is (= -4 (bflow-solution-objective sol)))
    (is (check-certificate problem sol))
    (is (= -4 (solve-mcf-ssp problem))))
  ;; A self-loop's flow is free: the negative-rate range saturates, and the
  ;; convex kink stops it at the slope sign change.
  (let* ((problem (make-bflow-problem
                   1 '(0)
                   (list (make-bflow-arc 0 0 (make-bflow-cost
                                              0 0 '((-4 . 2) (1 . 3)))))))
         (sol (solve-bflow problem)))
    (is (= -8 (bflow-solution-objective sol)))
    (is (equalp #(2) (bflow-solution-flows sol)))
    (is (check-certificate problem sol)))
  ;; Triangle with piecewise-linear costs, against the SSP oracle.
  (let* ((problem (make-bflow-problem
                   3 '(4 -1 -3)
                   (list (make-bflow-arc 0 1 (make-bflow-cost
                                              -1 -2 '((1 . 2) (4 . 3))))
                         (make-bflow-arc 1 2 (make-bflow-cost
                                              0 0 '((0 . 1) (2 . 2))))
                         (make-bflow-arc 0 2 (make-bflow-cost
                                              -2 1 '((-1 . 3) (3 . 4)))))))
         (sol (solve-bflow problem)))
    (is (= (solve-mcf-ssp problem) (bflow-solution-objective sol)))
    (is (check-certificate problem sol)))
  ;; A point-domain arc forces exactly 2 units through; the parallel arc
  ;; absorbs the remainder of the demand.
  (let* ((problem (make-bflow-problem
                   2 '(5 -5)
                   (list (make-bflow-arc 0 1 (make-bflow-cost 2 10 nil))
                         (make-bflow-arc 0 1 (make-linear-bflow-cost 1 0 5)))))
         (sol (solve-bflow problem)))
    (is (= 13 (bflow-solution-objective sol)))
    (is (equalp #(2 3) (bflow-solution-flows sol)))
    (is (check-certificate problem sol)))
  ;; Both arcs have point domains: the recovery's subgradient split sees
  ;; all-of-the-line subdifferentials on both children.
  (let* ((problem (make-bflow-problem
                   3 '(2 1 -3)
                   (list (make-bflow-arc 0 1 (make-bflow-cost 2 5 nil))
                         (make-bflow-arc 1 2 (make-bflow-cost 3 7 nil)))))
         (sol (solve-bflow problem)))
    (is (= 12 (bflow-solution-objective sol)))
    (is (equalp #(2 3) (bflow-solution-flows sol)))
    (is (check-certificate problem sol)))
  ;; An isolated node must have zero balance.
  (let ((sol (solve-bflow (make-bflow-problem 1 '(0) '()))))
    (is (= 0 (bflow-solution-objective sol))))
  (signals bflow-infeasible-error
    (solve-bflow (make-bflow-problem 1 '(1) '())))
  ;; Capacity shortfall.
  (signals bflow-infeasible-error
    (solve-bflow (make-bflow-problem
                  2 '(7 -7)
                  (list (make-bflow-arc 0 1 (make-linear-bflow-cost 1 0 5))))))
  ;; K4 is not series-parallel.
  (signals not-series-parallel-error
    (solve-bflow
     (make-bflow-problem
      4 '(0 0 0 0)
      (loop for (u . v) in '((0 . 1) (0 . 2) (0 . 3) (1 . 2) (1 . 3) (2 . 3))
            collect (make-bflow-arc u v (make-linear-bflow-cost 1 0 1)))))))

(test series-parallel-mcf/random-agreement-with-ssp
  ;; Feasible-by-construction instances: objective agreement with the SSP
  ;; oracle plus the optimality certificate. Optima need not be unique, so
  ;; flows and potentials are never compared directly.
  (let ((*random-state* (sb-ext:seed-random-state 0))
        (*test-dribble* nil))
    (finishes
      (dotimes (_ 400)
        (let* ((problem (random-gsp-problem (+ 1 (random 11)) (+ 1 (random 2))))
               (expected (solve-mcf-ssp problem))
               (got (try-solve problem)))
          (unless (eql got expected)
            (error "solver ~A vs oracle ~A on ~A" got expected problem)))))))

(test series-parallel-mcf/random-unbalanced-infeasible
  ;; Nonzero total balance: infeasible via a pendant pin or the final node
  ;; check.
  (let ((*random-state* (sb-ext:seed-random-state 1))
        (*test-dribble* nil))
    (finishes
      (dotimes (_ 200)
        (let* ((problem (random-gsp-problem 6 1))
               (balances (copy-seq (bflow-problem-balances problem)))
               (v (random (bflow-problem-num-nodes problem))))
          (incf (aref balances v) (if (zerop (random 2)) 1 -3))
          (let ((skewed (make-bflow-problem (bflow-problem-num-nodes problem)
                                            balances
                                            (bflow-problem-arcs problem))))
            (unless (null (try-solve skewed))
              (error "unbalanced instance accepted"))
            (unless (null (solve-mcf-ssp skewed))
              (error "oracle accepted an unbalanced instance"))))))))

(test series-parallel-mcf/random-capacity-infeasible
  ;; Balanced totals, but a same-component supply/demand pair larger than
  ;; every capacity bound.
  (let ((*random-state* (sb-ext:seed-random-state 2))
        (*test-dribble* nil))
    (finishes
      (dotimes (_ 200)
        (let* ((problem (random-gsp-problem 6 1))
               (arcs (bflow-problem-arcs problem))
               (arc (aref arcs (random (length arcs))))
               (u (bflow-arc-tail arc))
               (v (bflow-arc-head arc)))
          (unless (= u v)
            (let ((big (+ 1 (loop for a across arcs
                                  sum (max (abs (bflow-cost-lo (bflow-arc-cost a)))
                                           (abs (bflow-cost-hi (bflow-arc-cost a)))))))
                  (balances (copy-seq (bflow-problem-balances problem))))
              (incf (aref balances u) big)
              (decf (aref balances v) big)
              (let ((skewed (make-bflow-problem
                             (bflow-problem-num-nodes problem) balances arcs)))
                (unless (null (try-solve skewed))
                  (error "over-capacity instance accepted"))
                (unless (null (solve-mcf-ssp skewed))
                  (error "oracle accepted an over-capacity instance"))))))))))

(test series-parallel-mcf/random-zero-balance-negative-cycles
  ;; Zero balances with negative-cost segments: finite flow ranges make the
  ;; minimum cost circulation well-posed; agreement with the oracle. Zero
  ;; balances can still be infeasible when some arc's flow range excludes
  ;; zero and no circulation can return the forced flow.
  (let ((*random-state* (sb-ext:seed-random-state 3))
        (*test-dribble* nil))
    (finishes
      (dotimes (_ 300)
        (multiple-value-bind (n edges)
            (random-gsp-edges (+ 1 (random 9)) (+ 1 (random 2)))
          (let* ((problem (make-bflow-problem
                           n
                           (make-array n :element-type 'fixnum
                                         :initial-element 0)
                           (loop for (u . v) in edges
                                 collect (make-bflow-arc u v (random-cost)))))
                 (expected (solve-mcf-ssp problem))
                 (got (try-solve problem)))
            (unless (eql got expected)
              (error "solver ~A vs oracle ~A on ~A" got expected problem))))))))

(test series-parallel-mcf/random-vs-brute-force
  ;; Small instances with random balances against an exhaustive oracle.
  (let ((*random-state* (sb-ext:seed-random-state 4))
        (*test-dribble* nil))
    (finishes
      (dotimes (_ 300)
        (let* ((problem (random-gsp-problem (+ 1 (random 5)) 1 :small t))
               (n (bflow-problem-num-nodes problem))
               (balances (copy-seq (bflow-problem-balances problem))))
          ;; Half the trials perturb the sampled-feasible balances by a
          ;; zero-sum shift, which may or may not stay feasible.
          (when (and (zerop (random 2)) (> n 1))
            (let ((u (random n))
                  (v (random n))
                  (d (- (random 5) 2)))
              (incf (aref balances u) d)
              (decf (aref balances v) d)))
          (let* ((perturbed (make-bflow-problem
                             n balances (bflow-problem-arcs problem)))
                 (expected (brute-force-bflow perturbed))
                 (got (try-solve perturbed)))
            (unless (eql got expected)
              (error "solver ~A vs brute force ~A on ~A"
                     got expected perturbed))))))))

(test series-parallel-mcf/random-relabeling-invariance
  (let ((*random-state* (sb-ext:seed-random-state 5))
        (*test-dribble* nil))
    (finishes
      (dotimes (_ 150)
        (let* ((problem (random-gsp-problem 8 1))
               (n (bflow-problem-num-nodes problem))
               (perm (let ((perm (make-array n)))
                       (dotimes (v n) (setf (aref perm v) v))
                       (loop for i from (1- n) above 0
                             do (rotatef (aref perm i)
                                         (aref perm (random (1+ i)))))
                       perm))
               (balances (make-array n :element-type 'fixnum))
               (arcs (loop for arc across (bflow-problem-arcs problem)
                           collect (make-bflow-arc
                                    (aref perm (bflow-arc-tail arc))
                                    (aref perm (bflow-arc-head arc))
                                    (bflow-arc-cost arc)))))
          (dotimes (v n)
            (setf (aref balances (aref perm v))
                  (aref (bflow-problem-balances problem) v)))
          (let ((relabeled (make-bflow-problem n balances arcs)))
            (unless (= (the fixnum (try-solve problem))
                       (the fixnum (try-solve relabeled)))
              (error "relabeling changed the objective on ~A" problem))))))))

(test series-parallel-mcf/random-reversal-invariance
  ;; Reversing every arc with the reflected cost c'(x) = c(-x) describes the
  ;; same problem, so the objective is unchanged.
  (let ((*random-state* (sb-ext:seed-random-state 6))
        (*test-dribble* nil))
    (finishes
      (dotimes (_ 150)
        (let* ((problem (random-gsp-problem 8 1))
               (reversed (make-bflow-problem
                          (bflow-problem-num-nodes problem)
                          (bflow-problem-balances problem)
                          (loop for arc across (bflow-problem-arcs problem)
                                collect (make-bflow-arc
                                         (bflow-arc-head arc)
                                         (bflow-arc-tail arc)
                                         (reflect-cost (bflow-arc-cost arc)))))))
          (unless (= (the fixnum (try-solve problem))
                     (the fixnum (try-solve reversed)))
            (error "arc reversal changed the objective on ~A" problem)))))))
