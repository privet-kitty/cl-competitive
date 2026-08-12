(defpackage :cp/series-parallel-decomposition
  (:use :cl)
  (:export #:sp-decompose #:sp-decomposition
           #:sp-decomposition-num-comps #:sp-decomposition-ops
           #:sp-decomposition-tails #:sp-decomposition-heads
           #:sp-parallel #:sp-parallel-result #:sp-parallel-first #:sp-parallel-second
           #:sp-series #:sp-series-result #:sp-series-first #:sp-series-second
           #:sp-series-mid #:sp-series-end0 #:sp-series-end1
           #:sp-pendant #:sp-pendant-comp #:sp-pendant-node #:sp-pendant-survivor
           #:sp-loop #:sp-loop-comp #:sp-loop-node
           #:sp-node-check #:sp-node-check-node)
  (:documentation "Provides recognition and decomposition of generalized
series-parallel multigraphs (equivalently, graphs of treewidth <= 2, checked
per connected component) by the reduction system: loop close, parallel merge,
series merge, pendant close, isolated-node check. The result is the op log in
reduction order plus an orientation (ordered terminal pair) per component,
from which a consumer can evaluate any associative series/parallel semiring
bottom-up and replay it top-down. Worst-case O(n + m) time.

Reference: Valdes, Tarjan, and Lawler, The recognition of series parallel
digraphs, 1979."))
(in-package :cp/series-parallel-decomposition)

;; One reduction step. Component ids 0, ..., m-1 are the input edges
;; themselves; merged components get fresh ids in creation order.

(defstruct (sp-parallel (:constructor make-sp-parallel (result first second))
                        (:copier nil)
                        (:predicate nil))
  "Two components sharing both terminals merged into RESULT."
  (result 0 :type (integer 0 #.most-positive-fixnum) :read-only t)
  (first 0 :type (integer 0 #.most-positive-fixnum) :read-only t)
  (second 0 :type (integer 0 #.most-positive-fixnum) :read-only t))

(defstruct (sp-series (:constructor make-sp-series
                          (result first second mid end0 end1))
                      (:copier nil)
                      (:predicate nil))
  "Two components meeting at the degree-2 node MID merged into RESULT with
terminal pair (END0, END1), where FIRST is the child incident to END0 and
SECOND the child incident to END1. The outer terminals are distinct (END0 /=
END1 /= MID): a series merge fires only after every parallel bundle at MID has
been merged away, so it never creates a loop."
  (result 0 :type (integer 0 #.most-positive-fixnum) :read-only t)
  (first 0 :type (integer 0 #.most-positive-fixnum) :read-only t)
  (second 0 :type (integer 0 #.most-positive-fixnum) :read-only t)
  (mid 0 :type (integer 0 #.most-positive-fixnum) :read-only t)
  (end0 0 :type (integer 0 #.most-positive-fixnum) :read-only t)
  (end1 0 :type (integer 0 #.most-positive-fixnum) :read-only t))

(defstruct (sp-pendant (:constructor make-sp-pendant (comp node survivor))
                       (:copier nil)
                       (:predicate nil))
  "Degree-1 node NODE closed with its single component COMP; the other
terminal is SURVIVOR."
  (comp 0 :type (integer 0 #.most-positive-fixnum) :read-only t)
  (node 0 :type (integer 0 #.most-positive-fixnum) :read-only t)
  (survivor 0 :type (integer 0 #.most-positive-fixnum) :read-only t))

(defstruct (sp-loop (:constructor make-sp-loop (comp node))
                    (:copier nil)
                    (:predicate nil))
  "Component COMP with both terminals at NODE closed."
  (comp 0 :type (integer 0 #.most-positive-fixnum) :read-only t)
  (node 0 :type (integer 0 #.most-positive-fixnum) :read-only t))

(defstruct (sp-node-check (:constructor make-sp-node-check (node))
                          (:copier nil)
                          (:predicate nil))
  "Isolated node: no live component is incident to NODE anymore."
  (node 0 :type (integer 0 #.most-positive-fixnum) :read-only t))

(defstruct (sp-decomposition (:constructor %make-sp-decomposition
                                 (num-comps ops tails heads))
                             (:copier nil)
                             (:predicate nil))
  "Structural decomposition of a generalized series-parallel multigraph: the
op log in reduction order and the orientation -- the ordered terminal pair
(TAILS[c], HEADS[c]) -- of every component. The pendant convention is
survivor -> pendant; a loop is oriented (node, node); a merge result's
orientation determines both children's."
  (num-comps 0 :type (integer 0 #.most-positive-fixnum) :read-only t)
  (ops #() :type simple-vector :read-only t)
  (tails #() :type (simple-array fixnum (*)) :read-only t)
  (heads #() :type (simple-array fixnum (*)) :read-only t))

(defun sp-decompose (num-nodes tails heads)
  "Reduces the multigraph with NUM-NODES nodes and the edges (TAILS[i],
HEADS[i]) -- parallel edges and self-loops allowed, connectivity not required
-- to an SP-DECOMPOSITION, or returns NIL if some connected component is not
generalized series-parallel (i.e. the reduction stalls on a subgraph of min
degree >= 3, which has a K4 minor).

The reduction is the deterministic unsatisfied-list scheme: each live
component is an edge of a working multigraph with an unordered terminal pair;
a stack holds the nodes whose reducibility is unresolved. Processing a node
compacts its incidence list in place -- dead entries are purged and parallel
bundles merge as they are discovered -- until either the whole list is
compacted, so its length is the node's reduced degree and the matching node
reduction applies, or a third distinct neighbor turns up, which proves the
node irreducible until a series or pendant reduction elsewhere removes one of
its incident components; exactly those events re-push the affected nodes. All
bookkeeping is index-based, so the op log is a pure function of the input."
  (declare ((integer 0 #.most-positive-fixnum) num-nodes))
  (let* ((tails (coerce tails '(simple-array fixnum (*))))
         (heads (coerce heads '(simple-array fixnum (*))))
         (num-edges (length tails))
         ;; Unordered terminal pair per component, in creation order.
         (ends0 (make-array num-edges :element-type 'fixnum
                                      :adjustable t :fill-pointer 0))
         (ends1 (make-array num-edges :element-type 'fixnum
                                      :adjustable t :fill-pointer 0))
         (comp-alive (make-array num-edges :element-type 'bit
                                           :adjustable t :fill-pointer 0))
         (node-alive (make-array num-nodes :element-type 'bit
                                           :initial-element 1))
         ;; Per node: incident non-loop component ids; entries go stale when
         ;; a component dies elsewhere and are purged lazily.
         (incidence (make-array num-nodes :element-type t))
         (node-stack nil)
         (ops nil)
         (live-comps 0))
    (declare (fixnum live-comps))
    (assert (= num-edges (length heads)))
    (dotimes (v num-nodes)
      (setf (aref incidence v)
            (make-array 2 :element-type 'fixnum :adjustable t :fill-pointer 0))
      (push v node-stack))
    (labels ((new-comp (u v)
               (vector-push-extend u ends0)
               (vector-push-extend v ends1)
               (vector-push-extend 1 comp-alive)
               (incf live-comps)
               (1- (fill-pointer ends0)))
             (kill-comp (comp)
               (setf (aref comp-alive comp) 0)
               (decf live-comps))
             (far-end (comp v)
               ;; The terminal of non-loop component COMP other than V.
               (let ((a (aref ends0 comp)))
                 (if (= a v) (aref ends1 comp) a)))
             (process-node (v)
               ;; Scan invariant: entries [0, k) are alive with pairwise
               ;; distinct far ends; entry K is the next to examine.
               (let ((inc (aref incidence v))
                     (k 0))
                 (declare (fixnum k))
                 (loop
                   (when (>= k (fill-pointer inc))
                     (return))
                   (let ((comp (aref inc k)))
                     (cond ((zerop (aref comp-alive comp))
                            ;; Stale entry: swap-remove.
                            (setf (aref inc k)
                                  (aref inc (1- (fill-pointer inc))))
                            (decf (fill-pointer inc)))
                           (t
                            (let* ((far (far-end comp v))
                                   (dup (loop for j from 0 below k
                                              when (= far (far-end (aref inc j) v))
                                                do (return j))))
                              (cond (dup
                                     ;; Parallel merge with the compacted
                                     ;; entry sharing both terminals; the
                                     ;; merged component takes its slot,
                                     ;; preserving the scan invariant. The
                                     ;; far node's neighbor set is unchanged,
                                     ;; so it needs no re-examination.
                                     (let ((first (aref inc dup)))
                                       (kill-comp first)
                                       (kill-comp comp)
                                       (let ((result (new-comp v far)))
                                         (push (make-sp-parallel result first comp)
                                               ops)
                                         (setf (aref inc dup) result
                                               (aref inc k)
                                               (aref inc (1- (fill-pointer inc))))
                                         (decf (fill-pointer inc))
                                         (vector-push-extend
                                          result (aref incidence far)))))
                                    ((= k 2) (return-from process-node))
                                    (t (incf k))))))))
                 ;; The whole list is compacted: V has exactly K <= 2 live
                 ;; incident components, with distinct far ends.
                 (ecase k
                   (0 (push (make-sp-node-check v) ops)
                      (setf (aref node-alive v) 0))
                   (1 (let* ((comp (aref inc 0))
                             (survivor (far-end comp v)))
                        (kill-comp comp)
                        (push (make-sp-pendant comp v survivor) ops)
                        (setf (aref node-alive v) 0)
                        (push survivor node-stack)))
                   (2 (let* ((first (aref inc 0))
                             (second (aref inc 1))
                             (a (far-end first v))
                             (b (far-end second v)))
                        (kill-comp first)
                        (kill-comp second)
                        (let ((result (new-comp a b)))
                          (push (make-sp-series result first second v a b) ops)
                          (setf (aref node-alive v) 0)
                          ;; The merged component may be parallel to an
                          ;; existing {A, B} component; re-examining the
                          ;; endpoints catches it.
                          (vector-push-extend result (aref incidence a))
                          (vector-push-extend result (aref incidence b))
                          (push a node-stack)
                          (push b node-stack))))))))
      ;; Input self-loops close immediately; no reduction ever creates
      ;; another loop (series terminals are distinct by construction and
      ;; parallel merges keep their endpoints).
      (dotimes (i num-edges)
        (let ((u (aref tails i))
              (v (aref heads i)))
          (assert (and (< u num-nodes) (< v num-nodes)))
          (let ((comp (new-comp u v)))
            (if (= u v)
                (progn
                  (kill-comp comp)
                  (push (make-sp-loop comp u) ops))
                (progn
                  (vector-push-extend comp (aref incidence u))
                  (vector-push-extend comp (aref incidence v)))))))
      (loop while node-stack
            do (let ((v (pop node-stack)))
                 (when (= 1 (aref node-alive v))
                   (process-node v))))
      (when (plusp live-comps)
        (return-from sp-decompose nil))
      (let* ((num-comps (fill-pointer ends0))
             (ops (coerce (nreverse ops) 'simple-vector))
             (orient-tails (make-array num-comps :element-type 'fixnum))
             (orient-heads (make-array num-comps :element-type 'fixnum))
             (oriented (make-array num-comps :element-type 'bit
                                             :initial-element 0)))
        ;; Reverse sweep: a closure op orients its component, and a merge
        ;; op's result orientation uniquely determines both children's -- so
        ;; every leaf knows its required orientation before any consumer
        ;; builds a value for it. Interior components never flip.
        (flet ((orient (comp tail head)
                 (setf (aref orient-tails comp) tail
                       (aref orient-heads comp) head
                       (aref oriented comp) 1)))
          (loop for i from (1- (length ops)) downto 0
                for op = (aref ops i)
                do (etypecase op
                     (sp-pendant
                      (orient (sp-pendant-comp op)
                              (sp-pendant-survivor op) (sp-pendant-node op)))
                     (sp-loop
                      (orient (sp-loop-comp op)
                              (sp-loop-node op) (sp-loop-node op)))
                     (sp-node-check)
                     (sp-parallel
                      (let ((result (sp-parallel-result op)))
                        (assert (= 1 (aref oriented result)))
                        (orient (sp-parallel-first op)
                                (aref orient-tails result)
                                (aref orient-heads result))
                        (orient (sp-parallel-second op)
                                (aref orient-tails result)
                                (aref orient-heads result))))
                     (sp-series
                      (let ((result (sp-series-result op))
                            (mid (sp-series-mid op))
                            (a (sp-series-end0 op))
                            (b (sp-series-end1 op)))
                        (assert (= 1 (aref oriented result)))
                        (cond ((and (= (aref orient-tails result) a)
                                    (= (aref orient-heads result) b))
                               (orient (sp-series-first op) a mid)
                               (orient (sp-series-second op) mid b))
                              (t
                               (assert (and (= (aref orient-tails result) b)
                                            (= (aref orient-heads result) a)))
                               (orient (sp-series-second op) b mid)
                               (orient (sp-series-first op) mid a))))))))
        (assert (loop for b across oriented always (= b 1)))
        (%make-sp-decomposition num-comps ops orient-tails orient-heads)))))
