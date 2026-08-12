(defpackage :cp/test/series-parallel-decomposition
  (:use :cl :fiveam :cp/series-parallel-decomposition)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/series-parallel-decomposition)
(in-suite base-suite)

(defun validate-decomposition (num-nodes tails heads d)
  "Replays the op log generically and checks every structural invariant: ops
consume live components and create fresh ids in order, terminals are
consistent, each node dies exactly once (pendant, series mid, or node check),
and the orientation assignment is coherent."
  (let* ((num-edges (length tails))
         (num-comps (sp-decomposition-num-comps d))
         (o-tails (sp-decomposition-tails d))
         (o-heads (sp-decomposition-heads d))
         (ends (make-array num-comps :initial-element nil))
         (alive (make-array num-comps :initial-element nil))
         (node-alive (make-array num-nodes :initial-element t))
         (next-comp num-edges))
    (assert (= num-comps (length o-tails) (length o-heads)))
    (dotimes (i num-edges)
      (setf (aref ends i) (cons (elt tails i) (elt heads i))
            (aref alive i) t))
    (flet ((as-set (pair)
             (cons (min (car pair) (cdr pair)) (max (car pair) (cdr pair))))
           (orientation (c) (cons (aref o-tails c) (aref o-heads c))))
      (loop
        for op across (sp-decomposition-ops d)
        do (etypecase op
             (sp-parallel
              (let ((result (sp-parallel-result op))
                    (first (sp-parallel-first op))
                    (second (sp-parallel-second op)))
                (assert (and (aref alive first) (aref alive second)))
                (setf (aref alive first) nil (aref alive second) nil)
                (assert (= result next-comp))
                (incf next-comp)
                (let ((e1 (aref ends first))
                      (e2 (aref ends second)))
                  (assert (equal (as-set e1) (as-set e2)))
                  (assert (/= (car e1) (cdr e1)))
                  (setf (aref ends result) e1
                        (aref alive result) t)
                  (let ((o (orientation result)))
                    (assert (equal (as-set o) (as-set e1)))
                    (assert (equal (orientation first) o))
                    (assert (equal (orientation second) o))))))
             (sp-series
              (let ((result (sp-series-result op))
                    (first (sp-series-first op))
                    (second (sp-series-second op))
                    (mid (sp-series-mid op))
                    (a (sp-series-end0 op))
                    (b (sp-series-end1 op)))
                (assert (and (aref alive first) (aref alive second)))
                (setf (aref alive first) nil (aref alive second) nil)
                (assert (= result next-comp))
                (incf next-comp)
                (assert (aref node-alive mid))
                (setf (aref node-alive mid) nil)
                (assert (and (/= a mid) (/= b mid) (/= a b)))
                (assert (and (aref node-alive a) (aref node-alive b)))
                (assert (equal (as-set (aref ends first)) (as-set (cons a mid))))
                (assert (equal (as-set (aref ends second)) (as-set (cons mid b))))
                (setf (aref ends result) (cons a b)
                      (aref alive result) t)
                (let ((o (orientation result)))
                  (cond ((equal o (cons a b))
                         (assert (equal (orientation first) (cons a mid)))
                         (assert (equal (orientation second) (cons mid b))))
                        (t
                         (assert (equal o (cons b a)))
                         (assert (equal (orientation second) (cons b mid)))
                         (assert (equal (orientation first) (cons mid a))))))))
             (sp-pendant
              (let ((comp (sp-pendant-comp op))
                    (pendant (sp-pendant-node op))
                    (survivor (sp-pendant-survivor op)))
                (assert (aref alive comp))
                (setf (aref alive comp) nil)
                (assert (and (aref node-alive pendant) (aref node-alive survivor)))
                (setf (aref node-alive pendant) nil)
                (assert (equal (as-set (aref ends comp))
                               (as-set (cons survivor pendant))))
                (assert (/= pendant survivor))
                (assert (equal (orientation comp) (cons survivor pendant)))))
             (sp-loop
              (let ((comp (sp-loop-comp op))
                    (node (sp-loop-node op)))
                (assert (aref alive comp))
                (setf (aref alive comp) nil)
                (assert (aref node-alive node))
                (assert (equal (aref ends comp) (cons node node)))
                (assert (equal (orientation comp) (cons node node)))))
             (sp-node-check
              (let ((node (sp-node-check-node op)))
                (assert (aref node-alive node))
                (setf (aref node-alive node) nil)))))
      (assert (= next-comp num-comps))
      (assert (every #'null alive))
      (assert (notany #'identity node-alive))))
  t)

(defun random-gsp-graph (edges-per-component components &key dag)
  "Random generalized series-parallel multigraph, built by construction so
membership in the class is guaranteed: each connected component starts as a
single edge and grows by series subdivision, parallel duplication, pendant
attachment, and self-loops. Returns (values num-nodes tails heads). With DAG,
loops are skipped and edge directions never flip, so the construction stays a
directed acyclic graph."
  (let ((num-nodes 0)
        (tails nil)
        (heads nil))
    (dotimes (_ components)
      (let ((comp-nodes (list num-nodes (+ num-nodes 1)))
            (comp-edges (list (cons num-nodes (+ num-nodes 1))))
            (comp-count 1))
        (incf num-nodes 2)
        (loop while (< comp-count edges-per-component)
              do (case (random 10)
                   ((0 1 2 3)
                    ;; Series subdivision of a random non-loop edge.
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
                    ;; Parallel duplication of a random non-loop edge.
                    (let ((cell (nth (random comp-count) comp-edges)))
                      (unless (= (car cell) (cdr cell))
                        (push (cons (car cell) (cdr cell)) comp-edges)
                        (incf comp-count))))
                   ((7 8)
                    ;; Pendant attachment.
                    (let ((v (nth (random (length comp-nodes)) comp-nodes))
                          (w num-nodes))
                      (incf num-nodes)
                      (push w comp-nodes)
                      (push (cons v w) comp-edges)
                      (incf comp-count)))
                   (t
                    ;; Self-loop.
                    (unless dag
                      (let ((v (nth (random (length comp-nodes)) comp-nodes)))
                        (push (cons v v) comp-edges)
                        (incf comp-count))))))
        (dolist (edge comp-edges)
          (if (and (not dag) (zerop (random 2)))
              (progn (push (cdr edge) tails) (push (car edge) heads))
              (progn (push (car edge) tails) (push (cdr edge) heads))))))
    (values num-nodes tails heads)))

(test series-parallel-decomposition/hand
  ;; Single arc: pendant close then node check.
  (let ((d (sp-decompose 2 '(0) '(1))))
    (validate-decomposition 2 '(0) '(1) d)
    (is (= 2 (length (sp-decomposition-ops d))))
    (is (typep (aref (sp-decomposition-ops d) 0) 'sp-pendant))
    (is (typep (aref (sp-decomposition-ops d) 1) 'sp-node-check)))
  ;; Self-loop closes then checks.
  (let ((d (sp-decompose 1 '(0) '(0))))
    (validate-decomposition 1 '(0) '(0) d)
    (is (typep (aref (sp-decomposition-ops d) 0) 'sp-loop))
    (is (= 0 (aref (sp-decomposition-tails d) 0)))
    (is (= 0 (aref (sp-decomposition-heads d) 0))))
  ;; Isolated node gets checked.
  (let ((d (sp-decompose 3 '(0) '(1))))
    (validate-decomposition 3 '(0) '(1) d)
    (is (find-if (lambda (op) (and (typep op 'sp-node-check)
                                   (= 2 (sp-node-check-node op))))
                 (sp-decomposition-ops d))))
  ;; Parallel pair merges.
  (let ((d (sp-decompose 2 '(0 1) '(1 0))))
    (validate-decomposition 2 '(0 1) '(1 0) d)
    (let ((op (aref (sp-decomposition-ops d) 0)))
      (is (typep op 'sp-parallel))
      (is (= 2 (sp-parallel-result op)))))
  ;; Triangle: one series merge, one parallel merge.
  (let ((d (sp-decompose 3 '(0 1 2) '(1 2 0))))
    (validate-decomposition 3 '(0 1 2) '(1 2 0) d)
    (is (= 1 (count-if (lambda (op) (typep op 'sp-series))
                       (sp-decomposition-ops d))))
    (is (= 1 (count-if (lambda (op) (typep op 'sp-parallel))
                       (sp-decomposition-ops d)))))
  ;; Theta graph: three parallel edges need two parallel merges.
  (let ((d (sp-decompose 2 '(0 0 1) '(1 1 0))))
    (validate-decomposition 2 '(0 0 1) '(1 1 0) d)
    (is (= 2 (count-if (lambda (op) (typep op 'sp-parallel))
                       (sp-decomposition-ops d)))))
  ;; A 2-cycle of doubled edges: parallels create the shared-endpoint
  ;; situation a naive series rule would turn into loops; the parallel-first
  ;; discipline must resolve it.
  (let ((tails '(0 0 1 1 2))
        (heads '(1 1 2 2 0)))
    (validate-decomposition 3 tails heads (sp-decompose 3 tails heads)))
  ;; Nested loops and pendants.
  (let ((tails '(0 0 1 1 2 3 3))
        (heads '(0 1 1 2 3 3 1)))
    (validate-decomposition 4 tails heads (sp-decompose 4 tails heads))))

(test series-parallel-decomposition/rejection
  ;; K4, the minimal non-series-parallel graph.
  (is (null (sp-decompose 4 '(0 0 0 1 1 2) '(1 2 3 2 3 3))))
  ;; K33 (treewidth 3).
  (let ((tails nil) (heads nil))
    (loop for u from 0 below 3
          do (loop for v from 3 below 6
                   do (push u tails) (push v heads)))
    (is (null (sp-decompose 6 tails heads))))
  ;; Subdivide every K4 edge and attach pendants and parallel doublings: the
  ;; decorations reduce away but the K4 core must still be detected.
  (let ((k4-tails '(0 0 0 1 1 2))
        (k4-heads '(1 2 3 2 3 3))
        (num-nodes 4)
        (tails nil)
        (heads nil))
    (loop for u in k4-tails
          for v in k4-heads
          do (let ((w num-nodes))
               (incf num-nodes)
               (push u tails) (push w heads)
               (push w tails) (push v heads)))
    (dotimes (v 4)
      (let ((w num-nodes))
        (incf num-nodes)
        (dotimes (_ 2)
          (push v tails) (push w heads))))
    (is (null (sp-decompose num-nodes tails heads)))))

(test series-parallel-decomposition/random-min-degree-three-rejected
  ;; Any simple graph with min degree >= 3 contains a K4 subdivision, so
  ;; these must all be rejected. (Simplicity matters: a tripled edge has min
  ;; degree 3 but is a series-parallel theta graph.)
  (let ((*random-state* (sb-ext:seed-random-state 0))
        (*test-dribble* nil))
    (finishes
      (dotimes (_ 200)
        (let* ((n (+ 4 (random 8)))
               (edges (make-hash-table :test #'equal))
               (degree (make-array n :initial-element 0)))
          (loop while (loop for d across degree thereis (< d 3))
                do (let* ((u (random n))
                          (v (mod (+ u 1 (random (1- n))) n)))
                     (unless (or (gethash (cons u v) edges)
                                 (gethash (cons v u) edges))
                       (setf (gethash (cons u v) edges) t)
                       (incf (aref degree u))
                       (incf (aref degree v)))))
          (let ((tails nil) (heads nil))
            (loop for edge being each hash-key of edges
                  do (push (car edge) tails) (push (cdr edge) heads))
            (when (sp-decompose n tails heads)
              (error "min-degree-3 graph accepted"))))))))

(test series-parallel-decomposition/determinism
  ;; Index-based bookkeeping makes the op log a pure function of the input.
  (let ((*random-state* (sb-ext:seed-random-state 1))
        (*test-dribble* nil))
    (finishes
      (dotimes (_ 50)
        (multiple-value-bind (n tails heads) (random-gsp-graph 30 2)
          (let ((d1 (sp-decompose n tails heads))
                (d2 (sp-decompose n tails heads)))
            (unless (and (equalp (sp-decomposition-ops d1)
                                 (sp-decomposition-ops d2))
                         (equalp (sp-decomposition-tails d1)
                                 (sp-decomposition-tails d2))
                         (equalp (sp-decomposition-heads d1)
                                 (sp-decomposition-heads d2)))
              (error "decomposition is not deterministic"))))))))

(test series-parallel-decomposition/random-gsp-accepted
  (let ((*random-state* (sb-ext:seed-random-state 2))
        (*test-dribble* nil))
    (finishes
      (dotimes (_ 300)
        (let ((components (+ 1 (random 3)))
              (edges-per (+ 1 (random 14))))
          (multiple-value-bind (n tails heads)
              (random-gsp-graph edges-per components)
            (let ((d (sp-decompose n tails heads)))
              (unless d
                (error "constructed GSP instance rejected"))
              (validate-decomposition n tails heads d))))))))
