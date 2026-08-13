(defpackage :cp/multi-slope-trick
  (:use :cl)
  (:export #:mstrick #:make-mstrick
           #:mstrick-dom-min #:mstrick-dom-max
           #:mstrick-value #:mstrick-conj-value
           #:mstrick-subdiff #:mstrick-arg-subdiff
           #:mstrick-insert-segment #:mstrick-remove-segment
           #:mstrick-add-kink #:mstrick-translate
           #:mstrick-inf-conv #:mstrick-pointwise-add
           #:mstrick-inf-conv-with-rollback #:mstrick-inf-conv-rollback
           #:mstrick-pointwise-add-with-rollback #:mstrick-pointwise-add-rollback
           #:mstrick-restrict-dom-max #:mstrick-restrict-dom-max-rollback
           #:mstrick-restrict-dom-min #:mstrick-restrict-dom-min-rollback
           #:mstrick-max-affine #:mstrick-convex-hull-with-point
           #:mstrick-map-segments
           #:+negative-inf+ #:+positive-inf+)
  (:documentation
   "Provides slope trick: a convex piecewise-linear function f with compact
effective domain, stored as (DOM-MIN, ANCHOR-VALUE, MIN-SLOPE, SEGMENTS), where
SEGMENTS is a treap holding the linear segments of f as slope/width pairs
(a_1, w_1), ..., (a_n, w_n) with a_1 < ... < a_n and every w_i > 0, laid out
rightward from the anchor vertex (DOM-MIN, ANCHOR-VALUE). Writing
b_j = DOM-MIN + w_1 + ... + w_j:

- dom f = [b_0, b_n] (the singleton {DOM-MIN} when n = 0);
- f has slope a_j on [b_{j-1}, b_j] and f(b_0) = ANCHOR-VALUE.

Conjugate view: f* is a finite convex piecewise-linear function on the whole
axis -- the classic slope-trick function --

  f*(p) = DOM-MIN*p - ANCHOR-VALUE + sum_i w_i * max(0, p - a_i),

whose breakpoints are the a_i (with slope increment w_i) and whose slopes range
over [b_0, b_n]. Every operation below documents both readings.

All scalars are fixnums and all comparisons are exact. Most operations are
expected O(log n); INF-CONV and POINTWISE-ADD are bulk treap unions with
expected O(m log(n/m + 1)) for operand sizes m <= n; MAX-AFFINE and
CONVEX-HULL-WITH-POINT are discrete (integer-grid) envelope operations.

The destructive binary operations have journaled -WITH-ROLLBACK variants whose
tokens restore both operands exactly (same undo cost as the forward call);
tokens must be consumed strictly LIFO across nested calls."))
(in-package :cp/multi-slope-trick)

(defconstant +negative-inf+ most-negative-fixnum)
(defconstant +positive-inf+ most-positive-fixnum)

(defmacro the+ (type &rest exprs)
  (assert (cdr exprs))
  (labels ((recur (exprs)
             (if (cdr exprs)
                 (let ((tmp (gensym)))
                   `(let ((,tmp (+ (the ,type ,(first exprs))
                                   (the ,type ,(second exprs)))))
                      (declare (,type ,tmp))
                      ,(recur `(,tmp ,@(cddr exprs)))))
                 `(the ,type ,(car exprs)))))
    (recur exprs)))

(defmacro the* (type expr)
  (labels ((recur (expr)
             (if (listp expr)
                 `(,(car expr)
                   ,@(loop for elem in (cdr expr)
                           collect `(the ,type ,(recur elem))))
                 expr)))
    (recur expr)))

;; Each node is one linear segment of f (equivalently one kink of the
;; conjugate f*):
;; - WIDTH: the segment's horizontal length. (Conjugate view: the slope
;;   increment at the kink.)
;; - SLOPE-GAP: the slope increment from the in-order predecessor -- the
;;   node's key on the slope axis. The leftmost in-order node of a standalone
;;   tree has SLOPE-GAP = 0. (Conjugate view: the horizontal gap between
;;   adjacent breakpoints of f*.)
;; Per-subtree aggregates:
;; - WIDTH-SUM = sum of WIDTH.
;; - SLOPE-GAP-SUM = sum of SLOPE-GAP.
;; - BREGMAN = sum_{i<j} WIDTH_i * SLOPE-GAP_j; shift-invariant, which is what
;;   makes the relative encoding lazy-free.
(defstruct (node (:constructor make-node
                     (width slope-gap priority
                      &aux (width-sum width) (slope-gap-sum slope-gap)))
                 (:conc-name %node-)
                 (:copier nil)
                 (:predicate nil))
  (width 1 :type (integer 1 #.most-positive-fixnum))
  (slope-gap 0 :type fixnum)
  (width-sum 1 :type (integer 1 #.most-positive-fixnum))
  (slope-gap-sum 0 :type fixnum)
  (bregman 0 :type fixnum)
  (priority 0 :type (integer 0 #.most-positive-fixnum))
  (left nil :type (or null node))
  (right nil :type (or null node)))

(declaim (inline random-priority))
(defun random-priority ()
  (random (1+ most-positive-fixnum)))

(declaim (inline node-width-sum node-slope-gap-sum node-bregman))
(defun node-width-sum (node)
  (declare ((or null node) node))
  (if node (%node-width-sum node) 0))
(defun node-slope-gap-sum (node)
  (declare ((or null node) node))
  (if node (%node-slope-gap-sum node) 0))
(defun node-bregman (node)
  (declare ((or null node) node))
  (if node (%node-bregman node) 0))

(declaim (inline pull-up))
(defun pull-up (node)
  "Refreshes the per-subtree aggregates from the children's aggregates."
  (declare (node node))
  (let* ((left (%node-left node))
         (right (%node-right node))
         (ls (node-width-sum left))
         (lx (node-slope-gap-sum left))
         (rx (node-slope-gap-sum right)))
    (setf (%node-width-sum node)
          (the+ fixnum ls (%node-width node) (node-width-sum right))
          (%node-slope-gap-sum node)
          (the+ fixnum lx (%node-slope-gap node) rx)
          (%node-bregman node)
          (the+ fixnum
                (node-bregman left)
                (* ls (%node-slope-gap node))
                (node-bregman right)
                (* (the+ fixnum ls (%node-width node)) rx)))))

;; BREGMAN = sum_{i<j} WIDTH_i * SLOPE-GAP_j has no term indexed by
;; j = leftmost, so mutating the leftmost SLOPE-GAP leaves every ancestor's
;; BREGMAN untouched; only SLOPE-GAP-SUM climbs the spine. Mirror argument for
;; the rightmost WIDTH: no j is in-order after it, so only WIDTH-SUM climbs.

(declaim (ftype (function * (values node &optional)) set-leftmost-slope-gap))
(defun set-leftmost-slope-gap (node new-slope-gap)
  "Sets the leftmost in-order node's SLOPE-GAP to NEW-SLOPE-GAP."
  (declare (optimize (speed 3))
           (node node)
           (fixnum new-slope-gap))
  (labels ((recur (node)
             (let* ((left (%node-left node))
                    (delta (if left
                               (recur left)
                               (prog1 (the fixnum (- new-slope-gap (%node-slope-gap node)))
                                 (setf (%node-slope-gap node) new-slope-gap)))))
               (declare (fixnum delta))
               (incf (%node-slope-gap-sum node) delta)
               delta)))
    (recur node)
    node))

(declaim (ftype (function * (values node &optional)) add-to-rightmost-width))
(defun add-to-rightmost-width (node extra)
  "Adds EXTRA (possibly negative) to the rightmost in-order node's WIDTH."
  (declare (optimize (speed 3))
           (node node)
           (fixnum extra))
  (labels ((recur (node)
             (let ((right (%node-right node)))
               (if right
                   (recur right)
                   (incf (%node-width node) extra)))
             (incf (%node-width-sum node) extra)))
    (recur node)
    node))

(declaim (ftype (function * (values node &optional)) add-to-leftmost-slope-gap))
(defun add-to-leftmost-slope-gap (node extra)
  "Adds EXTRA to the leftmost in-order node's SLOPE-GAP."
  (declare (optimize (speed 3))
           (node node)
           (fixnum extra))
  (labels ((recur (node)
             (let ((left (%node-left node)))
               (if left
                   (recur left)
                   (incf (%node-slope-gap node) extra)))
             (incf (%node-slope-gap-sum node) extra)))
    (recur node)
    node))

(declaim (ftype (function * (values fixnum node &optional)) take-leftmost-slope-gap))
(defun take-leftmost-slope-gap (node)
  "Takes the leftmost in-order node's SLOPE-GAP, setting it to zero. Returns
\(values gap node)."
  (declare (optimize (speed 3))
           (node node))
  (labels ((recur (node)
             (let* ((left (%node-left node))
                    (gap (if left
                             (recur left)
                             (prog1 (%node-slope-gap node)
                               (setf (%node-slope-gap node) 0)))))
               (declare (fixnum gap))
               (decf (%node-slope-gap-sum node) gap)
               gap)))
    (let ((gap (recur node)))
      (values gap node))))

(declaim (ftype (function * (values fixnum &optional)) leftmost-width))
(defun leftmost-width (node)
  (declare (optimize (speed 3))
           (node node))
  (loop while (%node-left node)
        do (setq node (%node-left node)))
  (%node-width node))

(declaim (inline link-rise))
(defun link-rise (node pred-slope)
  "Returns the rise (integral of the derivative) over all of NODE's segments,
where PRED-SLOPE is the absolute slope of the run's in-order predecessor
\(MIN-SLOPE for a standalone tree). O(1) via the aggregate identity
sum w_i*a_i = PRED-SLOPE*WIDTH-SUM + SLOPE-GAP-SUM*WIDTH-SUM - BREGMAN."
  (declare ((or null node) node)
           (fixnum pred-slope))
  (let ((ws (node-width-sum node)))
    (the fixnum
         (- (the+ fixnum (* pred-slope ws) (* (node-slope-gap-sum node) ws))
            (node-bregman node)))))

(declaim (ftype (function * (values fixnum &optional)) width-sum-lt width-sum-le))
(defun width-sum-lt (node slope min-slope)
  "Returns the cumulative WIDTH over segments with slope strictly less than
SLOPE."
  (declare (optimize (speed 3))
           ((or null node) node)
           (fixnum slope min-slope))
  (let ((acc 0)
        (pred-slope min-slope))
    (declare (fixnum acc pred-slope))
    (loop while node
          do (let* ((left (%node-left node))
                    (node-slope (the+ fixnum pred-slope (node-slope-gap-sum left)
                                      (%node-slope-gap node))))
               (if (< node-slope slope)
                   (setq acc (the+ fixnum acc (node-width-sum left) (%node-width node))
                         pred-slope node-slope
                         node (%node-right node))
                   (setq node left))))
    acc))

(defun width-sum-le (node slope min-slope)
  "Returns the cumulative WIDTH over segments with slope less than or equal to
SLOPE."
  (declare (optimize (speed 3))
           ((or null node) node)
           (fixnum slope min-slope))
  (let ((acc 0)
        (pred-slope min-slope))
    (declare (fixnum acc pred-slope))
    (loop while node
          do (let* ((left (%node-left node))
                    (node-slope (the+ fixnum pred-slope (node-slope-gap-sum left)
                                      (%node-slope-gap node))))
               (if (< slope node-slope)
                   (setq node left)
                   (setq acc (the+ fixnum acc (node-width-sum left) (%node-width node))
                         pred-slope node-slope
                         node (%node-right node)))))
    acc))

(declaim (ftype (function * (values fixnum &optional))
                slope-at-width-idx slope-before-width-idx))
(defun slope-at-width-idx (node idx min-slope)
  "Returns the slope of the segment containing cumulative width IDX
\(0 <= IDX < WIDTH-SUM)."
  (declare (optimize (speed 3))
           ((or null node) node)
           (fixnum idx min-slope))
  (let ((pred-slope min-slope))
    (declare (fixnum pred-slope))
    (loop
      (unless node
        (error "slope-at-width-idx: index out of bounds"))
      (let* ((left (%node-left node))
             (ls (node-width-sum left))
             (node-slope (the+ fixnum pred-slope (node-slope-gap-sum left)
                               (%node-slope-gap node))))
        (cond ((< idx ls)
               (setq node left))
              ((< idx (the+ fixnum ls (%node-width node)))
               (return node-slope))
              (t
               (setq idx (the fixnum (- idx ls (%node-width node)))
                     pred-slope node-slope
                     node (%node-right node))))))))

(defun slope-before-width-idx (node idx min-slope)
  "Returns the slope of the segment just before cumulative width IDX
\(0 < IDX <= WIDTH-SUM)."
  (declare (optimize (speed 3))
           ((or null node) node)
           (fixnum idx min-slope))
  (let ((pred-slope min-slope))
    (declare (fixnum pred-slope))
    (loop
      (unless node
        (error "slope-before-width-idx: index out of bounds"))
      (let* ((left (%node-left node))
             (ls (node-width-sum left))
             (node-slope (the+ fixnum pred-slope (node-slope-gap-sum left)
                               (%node-slope-gap node))))
        (cond ((<= idx ls)
               (setq node left))
              ((<= idx (the+ fixnum ls (%node-width node)))
               (return node-slope))
              (t
               (setq idx (the fixnum (- idx ls (%node-width node)))
                     pred-slope node-slope
                     node (%node-right node))))))))

(declaim (ftype (function * (values fixnum &optional)) rise-up-to-width-idx))
(defun rise-up-to-width-idx (node idx min-slope)
  "Returns the rise over [0, IDX], measured in cumulative width from the run's
left end. Requires a non-empty tree and 0 <= IDX <= WIDTH-SUM."
  (declare (optimize (speed 3))
           ((or null node) node)
           (fixnum idx min-slope))
  (let ((acc 0)
        (pred-slope min-slope))
    (declare (fixnum acc pred-slope))
    (loop
      (unless node
        (error "rise-up-to-width-idx: index out of bounds"))
      (let* ((left (%node-left node))
             (ls (node-width-sum left))
             (node-slope (the+ fixnum pred-slope (node-slope-gap-sum left)
                               (%node-slope-gap node))))
        (cond ((< idx ls)
               (setq node left))
              ((<= idx (the+ fixnum ls (%node-width node)))
               (return (the+ fixnum acc (link-rise left pred-slope)
                             (* node-slope (the fixnum (- idx ls))))))
              (t
               (setq acc (the+ fixnum acc (link-rise left pred-slope)
                               (* node-slope (%node-width node)))
                     idx (the fixnum (- idx ls (%node-width node)))
                     pred-slope node-slope
                     node (%node-right node))))))))

(declaim (ftype (function * (values fixnum &optional)) conj-value-fold))
(defun conj-value-fold (node p min-slope dom-min anchor-value)
  "Evaluates the conjugate f*(p): the maximum of the Fenchel lines of the graph
vertices. The trunk DOM-MIN*p - ANCHOR-VALUE is the anchor vertex's line; each
kink at a slope <= p switches to the next vertex's line, folded from the
prefix aggregates."
  (declare (optimize (speed 3))
           ((or null node) node)
           (fixnum p min-slope dom-min anchor-value))
  (let ((trunk (the fixnum (- (the fixnum (* dom-min p)) anchor-value)))
        (acc-w 0)
        (acc-breg 0)
        (last-slope min-slope)
        (pred-slope min-slope)
        (visited nil))
    (declare (fixnum acc-w acc-breg last-slope pred-slope))
    (loop while node
          do (let* ((left (%node-left node))
                    (node-slope (the+ fixnum pred-slope (node-slope-gap-sum left)
                                      (%node-slope-gap node))))
               (if (< p node-slope)
                   (setq node left)
                   (let* ((new-breg (the+ fixnum acc-breg (node-bregman left)
                                          (* acc-w (node-slope-gap-sum left))))
                          (new-w (the+ fixnum acc-w (node-width-sum left))))
                     (setq acc-breg (the+ fixnum new-breg
                                          (* new-w (%node-slope-gap node)))
                           acc-w (the+ fixnum new-w (%node-width node))
                           last-slope node-slope
                           visited t
                           pred-slope node-slope
                           node (%node-right node))))))
    (if visited
        (the+ fixnum trunk acc-breg (* acc-w (the fixnum (- p last-slope))))
        trunk)))

;; The descent helpers below carry two running quantities over the visited
;; prefix: CUM-W = sum of WIDTH and CUM-SP = sum of WIDTH * slope. A whole
;; left subtree L is incorporated in one step via
;; sum_{i in L} w_i*a_i = PRED-SLOPE*WIDTH-SUM(L) + SLOPE-GAP-SUM(L)*WIDTH-SUM(L) - BREGMAN(L).

(declaim (ftype (function * (values (or null fixnum) fixnum fixnum &optional))
                conj-intercept-leftmost conj-intercept-rightmost))
(defun conj-intercept-leftmost (node x y dom-min anchor-value min-slope)
  "Returns the leftmost conjugate vertex of f* lying on or below the cut line
p |-> x*p - y, searched over the vertices whose left-adjacent conjugate
segment has slope < X. Returns (values key fstar adj-slope), where KEY is the
vertex's breakpoint slope, FSTAR = f*(KEY), and ADJ-SLOPE is the slope of the
conjugate segment left-adjacent to the vertex; KEY is NIL when no such vertex
exists."
  (declare (optimize (speed 3))
           ((or null node) node)
           (fixnum x y dom-min anchor-value min-slope))
  (let ((pred-slope min-slope)
        (cum-w 0)
        (cum-sp 0)
        (best-key nil)
        (best-fstar 0)
        (best-adj 0))
    (declare (fixnum pred-slope cum-w cum-sp best-fstar best-adj))
    (loop while node
          do (let* ((left (%node-left node))
                    (ls (node-width-sum left))
                    (lx (node-slope-gap-sum left))
                    (key (the+ fixnum pred-slope lx (%node-slope-gap node)))
                    (pre-w (the+ fixnum cum-w ls))
                    (pre-sp (the+ fixnum cum-sp
                                  (the fixnum
                                       (- (the+ fixnum (* pred-slope ls) (* lx ls))
                                          (node-bregman left)))))
                    (left-conj-slope (the+ fixnum dom-min pre-w)))
               (if (<= x left-conj-slope)
                   ;; The vertex sits in the non-decreasing region; so does
                   ;; everything in-order after it.
                   (setq node left)
                   (let ((fstar (the fixnum
                                     (- (the+ fixnum (* dom-min key) (* pre-w key))
                                        (the+ fixnum anchor-value pre-sp)))))
                     (if (<= fstar (the fixnum (- (the fixnum (* x key)) y)))
                         (setq best-key key
                               best-fstar fstar
                               best-adj left-conj-slope
                               node left)
                         (setq pred-slope key
                               cum-w (the+ fixnum pre-w (%node-width node))
                               cum-sp (the+ fixnum pre-sp (* (%node-width node) key))
                               node (%node-right node)))))))
    (values best-key best-fstar best-adj)))

(defun conj-intercept-rightmost (node x y dom-min anchor-value min-slope)
  "Mirror of CONJ-INTERCEPT-LEFTMOST: the rightmost conjugate vertex on or
below the cut line, searched over the vertices whose right-adjacent conjugate
segment has slope > X; ADJ-SLOPE is the right-adjacent conjugate slope."
  (declare (optimize (speed 3))
           ((or null node) node)
           (fixnum x y dom-min anchor-value min-slope))
  (let ((pred-slope min-slope)
        (cum-w 0)
        (cum-sp 0)
        (best-key nil)
        (best-fstar 0)
        (best-adj 0))
    (declare (fixnum pred-slope cum-w cum-sp best-fstar best-adj))
    (loop while node
          do (let* ((left (%node-left node))
                    (ls (node-width-sum left))
                    (lx (node-slope-gap-sum left))
                    (key (the+ fixnum pred-slope lx (%node-slope-gap node)))
                    (pre-w (the+ fixnum cum-w ls))
                    (pre-sp (the+ fixnum cum-sp
                                  (the fixnum
                                       (- (the+ fixnum (* pred-slope ls) (* lx ls))
                                          (node-bregman left)))))
                    (full-w (the+ fixnum pre-w (%node-width node)))
                    (full-sp (the+ fixnum pre-sp (* (%node-width node) key)))
                    (right-conj-slope (the+ fixnum dom-min full-w)))
               (if (<= right-conj-slope x)
                   ;; The vertex sits in the non-increasing region; so does
                   ;; everything in-order before it.
                   (setq pred-slope key
                         cum-w full-w
                         cum-sp full-sp
                         node (%node-right node))
                   (let ((fstar (the fixnum
                                     (- (the+ fixnum (* dom-min key) (* pre-w key))
                                        (the+ fixnum anchor-value pre-sp)))))
                     (if (<= fstar (the fixnum (- (the fixnum (* x key)) y)))
                         (setq best-key key
                               best-fstar fstar
                               best-adj right-conj-slope
                               pred-slope key
                               cum-w full-w
                               cum-sp full-sp
                               node (%node-right node))
                         (setq node left))))))
    (values best-key best-fstar best-adj)))

(declaim (ftype (function * (values fixnum &optional))
                intercept-leftmost intercept-rightmost))
(defun intercept-leftmost (node anchor-value dom-min b threshold min-slope)
  "Returns the slope of the leftmost segment of f crossed by the cut line
y = THRESHOLD*x - B, detected vertex by vertex: the cut line sits at or above
the graph point (b_j, f(b_j)) exactly when that vertex's Fenchel line,
evaluated at p = THRESHOLD, reaches B. Returns +NEGATIVE-INF+ when the cut
instead clears the left domain end."
  (declare (optimize (speed 3))
           ((or null node) node)
           (fixnum anchor-value dom-min b threshold min-slope))
  (let ((v-thr (the fixnum (- (the fixnum (* dom-min threshold)) anchor-value))))
    (when (<= b v-thr)
      (return-from intercept-leftmost +negative-inf+))
    (let ((pred-slope min-slope)
          (cum-w 0)
          (cum-sp 0)
          (best +negative-inf+))
      (declare (fixnum pred-slope cum-w cum-sp best))
      (loop while node
            do (let* ((left (%node-left node))
                      (ls (node-width-sum left))
                      (lx (node-slope-gap-sum left))
                      (node-slope (the+ fixnum pred-slope lx (%node-slope-gap node))))
                 (if (<= threshold node-slope)
                     (setq node left)
                     (let* ((new-cum-w (the+ fixnum cum-w ls (%node-width node)))
                            (new-cum-sp
                              (the+ fixnum cum-sp
                                    (the fixnum
                                         (- (the+ fixnum (* pred-slope ls) (* lx ls))
                                            (node-bregman left)))
                                    (* (%node-width node) node-slope)))
                            (extrap (the fixnum
                                         (- (the+ fixnum v-thr (* new-cum-w threshold))
                                            new-cum-sp))))
                       (if (<= b extrap)
                           (setq best node-slope
                                 node left)
                           (setq pred-slope node-slope
                                 cum-w new-cum-w
                                 cum-sp new-cum-sp
                                 node (%node-right node)))))))
      best)))

(defun intercept-rightmost (node anchor-value dom-min b threshold min-slope)
  "Mirror of INTERCEPT-LEFTMOST: the slope of the rightmost crossed segment,
or +POSITIVE-INF+ when the cut clears the right domain end."
  (declare (optimize (speed 3))
           ((or null node) node)
           (fixnum anchor-value dom-min b threshold min-slope))
  (let* ((v-thr (the fixnum (- (the fixnum (* dom-min threshold)) anchor-value)))
         (total-w (node-width-sum node))
         (total-sp (link-rise node min-slope))
         (end-value (the fixnum (- (the+ fixnum v-thr (* total-w threshold))
                                   total-sp))))
    (when (<= b end-value)
      (return-from intercept-rightmost +positive-inf+))
    (let ((pred-slope min-slope)
          (cum-w 0)
          (cum-sp 0)
          (best +positive-inf+))
      (declare (fixnum pred-slope cum-w cum-sp best))
      (loop while node
            do (let* ((left (%node-left node))
                      (ls (node-width-sum left))
                      (lx (node-slope-gap-sum left))
                      (node-slope (the+ fixnum pred-slope lx (%node-slope-gap node)))
                      (pre-cum-w (the+ fixnum cum-w ls))
                      (pre-cum-sp
                        (the+ fixnum cum-sp
                              (the fixnum
                                   (- (the+ fixnum (* pred-slope ls) (* lx ls))
                                      (node-bregman left))))))
                 (if (< node-slope threshold)
                     (setq pred-slope node-slope
                           cum-w (the+ fixnum pre-cum-w (%node-width node))
                           cum-sp (the+ fixnum pre-cum-sp (* (%node-width node) node-slope))
                           node (%node-right node))
                     ;; Extrapolate the affine piece just to the left of NODE
                     ;; out to THRESHOLD.
                     (let ((val (the fixnum
                                     (- (the+ fixnum v-thr (* pre-cum-w threshold))
                                        pre-cum-sp))))
                       (if (<= b val)
                           (setq best node-slope
                                 pred-slope node-slope
                                 cum-w (the+ fixnum pre-cum-w (%node-width node))
                                 cum-sp (the+ fixnum pre-cum-sp
                                              (* (%node-width node) node-slope))
                                 node (%node-right node))
                           (setq node left))))))
      best)))

(declaim (ftype (function * (values (or null fixnum) &optional))
                reanchor-after-left-split))
(defun reanchor-after-left-split (node lr-first node-slope)
  "Re-anchors a node that goes to the right part of a split after the left
recursion's right part was attached as its new left subtree, and pulls the
aggregates up. Returns the right part's first absolute slope."
  (declare (optimize (speed 3))
           (node node)
           ((or null fixnum) lr-first)
           (fixnum node-slope))
  (let ((new-left (%node-left node)))
    (prog1 (cond (new-left
                  (setf (%node-slope-gap node)
                        (the fixnum
                             (- node-slope
                                (the+ fixnum lr-first (%node-slope-gap-sum new-left)))))
                  lr-first)
                 (t
                  (setf (%node-slope-gap node) 0)
                  node-slope))
      (pull-up node))))

(declaim (ftype (function * (values (or null node) &optional)) simple-concat))
(defun simple-concat (left right)
  "Destructively concatenates respecting in-order, driven by priorities alone.
RIGHT's leftmost SLOPE-GAP must already encode the in-order gap to LEFT's
rightmost."
  (declare (optimize (speed 3))
           ((or null node) left right))
  (cond ((null left) right)
        ((null right) left)
        ((> (%node-priority left) (%node-priority right))
         (setf (%node-right left)
               (simple-concat (%node-right left) right))
         (pull-up left)
         left)
        (t
         (setf (%node-left right)
               (simple-concat left (%node-left right)))
         (pull-up right)
         right)))

(declaim (ftype (function * (values (or null node) (or null node) (or null fixnum)
                                    &optional))
                split-by-slope split-by-width-idx))
(defun split-by-slope (node slope min-slope)
  "Destructively splits by absolute slope: the left part contains segments with
slope < SLOPE, the right part the rest. Both halves are standalone (leftmost
SLOPE-GAP = 0). Returns (values left right right-first-slope), where
RIGHT-FIRST-SLOPE is the first absolute slope of the right part or NIL."
  (declare (optimize (speed 3))
           ((or null node) node)
           (fixnum slope min-slope))
  (if (null node)
      (values nil nil nil)
      (let* ((left (%node-left node))
             (node-slope (the+ fixnum min-slope (node-slope-gap-sum left)
                               (%node-slope-gap node))))
        (if (< node-slope slope)
            (multiple-value-bind (rl rr rr-first)
                (split-by-slope (%node-right node) slope node-slope)
              (setf (%node-right node) rl)
              (pull-up node)
              (values node rr rr-first))
            (multiple-value-bind (ll lr lr-first)
                (split-by-slope left slope min-slope)
              (setf (%node-left node) lr)
              (values ll node (reanchor-after-left-split node lr-first node-slope)))))))

(defun split-by-width-idx (node idx min-slope)
  "Destructively splits by cumulative WIDTH: the left part contains the
segments whose cumulative WIDTH is <= IDX, the right part the rest. If IDX
falls strictly inside a segment, it is split in two pieces at the same
absolute slope (the right piece's SLOPE-GAP = 0). Returns
\(values left right right-first-slope)."
  (declare (optimize (speed 3))
           ((or null node) node)
           (fixnum idx min-slope))
  (if (null node)
      (values nil nil nil)
      (let* ((left (%node-left node))
             (ls (node-width-sum left))
             (node-slope (the+ fixnum min-slope (node-slope-gap-sum left)
                               (%node-slope-gap node)))
             (end-b (the+ fixnum ls (%node-width node))))
        (cond ((<= end-b idx)
               (multiple-value-bind (rl rr rr-first)
                   (split-by-width-idx (%node-right node) (- idx end-b) node-slope)
                 (setf (%node-right node) rl)
                 (pull-up node)
                 (values node rr rr-first)))
              ((<= idx ls)
               (multiple-value-bind (ll lr lr-first)
                   (split-by-width-idx left idx min-slope)
                 (setf (%node-left node) nil
                       (%node-slope-gap node)
                       (if lr
                           (the fixnum
                                (- node-slope
                                   (the+ fixnum lr-first (%node-slope-gap-sum lr))))
                           0))
                 (pull-up node)
                 ;; Priority-aware reattachment: LR may contain the
                 ;; fresh-priority fragment of a deeper inside cut that
                 ;; outranks NODE.
                 (values ll (simple-concat lr node) (or lr-first node-slope))))
              (t
               ;; Split inside this node's width. NODE becomes the left half;
               ;; the right half starts with a fresh leaf at the same absolute
               ;; slope (SLOPE-GAP = 0) carrying the remaining width, joined
               ;; to the original right child by priorities. The leaf draws a
               ;; fresh priority; copying NODE's would let repeated cuts into
               ;; one wide segment accumulate an equal-priority run that
               ;; SIMPLE-CONCAT arranges as a chain, destroying treap balance.
               (let* ((within (- idx ls))
                      (remainder (- (%node-width node) within))
                      (right-child (%node-right node))
                      (rleaf (make-node remainder 0 (random-priority))))
                 (setf (%node-width node) within
                       (%node-right node) nil)
                 (pull-up node)
                 (values node (simple-concat rleaf right-child) node-slope)))))))

(declaim (ftype (function * (values (or null node) fixnum &optional)) remove-leftmost))
(defun remove-leftmost (node pred-slope)
  "Destructively removes the leftmost in-order node. Returns
\(values new-root new-min-slope); NEW-MIN-SLOPE is the slope of the new
leftmost (or PRED-SLOPE if the result is empty)."
  (declare (optimize (speed 3))
           (node node)
           (fixnum pred-slope))
  (let* ((left (%node-left node))
         (node-slope (the+ fixnum pred-slope (node-slope-gap-sum left)
                           (%node-slope-gap node))))
    (if left
        (multiple-value-bind (new-left new-anchor) (remove-leftmost left pred-slope)
          (cond (new-left
                 ;; Re-anchor NODE's SLOPE-GAP to the rightmost of the new
                 ;; left subtree.
                 (setf (%node-left node) new-left
                       (%node-slope-gap node)
                       (the fixnum
                            (- node-slope
                               (the+ fixnum new-anchor (%node-slope-gap-sum new-left)))))
                 (pull-up node)
                 (values node new-anchor))
                (t
                 ;; The left subtree is gone; NODE becomes the new leftmost at
                 ;; an unchanged absolute slope.
                 (setf (%node-left node) nil
                       (%node-slope-gap node) 0)
                 (pull-up node)
                 (values node node-slope))))
        (let ((right (%node-right node)))
          (if right
              (multiple-value-bind (gap new-root) (take-leftmost-slope-gap right)
                (values new-root (the+ fixnum pred-slope gap)))
              (values nil pred-slope))))))

(declaim (ftype (function * (values (or null node) fixnum &optional)) union-by-slope))
(defun union-by-slope (a b a-min-slope b-min-slope)
  "Destructively unions two standalone trees keyed by absolute slope: the
result holds both trees' kinks, and WIDTHs add when the same slope occurs in
both trees. Consumes both inputs and reuses their node priorities. Returns
\(values root min-slope); the returned min-slope is meaningful only for a
non-empty result. Expected O(m log(n/m + 1)) for input sizes m <= n."
  (declare (optimize (speed 3))
           ((or null node) a b)
           (fixnum a-min-slope b-min-slope))
  (cond ((null a) (values b b-min-slope))
        ((null b) (values a a-min-slope))
        (t
         (let (r other r-anchor o-anchor)
           (if (>= (%node-priority a) (%node-priority b))
               (setq r a other b r-anchor a-min-slope o-anchor b-min-slope)
               (setq r b other a r-anchor b-min-slope o-anchor a-min-slope))
           (locally (declare (node r other) (fixnum r-anchor o-anchor))
             (let ((r-slope (the+ fixnum r-anchor
                                  (node-slope-gap-sum (%node-left r))
                                  (%node-slope-gap r))))
               (multiple-value-bind (o-less o-geq o-geq-first)
                   (split-by-slope other r-slope o-anchor)
                 (let ((o-geq-anchor r-slope))
                   (declare (fixnum o-geq-anchor))
                   ;; Fuse the >= half's head into the root on an equal slope.
                   (when o-geq-first
                     (if (= (the fixnum o-geq-first) r-slope)
                         (progn
                           (incf (%node-width r) (leftmost-width o-geq))
                           (multiple-value-bind (rest rest-anchor)
                               (remove-leftmost o-geq o-geq-first)
                             (setq o-geq rest
                                   o-geq-anchor rest-anchor)))
                         (setq o-geq-anchor o-geq-first)))
                   ;; Detach the root's subtrees as standalone trees.
                   (let ((r-left (%node-left r))
                         (r-right nil)
                         (r-right-anchor r-slope))
                     (declare (fixnum r-right-anchor))
                     (let ((rr (%node-right r)))
                       (when rr
                         (multiple-value-bind (gap rr2) (take-leftmost-slope-gap rr)
                           (setq r-right rr2
                                 r-right-anchor (the+ fixnum r-slope gap)))))
                     (multiple-value-bind (left left-anchor)
                         (union-by-slope r-left o-less r-anchor o-anchor)
                       (multiple-value-bind (right right-anchor)
                           (union-by-slope r-right o-geq r-right-anchor o-geq-anchor)
                         ;; Reattach under R, re-anchoring its SLOPE-GAP
                         ;; against the merged left half's rightmost slope.
                         (setf (%node-slope-gap r)
                               (if left
                                   (the fixnum
                                        (- r-slope
                                           (the+ fixnum left-anchor
                                                 (%node-slope-gap-sum left))))
                                   0)
                               (%node-left r) left
                               (%node-right r)
                               (and right
                                    (set-leftmost-slope-gap
                                     right (- right-anchor r-slope))))
                         (pull-up r)
                         (values r (if left left-anchor r-slope)))))))))))))

(declaim (ftype (function * (values (or null node) (or null node) &optional))
                split-by-width-intrinsic))
(defun split-by-width-intrinsic (node idx)
  "Splits by cumulative WIDTH in the mass-preserving reading: each node is a
slope jump at its left edge followed by a run of WIDTH, and the split cuts the
width axis at IDX leaving every SLOPE-GAP untouched. Unlike
SPLIT-BY-WIDTH-IDX, the right part is NOT re-anchored to standalone form --
its head keeps the jump at its left edge (zero when the cut falls strictly
inside a node's width)."
  (declare (optimize (speed 3))
           ((or null node) node)
           (fixnum idx))
  (if (null node)
      (values nil nil)
      (let* ((left (%node-left node))
             (ls (node-width-sum left))
             (end-b (the+ fixnum ls (%node-width node))))
        (cond ((<= end-b idx)
               (let ((r (%node-right node)))
                 (setf (%node-right node) nil)
                 (pull-up node)
                 (multiple-value-bind (rl rr) (split-by-width-intrinsic r (- idx end-b))
                   ;; Priority-aware reattachment: RL may contain a
                   ;; fresh-priority fragment from a deeper inside cut that
                   ;; outranks NODE.
                   (values (simple-concat node rl) rr))))
              ((<= idx ls)
               (setf (%node-left node) nil)
               (pull-up node)
               (multiple-value-bind (ll lr) (split-by-width-intrinsic left idx)
                 (values ll (simple-concat lr node))))
              (t
               ;; Cut strictly inside this node's width, with zero jump on the
               ;; right fragment. The left fragment keeps the node's priority;
               ;; the right fragment is a fresh leaf with a fresh priority
               ;; (copying the priority instead would let cascaded unions cut
               ;; one node into an equal-priority run that SIMPLE-CONCAT
               ;; arranges as a chain, destroying treap balance).
               (let* ((within (- idx ls))
                      (remainder (- (%node-width node) within))
                      (right-child (%node-right node))
                      (rleaf (make-node remainder 0 (random-priority))))
                 (setf (%node-width node) within
                       (%node-right node) nil)
                 (pull-up node)
                 (values node (simple-concat rleaf right-child))))))))

(declaim (ftype (function * (values (or null node) &optional)) union-by-width))
(defun union-by-width (a b)
  "Destructively merges two trees covering the same total WIDTH into the common
refinement of the two partitions they describe: the result has a node boundary
wherever either input does, node widths partition accordingly, and SLOPE-GAPs
add where boundaries of the two inputs coincide. Slope positions play no role:
a SLOPE-GAP is the jump at the node's left edge, an intrinsic mass invariant
under interleaving. Expected O(m log(n/m + 1)) for input sizes m <= n."
  (declare (optimize (speed 3))
           ((or null node) a b))
  (cond ((null a) b)
        ((null b) a)
        ;; One side is a single segment: nothing to refine; fuse its jump into
        ;; the other's head.
        ((and (null (%node-left a)) (null (%node-right a)))
         (add-to-leftmost-slope-gap b (%node-slope-gap a)))
        ((and (null (%node-left b)) (null (%node-right b)))
         (add-to-leftmost-slope-gap a (%node-slope-gap b)))
        (t
         ;; The larger-priority root R dissolves: the other tree's slice over
         ;; R's width span already is the refinement of R's segment and
         ;; inherits R's jump on its head, while R's subtrees union with the
         ;; outer slices.
         (let (r other)
           (if (>= (%node-priority a) (%node-priority b))
               (setq r a other b)
               (setq r b other a))
           (locally (declare (node r other))
             (let ((ls (node-width-sum (%node-left r)))
                   (r-left (%node-left r))
                   (r-right (%node-right r)))
               (multiple-value-bind (o-left o-rest) (split-by-width-intrinsic other ls)
                 (multiple-value-bind (o-mid o-right)
                     (split-by-width-intrinsic o-rest (%node-width r))
                   (let ((left (union-by-width r-left o-left))
                         (right (union-by-width r-right o-right)))
                     (unless o-mid
                       (error "union-by-width: total widths of the operands differ"))
                     (simple-concat
                      left
                      (simple-concat
                       (add-to-leftmost-slope-gap o-mid (%node-slope-gap r))
                       right)))))))))))

(declaim (ftype (function * (values (or null node) &optional)) concat))
(defun concat (left right left-anchor right-anchor)
  "Destructively concatenates two standalone trees in in-order. LEFT-ANCHOR and
RIGHT-ANCHOR are the min-slope of each side; every slope in LEFT must be <=
every slope in RIGHT (not validated). If LEFT's rightmost slope coincides with
RIGHT's leftmost, the two WIDTHs are summed."
  (declare (optimize (speed 3))
           ((or null node) left right)
           (fixnum left-anchor right-anchor))
  (cond ((null left) right)
        ((null right) left)
        (t
         (let ((left-last (the+ fixnum left-anchor (%node-slope-gap-sum left))))
           (if (= left-last right-anchor)
               ;; Boundary slopes equal: merge into the rightmost of LEFT.
               (let ((right-lm-width (leftmost-width right)))
                 (setq left (add-to-rightmost-width left right-lm-width))
                 (multiple-value-bind (right-rest new-right-anchor)
                     (remove-leftmost right right-anchor)
                   (if right-rest
                       (simple-concat
                        left
                        (set-leftmost-slope-gap right-rest
                                                (- new-right-anchor left-last)))
                       left)))
               (simple-concat
                left
                (set-leftmost-slope-gap right (- right-anchor left-last))))))))

(declaim (ftype (function * (values (or null node) fixnum &optional)) insert))
(defun insert (node slope width min-slope)
  "Destructively inserts a kink at absolute slope SLOPE with slope-increment
WIDTH >= 0 (a zero WIDTH is a no-op). If SLOPE matches an existing kink, the
corresponding WIDTH is incremented. Returns (values new-root new-min-slope).

Single-pass: a random priority is drawn once, then one descent either
increments a matching kink in place, or, at the highest level where the new
priority outranks the current node, splits that subtree by SLOPE and places
the new node there."
  (declare (optimize (speed 3))
           ((or null node) node)
           (fixnum slope width min-slope))
  (when (zerop width)
    (return-from insert (values node min-slope)))
  (let ((new-priority (random-priority)))
    (cond ((null node)
           (values (make-node width 0 new-priority) slope))
          ((< slope min-slope)
           ;; New leftmost: prepend a fresh leaf and re-gap the old leftmost.
           (values (simple-concat (make-node width 0 new-priority)
                                  (set-leftmost-slope-gap node (- min-slope slope)))
                   slope))
          (t
           (values (insert-inner node slope width new-priority nil min-slope)
                   min-slope)))))

(defun insert-inner (node slope width new-priority found pred-slope)
  "Returns (values subtree changed-p). CHANGED-P is true iff an existing kink
matched and its WIDTH was incremented, or the new node was placed at this
level or below; the caller then re-anchors and pulls up. FOUND is true once
the descent has passed the priority-transition level."
  (declare (optimize (speed 3))
           ((or null node) node)
           (fixnum slope width pred-slope)
           ((integer 0 #.most-positive-fixnum) new-priority))
  (if (null node)
      (if found
          (values nil nil)
          ;; Hitting an empty slot below the transition level: drop the new
          ;; leaf in here.
          (values (make-node width (- slope pred-slope) new-priority) t))
      (let* ((left (%node-left node))
             (lx (node-slope-gap-sum left))
             (node-slope (the+ fixnum pred-slope lx (%node-slope-gap node)))
             (new-found (or found (> new-priority (%node-priority node))))
             (transition-here (and (not found) new-found)))
        (cond ((< slope node-slope)
               (multiple-value-bind (new-left changed)
                   (insert-inner left slope width new-priority new-found pred-slope)
                 (cond (changed
                        ;; The leftmost descendant of the left subtree may
                        ;; have moved; re-anchor NODE's SLOPE-GAP from the
                        ;; change in the subtree's SLOPE-GAP-SUM.
                        (setf (%node-left node) new-left
                              (%node-slope-gap node)
                              (the fixnum
                                   (- (the+ fixnum (%node-slope-gap node) lx)
                                      (node-slope-gap-sum new-left))))
                        (pull-up node)
                        (values node t))
                       (transition-here
                        (place-new-root node slope width new-priority pred-slope))
                       (t (values node nil)))))
              ((< node-slope slope)
               (multiple-value-bind (new-right changed)
                   (insert-inner (%node-right node) slope width new-priority
                                 new-found node-slope)
                 (cond (changed
                        (setf (%node-right node) new-right)
                        (pull-up node)
                        (values node t))
                       (transition-here
                        (place-new-root node slope width new-priority pred-slope))
                       (t (values node nil)))))
              (t
               (incf (%node-width node) width)
               (pull-up node)
               (values node t))))))

(defun place-new-root (subtree slope width priority pred-slope)
  "Splits SUBTREE by SLOPE and wraps the halves under a new root carrying the
new kink. Used only at the priority-transition level of INSERT-INNER."
  (declare (optimize (speed 3))
           (node subtree)
           (fixnum slope width pred-slope)
           ((integer 0 #.most-positive-fixnum) priority))
  (multiple-value-bind (left-part right-part right-first)
      (split-by-slope subtree slope pred-slope)
    (let ((new-root (make-node width
                               (if left-part
                                   (the fixnum
                                        (- slope
                                           (the+ fixnum pred-slope
                                                 (%node-slope-gap-sum left-part))))
                                   (- slope pred-slope))
                               priority)))
      (setf (%node-left new-root) left-part
            (%node-right new-root)
            (if right-first
                (set-leftmost-slope-gap right-part (- right-first slope))
                right-part))
      (pull-up new-root)
      (values new-root t))))

(declaim (ftype (function * (values (or null node) fixnum &optional)) %delete))
(defun %delete (node slope width min-slope)
  "Destructively deletes WIDTH units of slope-increment from the kink at SLOPE.
Signals an error if no kink at SLOPE holds at least WIDTH. If the matched
kink's WIDTH reaches zero the node is spliced out. Returns
\(values new-root new-min-slope). Single-pass O(log n) descent."
  (declare (optimize (speed 3))
           ((or null node) node)
           (fixnum slope width min-slope))
  (when (zerop width)
    (return-from %delete (values node min-slope)))
  (unless node
    (error "%delete: no kink at slope ~D holding width ~D" slope width))
  (let ((was-at-leftmost (= slope min-slope)))
    (multiple-value-bind (new-root outcome) (delete-inner node slope width min-slope)
      (when (eq outcome :not-found)
        (error "%delete: no kink at slope ~D holding width ~D" slope width))
      (if (or (not was-at-leftmost) (not (eq outcome :spliced)))
          (values new-root min-slope)
          ;; Removed the global leftmost: the new leftmost may carry a
          ;; non-zero SLOPE-GAP (the gap from the deleted leftmost). Absorb it
          ;; into min-slope to restore the standalone-tree invariant.
          (if (null new-root)
              (values nil 0)
              (multiple-value-bind (gap normalized) (take-leftmost-slope-gap new-root)
                (values normalized (the+ fixnum min-slope gap))))))))

(defun delete-inner (node slope width pred-slope)
  "Returns (values subtree outcome), OUTCOME being :NOT-FOUND, :DECREMENTED, or
:SPLICED. On :NOT-FOUND the subtree is returned structurally unchanged."
  (declare (optimize (speed 3))
           (node node)
           (fixnum slope width pred-slope))
  (let* ((left (%node-left node))
         (lx (node-slope-gap-sum left))
         (node-slope (the+ fixnum pred-slope lx (%node-slope-gap node))))
    (cond ((< slope node-slope)
           (if (null left)
               (values node :not-found)
               (multiple-value-bind (new-left outcome)
                   (delete-inner left slope width pred-slope)
                 (setf (%node-left node) new-left)
                 (unless (eq outcome :not-found)
                   ;; On a splice below, the left subtree's leftmost may have
                   ;; moved; re-anchor NODE's SLOPE-GAP from the change.
                   (when (eq outcome :spliced)
                     (setf (%node-slope-gap node)
                           (the fixnum
                                (- (the+ fixnum (%node-slope-gap node) lx)
                                   (node-slope-gap-sum new-left)))))
                   (pull-up node))
                 (values node outcome))))
          ((< node-slope slope)
           (let ((right (%node-right node)))
             (if (null right)
                 (values node :not-found)
                 (multiple-value-bind (new-right outcome)
                     (delete-inner right slope width node-slope)
                   (setf (%node-right node) new-right)
                   (unless (eq outcome :not-found)
                     (pull-up node))
                   (values node outcome)))))
          ((< (%node-width node) width)
           (values node :not-found))
          ((< width (%node-width node))
           (decf (%node-width node) width)
           (pull-up node)
           (values node :decremented))
          (t
           ;; Splice NODE out. The right subtree's leftmost becomes a direct
           ;; successor of NODE's in-order predecessor, so its SLOPE-GAP
           ;; absorbs NODE's.
           (let ((l (%node-left node))
                 (r (%node-right node)))
             (values (cond ((null r) l)
                           ((null l)
                            (add-to-leftmost-slope-gap r (%node-slope-gap node)))
                           (t
                            (simple-concat
                             l (add-to-leftmost-slope-gap r (%node-slope-gap node)))))
                     :spliced))))))

(defun concat-kept (keep-left keep-right original-min-slope keep-right-first rest-anchor)
  "Recombines the kept outer parts of an envelope splice: concatenates
KEEP-LEFT and KEEP-RIGHT and derives the surviving standalone anchor -- the
original anchor while KEEP-LEFT is non-empty, KEEP-RIGHT's first slope when
only it survives, 0 when both parts are empty. Returns
\(values segments min-slope)."
  (declare (optimize (speed 3))
           ((or null node) keep-left keep-right)
           (fixnum original-min-slope rest-anchor)
           ((or null fixnum) keep-right-first))
  (let* ((keep-left-p (if keep-left t nil))
         (keep-right-p (if keep-right t nil))
         (keep-right-anchor (or keep-right-first rest-anchor)))
    (values (concat keep-left keep-right original-min-slope keep-right-anchor)
            (cond (keep-left-p original-min-slope)
                  (keep-right-p keep-right-anchor)
                  (t 0)))))

;; Undo journals. A journaled operation produces the same result as its plain
;; counterpart and additionally returns a list of records -- one per recursion
;; frame, pushed in post-order and popped in exact reverse by the undo, whose
;; recursion mirrors the forward shape, so the undo cost equals the forward
;; cost. A record stores only the node fields the frame overwrote (WIDTH,
;; SLOPE-GAP) and any node the frame detached; aggregates are recomputed by
;; PULL-UP on the unwind, which reproduces them exactly since all arithmetic
;; is exact. Records are either a bare keyword or a cons headed by one.

(declaim (ftype (function * (values (or null node) fixnum list &optional))
                remove-leftmost-journaled))
(defun remove-leftmost-journaled (node pred-slope)
  "REMOVE-LEFTMOST with an undo journal: additionally returns the record list
consumed by UNDO-REMOVE-LEFTMOST."
  (declare (optimize (speed 3))
           (node node)
           (fixnum pred-slope))
  (let ((recs nil))
    (labels ((recur (node pred-slope)
               (declare (node node) (fixnum pred-slope))
               (let* ((left (%node-left node))
                      (node-slope (the+ fixnum pred-slope (node-slope-gap-sum left)
                                        (%node-slope-gap node))))
                 (if left
                     (let ((old-gap (%node-slope-gap node)))
                       (multiple-value-bind (new-left new-anchor) (recur left pred-slope)
                         (declare (fixnum new-anchor))
                         (cond (new-left
                                (setf (%node-left node) new-left
                                      (%node-slope-gap node)
                                      (the fixnum
                                           (- node-slope
                                              (the+ fixnum new-anchor
                                                    (%node-slope-gap-sum new-left)))))
                                (pull-up node)
                                (push (cons :spine old-gap) recs)
                                (values node new-anchor))
                               (t
                                (setf (%node-left node) nil
                                      (%node-slope-gap node) 0)
                                (pull-up node)
                                (push (cons :spine old-gap) recs)
                                (values node node-slope)))))
                     (let ((right (%node-right node)))
                       (setf (%node-right node) nil)
                       (if right
                           (multiple-value-bind (gap new-root)
                               (take-leftmost-slope-gap right)
                             (push (list* :removed node gap) recs)
                             (values new-root (the+ fixnum pred-slope gap)))
                           (progn
                             (push (list* :removed node nil) recs)
                             (values nil pred-slope))))))))
      (multiple-value-bind (root anchor) (recur node pred-slope)
        (values root anchor recs)))))

(declaim (ftype (function * (values node &optional)) undo-remove-leftmost))
(defun undo-remove-leftmost (cur recs)
  "Undoes a REMOVE-LEFTMOST-JOURNALED: reattaches the removed node and
restores every touched field. CUR must be the tree the removal returned."
  (declare (optimize (speed 3))
           ((or null node) cur)
           (list recs))
  (labels ((recur (cur)
             (declare ((or null node) cur))
             (let ((rec (pop recs)))
               (ecase (car rec)
                 (:spine
                  (let ((node cur))
                    (declare (node node))
                    (setf (%node-left node) (recur (%node-left node))
                          (%node-slope-gap node) (cdr rec))
                    (pull-up node)
                    node))
                 (:removed
                  (let ((node (cadr rec))
                        (right-gap (cddr rec)))
                    (declare (node node))
                    (when right-gap
                      (setf (%node-right node)
                            (set-leftmost-slope-gap cur right-gap)))
                    (pull-up node)
                    node))))))
    (recur cur)))

(declaim (ftype (function * (values (or null node) fixnum list &optional))
                union-by-slope-journaled))
(defun union-by-slope-journaled (a b a-min-slope b-min-slope)
  "UNION-BY-SLOPE with an undo journal: additionally returns the record list
consumed by UNDO-UNION-BY-SLOPE."
  (declare (optimize (speed 3))
           ((or null node) a b)
           (fixnum a-min-slope b-min-slope))
  (let ((recs nil))
    (labels
        ((split-j (node slope min-slope)
           (declare ((or null node) node)
                    (fixnum slope min-slope))
           (if (null node)
               (progn (push :s-empty recs) (values nil nil nil))
               (let* ((left (%node-left node))
                      (node-slope (the+ fixnum min-slope (node-slope-gap-sum left)
                                        (%node-slope-gap node))))
                 (if (< node-slope slope)
                     (multiple-value-bind (rl rr rr-first)
                         (split-j (%node-right node) slope node-slope)
                       (setf (%node-right node) rl)
                       (pull-up node)
                       (push :s-lt recs)
                       (values node rr rr-first))
                     (let ((old-gap (%node-slope-gap node)))
                       (multiple-value-bind (ll lr lr-first)
                           (split-j left slope min-slope)
                         (setf (%node-left node) lr)
                         (let ((rf (reanchor-after-left-split node lr-first node-slope)))
                           (push (cons :s-ge old-gap) recs)
                           (values ll node rf))))))))
         (union-j (a b a-anchor b-anchor)
           (declare ((or null node) a b)
                    (fixnum a-anchor b-anchor))
           (cond ((null a) (push :u-trivial-a recs) (values b b-anchor))
                 ((null b) (push :u-trivial-b recs) (values a a-anchor))
                 (t
                  (let (r other r-anchor o-anchor root-from-a)
                    (if (>= (%node-priority a) (%node-priority b))
                        (setq r a other b r-anchor a-anchor o-anchor b-anchor
                              root-from-a t)
                        (setq r b other a r-anchor b-anchor o-anchor a-anchor
                              root-from-a nil))
                    (locally (declare (node r other) (fixnum r-anchor o-anchor))
                      (let ((old-width (%node-width r))
                            (old-gap (%node-slope-gap r))
                            (r-slope (the+ fixnum r-anchor
                                           (node-slope-gap-sum (%node-left r))
                                           (%node-slope-gap r)))
                            (collision nil)
                            (detach-gap nil))
                        (multiple-value-bind (o-less o-geq o-geq-first)
                            (split-j other r-slope o-anchor)
                          (let ((o-geq-anchor r-slope))
                            (declare (fixnum o-geq-anchor))
                            (when o-geq-first
                              (if (= (the fixnum o-geq-first) r-slope)
                                  (progn
                                    (incf (%node-width r) (leftmost-width o-geq))
                                    (multiple-value-bind (rest rest-anchor rl-recs)
                                        (remove-leftmost-journaled o-geq o-geq-first)
                                      (setq o-geq rest
                                            o-geq-anchor rest-anchor
                                            collision rl-recs)))
                                  (setq o-geq-anchor o-geq-first)))
                            (let ((r-left (%node-left r))
                                  (r-right nil)
                                  (r-right-anchor r-slope))
                              (declare (fixnum r-right-anchor))
                              (let ((rr (%node-right r)))
                                (when rr
                                  (multiple-value-bind (gap rr2)
                                      (take-leftmost-slope-gap rr)
                                    (setq r-right rr2
                                          r-right-anchor (the+ fixnum r-slope gap)
                                          detach-gap gap))))
                              (multiple-value-bind (left left-anchor)
                                  (union-j r-left o-less r-anchor o-anchor)
                                (multiple-value-bind (right right-anchor)
                                    (union-j r-right o-geq r-right-anchor o-geq-anchor)
                                  (setf (%node-slope-gap r)
                                        (if left
                                            (the fixnum
                                                 (- r-slope
                                                    (the+ fixnum left-anchor
                                                          (%node-slope-gap-sum left))))
                                            0)
                                        (%node-left r) left
                                        (%node-right r)
                                        (and right
                                             (set-leftmost-slope-gap
                                              right (- right-anchor r-slope))))
                                  (pull-up r)
                                  (push (list :u-frame root-from-a old-width old-gap
                                              detach-gap collision)
                                        recs)
                                  (values r (if left left-anchor r-slope))))))))))))))
      (multiple-value-bind (root anchor) (union-j a b a-min-slope b-min-slope)
        (values root anchor recs)))))

(declaim (ftype (function * (values (or null node) (or null node) &optional))
                undo-union-by-slope))
(defun undo-union-by-slope (merged recs)
  "Undoes a UNION-BY-SLOPE-JOURNALED: dismantles the merged tree back into the
two operands, exactly as they were -- shape, priorities, and every field.
MERGED must be the tree the union returned. Returns (values a b)."
  (declare (optimize (speed 3))
           ((or null node) merged)
           (list recs))
  (labels ((tag (rec) (if (consp rec) (car rec) rec))
           (undo-split (left right)
             (declare ((or null node) left right))
             (let ((rec (pop recs)))
               (ecase (tag rec)
                 (:s-empty nil)
                 (:s-lt
                  (let ((node left))
                    (declare (node node))
                    (setf (%node-right node)
                          (undo-split (%node-right node) right))
                    (pull-up node)
                    node))
                 (:s-ge
                  (let ((node right))
                    (declare (node node))
                    (setf (%node-left node)
                          (undo-split left (%node-left node))
                          (%node-slope-gap node) (cdr rec))
                    (pull-up node)
                    node)))))
           (recur (merged)
             (declare ((or null node) merged))
             (let ((rec (pop recs)))
               (ecase (tag rec)
                 (:u-trivial-a (values nil merged))
                 (:u-trivial-b (values merged nil))
                 (:u-frame
                  (destructuring-bind (root-from-a old-width old-gap detach-gap collision)
                      (cdr rec)
                    (let* ((r merged)
                           (left-merged (%node-left r))
                           (right-merged (%node-right r)))
                      (declare (node r))
                      (setf (%node-left r) nil
                            (%node-right r) nil)
                      (when right-merged
                        (set-leftmost-slope-gap right-merged 0))
                      (multiple-value-bind (r-right o-geq-after) (recur right-merged)
                        (multiple-value-bind (r-left o-less) (recur left-merged)
                          (let* ((o-geq (if collision
                                            (undo-remove-leftmost o-geq-after collision)
                                            o-geq-after))
                                 (other (undo-split o-less o-geq)))
                            (setf (%node-left r) r-left
                                  (%node-right r)
                                  (when r-right
                                    (set-leftmost-slope-gap r-right detach-gap))
                                  (%node-width r) old-width
                                  (%node-slope-gap r) old-gap)
                            (pull-up r)
                            (if root-from-a
                                (values r other)
                                (values other r))))))))))))
    (recur merged)))

(declaim (ftype (function * (values (or null node) list &optional))
                union-by-width-journaled))
(defun union-by-width-journaled (a b)
  "UNION-BY-WIDTH with an undo journal: additionally returns the record list
consumed by UNDO-UNION-BY-WIDTH."
  (declare (optimize (speed 3))
           ((or null node) a b))
  (let ((recs nil))
    (labels
        ((concat-j (left right)
           (declare ((or null node) left right))
           (cond ((null left) (push :c-trivial-l recs) right)
                 ((null right) (push :c-trivial-r recs) left)
                 ((> (%node-priority left) (%node-priority right))
                  (setf (%node-right left) (concat-j (%node-right left) right))
                  (pull-up left)
                  (push :c-left recs)
                  left)
                 (t
                  (setf (%node-left right) (concat-j left (%node-left right)))
                  (pull-up right)
                  (push :c-right recs)
                  right)))
         (wsplit-j (node idx)
           (declare ((or null node) node)
                    (fixnum idx))
           (if (null node)
               (progn (push :ws-empty recs) (values nil nil))
               (let* ((left (%node-left node))
                      (ls (node-width-sum left))
                      (end-b (the+ fixnum ls (%node-width node))))
                 (cond ((<= end-b idx)
                        (let ((r (%node-right node)))
                          (setf (%node-right node) nil)
                          (pull-up node)
                          (multiple-value-bind (rl rr) (wsplit-j r (- idx end-b))
                            (let ((l (concat-j node rl)))
                              (push :ws-lt recs)
                              (values l rr)))))
                       ((<= idx ls)
                        (setf (%node-left node) nil)
                        (pull-up node)
                        (multiple-value-bind (ll lr) (wsplit-j left idx)
                          (let ((r (concat-j lr node)))
                            (push :ws-ge recs)
                            (values ll r))))
                       (t
                        (let* ((within (- idx ls))
                               (old-width (%node-width node))
                               (right-child (%node-right node))
                               (rleaf (make-node (- old-width within) 0
                                                 (random-priority))))
                          (setf (%node-width node) within
                                (%node-right node) nil)
                          (pull-up node)
                          (let ((rp (concat-j rleaf right-child)))
                            (push (cons :ws-inside old-width) recs)
                            (values node rp))))))))
         (union-j (a b)
           (declare ((or null node) a b))
           (cond ((null a) (push :w-trivial-a recs) b)
                 ((null b) (push :w-trivial-b recs) a)
                 ((and (null (%node-left a)) (null (%node-right a)))
                  (let ((merged (add-to-leftmost-slope-gap b (%node-slope-gap a))))
                    (push (list* :w-fuse t a) recs)
                    merged))
                 ((and (null (%node-left b)) (null (%node-right b)))
                  (let ((merged (add-to-leftmost-slope-gap a (%node-slope-gap b))))
                    (push (list* :w-fuse nil b) recs)
                    merged))
                 (t
                  (let (r other root-from-a)
                    (if (>= (%node-priority a) (%node-priority b))
                        (setq r a other b root-from-a t)
                        (setq r b other a root-from-a nil))
                    (locally (declare (node r other))
                      (let ((ls (node-width-sum (%node-left r)))
                            (r-left (%node-left r))
                            (r-right (%node-right r)))
                        (setf (%node-left r) nil
                              (%node-right r) nil)
                        (multiple-value-bind (o-left o-rest) (wsplit-j other ls)
                          (multiple-value-bind (o-mid o-right)
                              (wsplit-j o-rest (%node-width r))
                            (let ((left (union-j r-left o-left))
                                  (right (union-j r-right o-right)))
                              (unless o-mid
                                (error "union-by-width: total widths of the operands differ"))
                              (let ((result
                                      (concat-j
                                       left
                                       (concat-j (add-to-leftmost-slope-gap
                                                  o-mid (%node-slope-gap r))
                                                 right))))
                                (push (list* :w-frame root-from-a r) recs)
                                result)))))))))))
      (values (union-j a b) recs))))

(declaim (ftype (function * (values (or null node) (or null node) &optional))
                undo-union-by-width))
(defun undo-union-by-width (merged recs)
  "Undoes a UNION-BY-WIDTH-JOURNALED: dismantles the merged tree back into the
two operands, exactly as they were, except that a node the union cut inside
its width comes back re-fused from its fragments (original priority and
fields; the fresh-priority right fragment is discarded). MERGED must be the
tree the union returned. Returns (values a b)."
  (declare (optimize (speed 3))
           ((or null node) merged)
           (list recs))
  (labels ((tag (rec) (if (consp rec) (car rec) rec))
           (undo-concat (merged)
             (declare ((or null node) merged))
             (let ((rec (pop recs)))
               (ecase (tag rec)
                 (:c-trivial-l (values nil merged))
                 (:c-trivial-r (values merged nil))
                 (:c-left
                  (let ((ln merged))
                    (declare (node ln))
                    (multiple-value-bind (lr rn) (undo-concat (%node-right ln))
                      (setf (%node-right ln) lr)
                      (pull-up ln)
                      (values ln rn))))
                 (:c-right
                  (let ((rn merged))
                    (declare (node rn))
                    (multiple-value-bind (ln rl) (undo-concat (%node-left rn))
                      (setf (%node-left rn) rl)
                      (pull-up rn)
                      (values ln rn)))))))
           (undo-wsplit (left right)
             (declare ((or null node) left right))
             (let ((rec (pop recs)))
               (ecase (tag rec)
                 (:ws-empty nil)
                 (:ws-lt
                  (multiple-value-bind (node rl) (undo-concat left)
                    (declare (node node))
                    (setf (%node-right node) (undo-wsplit rl right))
                    (pull-up node)
                    node))
                 (:ws-ge
                  (multiple-value-bind (lr node) (undo-concat right)
                    (declare (node node))
                    (setf (%node-left node) (undo-wsplit left lr))
                    (pull-up node)
                    node))
                 (:ws-inside
                  (multiple-value-bind (rleaf right-child) (undo-concat right)
                    (declare (ignore rleaf))
                    (let ((node left))
                      (declare (node node))
                      (setf (%node-right node) right-child
                            (%node-width node) (cdr rec))
                      (pull-up node)
                      node))))))
           (recur (merged)
             (declare ((or null node) merged))
             (let ((rec (pop recs)))
               (ecase (tag rec)
                 (:w-trivial-a (values nil merged))
                 (:w-trivial-b (values merged nil))
                 (:w-fuse
                  (let* ((from-a (cadr rec))
                         (leaf (cddr rec))
                         (other (add-to-leftmost-slope-gap
                                 merged (- (%node-slope-gap leaf)))))
                    (if from-a
                        (values leaf other)
                        (values other leaf))))
                 (:w-frame
                  (let ((root-from-a (cadr rec))
                        (r (cddr rec)))
                    (declare (node r))
                    (multiple-value-bind (left mid-right) (undo-concat merged)
                      (multiple-value-bind (fused right) (undo-concat mid-right)
                        (let ((o-mid (add-to-leftmost-slope-gap
                                      fused (- (%node-slope-gap r)))))
                          (multiple-value-bind (r-right o-right) (recur right)
                            (multiple-value-bind (r-left o-left) (recur left)
                              (let* ((o-rest (undo-wsplit o-mid o-right))
                                     (other (undo-wsplit o-left o-rest)))
                                (setf (%node-left r) r-left
                                      (%node-right r) r-right)
                                (pull-up r)
                                (if root-from-a
                                    (values r other)
                                    (values other r))))))))))))))
    (recur merged)))

(defstruct (mstrick (:constructor make-mstrick (dom-min &optional (anchor-value 0)))
                    (:conc-name %mstrick-)
                    (:copier nil)
                    (:predicate nil))
  "Convex piecewise-linear function f with compact effective domain. The
constructor gives the single graph point (DOM-MIN, ANCHOR-VALUE): f =
ANCHOR-VALUE at DOM-MIN and +inf elsewhere. Conjugate view: the affine
function f*(p) = DOM-MIN*p - ANCHOR-VALUE."
  ;; Left end of the effective domain. (Conjugate view: the slope of the
  ;; leftmost piece of f*.)
  (dom-min 0 :type fixnum)
  ;; f(DOM-MIN). (Conjugate view: f*(p) = DOM-MIN*p - ANCHOR-VALUE left of
  ;; every breakpoint.)
  (anchor-value 0 :type fixnum)
  ;; Slope of the leftmost segment when SEGMENTS is non-empty, 0 otherwise.
  ;; Position bookkeeping for the treap's relative SLOPE-GAP encoding; not
  ;; part of the height decoding.
  (min-slope 0 :type fixnum)
  (segments nil :type (or null node)))

(declaim (inline mstrick-dom-min))
(defun mstrick-dom-min (mstrick)
  "Returns the left end of the effective domain of f. Conjugate view: the
slope of the leftmost piece of f*."
  (%mstrick-dom-min mstrick))

(declaim (inline mstrick-dom-max))
(defun mstrick-dom-max (mstrick)
  "Returns the right end of the effective domain of f. Conjugate view: the
slope of the rightmost piece of f*."
  (the fixnum (+ (%mstrick-dom-min mstrick)
                 (node-width-sum (%mstrick-segments mstrick)))))

(declaim (ftype (function * (values fixnum &optional)) mstrick-value))
(defun mstrick-value (mstrick x)
  "Returns f(X); +POSITIVE-INF+ outside dom f = [DOM-MIN, DOM-MAX]."
  (declare (optimize (speed 3))
           (fixnum x))
  (let ((dom-min (%mstrick-dom-min mstrick))
        (segments (%mstrick-segments mstrick)))
    (cond ((or (< x dom-min) (< (mstrick-dom-max mstrick) x))
           +positive-inf+)
          ((null segments)
           (%mstrick-anchor-value mstrick))
          (t
           (the+ fixnum
                 (%mstrick-anchor-value mstrick)
                 (rise-up-to-width-idx segments (- x dom-min)
                                       (%mstrick-min-slope mstrick)))))))

(declaim (ftype (function * (values fixnum &optional)) mstrick-conj-value))
(defun mstrick-conj-value (mstrick p)
  "Returns the conjugate f*(P) = max_x (P*x - f(x)) -- the classic slope-trick
function is this conjugate."
  (declare (optimize (speed 3))
           (fixnum p))
  (conj-value-fold (%mstrick-segments mstrick) p (%mstrick-min-slope mstrick)
                   (%mstrick-dom-min mstrick) (%mstrick-anchor-value mstrick)))

(declaim (ftype (function * (values fixnum fixnum &optional)) mstrick-subdiff))
(defun mstrick-subdiff (mstrick x)
  "Returns the subdifferential of f at X as (values left-slope right-slope).
Empty outside the effective domain: (+NEGATIVE-INF+, +NEGATIVE-INF+) when X <
DOM-MIN, (+POSITIVE-INF+, +POSITIVE-INF+) when X > DOM-MAX. At the left domain
end the left slope is +NEGATIVE-INF+; mirror at the right."
  (declare (optimize (speed 3))
           (fixnum x))
  (let ((dom-min (%mstrick-dom-min mstrick))
        (dom-max (mstrick-dom-max mstrick))
        (segments (%mstrick-segments mstrick))
        (min-slope (%mstrick-min-slope mstrick)))
    (cond ((< x dom-min) (values +negative-inf+ +negative-inf+))
          ((< dom-max x) (values +positive-inf+ +positive-inf+))
          (t
           (let ((idx (- x dom-min)))
             (values (if (= x dom-min)
                         +negative-inf+
                         (slope-before-width-idx segments idx min-slope))
                     (if (= x dom-max)
                         +positive-inf+
                         (slope-at-width-idx segments idx min-slope))))))))

(declaim (ftype (function * (values fixnum fixnum &optional)) mstrick-arg-subdiff))
(defun mstrick-arg-subdiff (mstrick p)
  "Returns argmin_s (f(s) - P*s) as the closed interval (values left right).
Conjugate view: the subdifferential of f* at P."
  (declare (optimize (speed 3))
           (fixnum p))
  (let ((dom-min (%mstrick-dom-min mstrick))
        (segments (%mstrick-segments mstrick))
        (min-slope (%mstrick-min-slope mstrick)))
    (if (null segments)
        (values dom-min dom-min)
        (values (the+ fixnum dom-min (width-sum-lt segments p min-slope))
                (the+ fixnum dom-min (width-sum-le segments p min-slope))))))

(defun mstrick-insert-segment (mstrick slope width)
  "Infimal-convolves f with a single linear segment of slope SLOPE and signed
horizontal width WIDTH (the segment from (0, 0) to (WIDTH, SLOPE*WIDTH)); a
negative WIDTH flips the segment horizontally and grows the domain leftward.

Conjugate view: f*(p) += max(0, WIDTH*(p - SLOPE)) -- the classic slope-trick
addition of a ReLU kink of slope WIDTH at p = SLOPE."
  (declare (optimize (speed 3))
           (fixnum slope width))
  (unless (zerop width)
    ;; At the left end of the new domain only one decomposition of the
    ;; infimal convolution is feasible: the inserted segment contributes
    ;; nothing there for WIDTH > 0 and its full rise for WIDTH < 0.
    (when (< width 0)
      (incf (%mstrick-dom-min mstrick) width)
      (incf (%mstrick-anchor-value mstrick) (the fixnum (* slope width))))
    (multiple-value-bind (new-segments new-min-slope)
        (insert (%mstrick-segments mstrick) slope (abs width)
                (%mstrick-min-slope mstrick))
      (setf (%mstrick-segments mstrick) new-segments
            (%mstrick-min-slope mstrick) new-min-slope)))
  mstrick)

(defun mstrick-remove-segment (mstrick slope width)
  "Removes a segment previously added by (MSTRICK-INSERT-SEGMENT MSTRICK SLOPE
WIDTH). Signals an error when no such segment is stored; the behavior is
undefined if the removal breaks convexity.

Conjugate view: f*(p) -= max(0, WIDTH*(p - SLOPE))."
  (declare (optimize (speed 3))
           (fixnum slope width))
  (unless (zerop width)
    (when (< width 0)
      (decf (%mstrick-dom-min mstrick) width)
      (decf (%mstrick-anchor-value mstrick) (the fixnum (* slope width))))
    (multiple-value-bind (new-segments new-min-slope)
        (%delete (%mstrick-segments mstrick) slope (abs width)
                 (%mstrick-min-slope mstrick))
      (setf (%mstrick-segments mstrick) new-segments
            (%mstrick-min-slope mstrick)
            (if new-segments new-min-slope 0))))
  mstrick)

(defun mstrick-add-kink (mstrick kink left-slope right-slope)
  "Adds the one-kink piecewise-linear function h to f, where h(KINK) = 0 and h
has slope LEFT-SLOPE on x <= KINK and slope RIGHT-SLOPE on x >= KINK. h is
convex for LEFT-SLOPE <= RIGHT-SLOPE; a concave step undoes a previously added
one, and the behavior is undefined if f + h is not convex. The effective
domain is unchanged.

Conjugate view: infimal-convolve f* with p |-> KINK*p on [LEFT-SLOPE,
RIGHT-SLOPE] (+inf outside) -- the tilted window minimum. With equal slopes
this adds a linear function to f; with LEFT-SLOPE = 0 or RIGHT-SLOPE = 0 it is
the classic sliding-window minimum of f*."
  (declare (optimize (speed 3))
           (fixnum kink left-slope right-slope))
  (let ((dom-min (%mstrick-dom-min mstrick))
        (segments (%mstrick-segments mstrick))
        (min-slope (%mstrick-min-slope mstrick)))
    (let ((offset (- dom-min kink)))
      (declare (fixnum offset))
      (cond ((null segments)
             ;; Single-point domain: add h(DOM-MIN), reading h on the side of
             ;; the kink the point falls on.
             (let ((rate (if (< kink dom-min) right-slope left-slope)))
               (incf (%mstrick-anchor-value mstrick) (the fixnum (* rate offset)))))
            ((<= (mstrick-dom-max mstrick) kink)
             ;; Whole domain left of the kink: h is linear at LEFT-SLOPE.
             (incf (%mstrick-anchor-value mstrick)
                   (the fixnum (* left-slope offset)))
             (incf (%mstrick-min-slope mstrick) left-slope))
            ((<= kink dom-min)
             ;; Whole domain right of the kink: h is linear at RIGHT-SLOPE.
             (incf (%mstrick-anchor-value mstrick)
                   (the fixnum (* right-slope offset)))
             (incf (%mstrick-min-slope mstrick) right-slope))
            (t
             ;; Kink strictly inside the domain: split the segments at it and
             ;; shift each side's slopes; the relative gaps within each side
             ;; are unchanged. The anchor sits left of the kink.
             (incf (%mstrick-anchor-value mstrick)
                   (the fixnum (* left-slope offset)))
             (multiple-value-bind (l r r-first)
                 (split-by-width-idx segments (- kink dom-min) min-slope)
               (let* ((l-p (if l t nil))
                      (new-left-anchor (the+ fixnum min-slope left-slope))
                      (new-right-anchor (the+ fixnum (or r-first min-slope)
                                              right-slope)))
                 ;; CONCAT (not SIMPLE-CONCAT) so a kink whose gap closes to
                 ;; zero -- a negated ADD-KINK undoing the split of the call
                 ;; it reverses -- fuses back into a single node.
                 (setf (%mstrick-segments mstrick)
                       (concat l r new-left-anchor new-right-anchor)
                       (%mstrick-min-slope mstrick)
                       (if l-p new-left-anchor new-right-anchor))))))))
  mstrick)

(defun mstrick-translate (mstrick delta)
  "Translates the graph of f right by DELTA: f(x) := f(x - DELTA).

Conjugate view: f*(p) += DELTA*p."
  (declare (fixnum delta))
  (incf (%mstrick-dom-min mstrick) delta)
  mstrick)

(defun mstrick-inf-conv (mstrick other)
  "Infimal convolution f := f box OTHER, i.e. f(x) := inf over x1 + x2 = x of
\(f(x1) + OTHER(x2)). The effective domains Minkowski-add, the anchor vertices
add, and the segment multisets union by slope via bulk treap union. OTHER is
destructively consumed.

Conjugate view: f* += OTHER* (pointwise sum of the conjugates)."
  (declare (optimize (speed 3)))
  (incf (%mstrick-dom-min mstrick) (%mstrick-dom-min other))
  (incf (%mstrick-anchor-value mstrick) (%mstrick-anchor-value other))
  (multiple-value-bind (segments min-slope)
      (union-by-slope (%mstrick-segments mstrick) (%mstrick-segments other)
                      (%mstrick-min-slope mstrick) (%mstrick-min-slope other))
    (setf (%mstrick-segments mstrick) segments
          (%mstrick-min-slope mstrick) (if segments min-slope 0)))
  mstrick)

(defun mstrick-pointwise-add (mstrick other)
  "Pointwise sum f := f + OTHER on the intersection of the effective domains;
signals an error if the domains are disjoint. Both operands are restricted to
the common window, then the segment trees merge into the common refinement of
the two partitions via bulk treap union. OTHER is destructively consumed.

Conjugate view: f* := f* box OTHER*."
  (declare (optimize (speed 3)))
  (let ((lo (max (%mstrick-dom-min mstrick) (%mstrick-dom-min other)))
        (hi (min (mstrick-dom-max mstrick) (mstrick-dom-max other))))
    (unless (<= lo hi)
      (error "mstrick-pointwise-add: disjoint effective domains"))
    (mstrick-restrict-dom-min mstrick lo)
    (mstrick-restrict-dom-max mstrick hi)
    (mstrick-restrict-dom-min other lo)
    (mstrick-restrict-dom-max other hi)
    (incf (%mstrick-anchor-value mstrick) (%mstrick-anchor-value other))
    (incf (%mstrick-min-slope mstrick) (%mstrick-min-slope other))
    (let ((segments (union-by-width (%mstrick-segments mstrick)
                                    (%mstrick-segments other))))
      (setf (%mstrick-segments mstrick) segments)
      (unless segments
        (setf (%mstrick-min-slope mstrick) 0))))
  mstrick)

(defun mstrick-restrict-dom-max (mstrick c)
  "Restricts the effective domain to (-inf, C]: f := f + delta_{(-inf, C]}.
Signals an error if C < DOM-MIN (the domain would become empty).

Conjugate view: clip every slope of f* above C down to C (the classic
left-cumulative slope clip).

Returns an opaque rollback token, only to be consumed by
MSTRICK-RESTRICT-DOM-MAX-ROLLBACK."
  (declare (optimize (speed 3))
           (fixnum c))
  (let ((dom-min (%mstrick-dom-min mstrick)))
    (when (< c dom-min)
      (error "mstrick-restrict-dom-max: C = ~D < DOM-MIN = ~D would empty the domain"
             c dom-min))
    (let ((was-non-empty (if (%mstrick-segments mstrick) t nil))
          (rest-segments nil)
          (rest-min-slope 0))
      (declare (fixnum rest-min-slope))
      (when (< c (mstrick-dom-max mstrick))
        (multiple-value-bind (l r r-first)
            (split-by-width-idx (%mstrick-segments mstrick) (- c dom-min)
                                (%mstrick-min-slope mstrick))
          (setf (%mstrick-segments mstrick) l
                rest-segments r)
          (when r-first
            (setq rest-min-slope r-first))))
      (when (and was-non-empty (null (%mstrick-segments mstrick)))
        ;; No breakpoints survive the clip: f collapses to the single point
        ;; {DOM-MIN}, whose value is still the anchor.
        (setf (%mstrick-min-slope mstrick) 0))
      (let ((rest (make-mstrick dom-min (%mstrick-anchor-value mstrick))))
        (setf (%mstrick-min-slope rest) rest-min-slope
              (%mstrick-segments rest) rest-segments)
        rest))))

(defun mstrick-restrict-dom-max-rollback (mstrick rest)
  "Undoes an MSTRICK-RESTRICT-DOM-MAX, consuming its rollback token REST."
  (declare (optimize (speed 3)))
  (let ((was-empty (null (%mstrick-segments mstrick)))
        (self-anchor (%mstrick-min-slope mstrick)))
    ;; CONCAT (not SIMPLE-CONCAT) so an equal-slope boundary -- left over from
    ;; a clip that fell strictly inside a kink -- fuses back into one node.
    (setf (%mstrick-segments mstrick)
          (concat (%mstrick-segments mstrick) (%mstrick-segments rest)
                  self-anchor (%mstrick-min-slope rest)))
    (when was-empty
      (setf (%mstrick-min-slope mstrick) (%mstrick-min-slope rest)))
    (unless (%mstrick-segments mstrick)
      (setf (%mstrick-min-slope mstrick) 0))
    (setf (%mstrick-dom-min mstrick) (%mstrick-dom-min rest)
          (%mstrick-anchor-value mstrick) (%mstrick-anchor-value rest)))
  mstrick)

(defun mstrick-restrict-dom-min (mstrick c)
  "Restricts the effective domain to [C, +inf): f := f + delta_{[C, +inf)}.
Signals an error if C > DOM-MAX.

Conjugate view: clip every slope of f* below C up to C (the classic
right-cumulative slope clip).

Returns an opaque rollback token, only to be consumed by
MSTRICK-RESTRICT-DOM-MIN-ROLLBACK."
  (declare (optimize (speed 3))
           (fixnum c))
  (let ((dom-min (%mstrick-dom-min mstrick)))
    (if (<= c dom-min)
        (let ((rest (make-mstrick dom-min (%mstrick-anchor-value mstrick))))
          (setf (%mstrick-min-slope rest) (%mstrick-min-slope mstrick))
          rest)
        (let ((dom-max (mstrick-dom-max mstrick))
              (min-slope (%mstrick-min-slope mstrick)))
          (when (< dom-max c)
            (error "mstrick-restrict-dom-min: C = ~D > DOM-MAX = ~D would empty the domain"
                   c dom-max))
          ;; Split off the dropped left part L (width exactly C - DOM-MIN);
          ;; when C = DOM-MAX everything lands in L and f collapses to the
          ;; single point {C}. The dropped part carries the rise f(C) -
          ;; f(DOM-MIN) in its aggregates, giving the new anchor in O(1).
          (multiple-value-bind (l r r-first)
              (split-by-width-idx (%mstrick-segments mstrick) (- c dom-min) min-slope)
            (let ((rest (make-mstrick dom-min (%mstrick-anchor-value mstrick))))
              (setf (%mstrick-min-slope rest) min-slope
                    (%mstrick-segments rest) l)
              (incf (%mstrick-anchor-value mstrick) (link-rise l min-slope))
              (setf (%mstrick-segments mstrick) r
                    (%mstrick-min-slope mstrick) (or r-first 0)
                    (%mstrick-dom-min mstrick) c)
              rest))))))

(defun mstrick-restrict-dom-min-rollback (mstrick rest)
  "Undoes an MSTRICK-RESTRICT-DOM-MIN, consuming its rollback token REST."
  (declare (optimize (speed 3)))
  (when (%mstrick-segments rest)
    (let ((self-anchor (%mstrick-min-slope mstrick)))
      (setf (%mstrick-segments mstrick)
            (concat (%mstrick-segments rest) (%mstrick-segments mstrick)
                    (%mstrick-min-slope rest) self-anchor)
            ;; REST was non-empty, so its leftmost is the merged leftmost.
            (%mstrick-min-slope mstrick) (%mstrick-min-slope rest)
            (%mstrick-dom-min mstrick) (%mstrick-dom-min rest)
            (%mstrick-anchor-value mstrick) (%mstrick-anchor-value rest))))
  mstrick)

;; The journaled binary operations below produce the same result as their
;; plain counterparts and return an opaque rollback token. The rollback
;; restores MSTRICK to its exact pre-operation state and returns the consumed
;; OTHER operand (the same object), also exactly restored. Tokens must be
;; consumed strictly LIFO across nested calls. MSTRICK-TRANSLATE needs no
;; token: translating by -DELTA is its exact inverse.

(defun mstrick-inf-conv-with-rollback (mstrick other)
  "MSTRICK-INF-CONV returning an opaque rollback token for
MSTRICK-INF-CONV-ROLLBACK. The undo cost equals the forward cost."
  (declare (optimize (speed 3)))
  (let ((f-dom-min (%mstrick-dom-min mstrick))
        (f-anchor (%mstrick-anchor-value mstrick))
        (f-min-slope (%mstrick-min-slope mstrick))
        (o-dom-min (%mstrick-dom-min other))
        (o-anchor (%mstrick-anchor-value other))
        (o-min-slope (%mstrick-min-slope other)))
    (incf (%mstrick-dom-min mstrick) o-dom-min)
    (incf (%mstrick-anchor-value mstrick) o-anchor)
    (multiple-value-bind (segments min-slope recs)
        (union-by-slope-journaled (%mstrick-segments mstrick)
                                  (%mstrick-segments other)
                                  f-min-slope o-min-slope)
      (setf (%mstrick-segments mstrick) segments
            (%mstrick-min-slope mstrick) (if segments min-slope 0))
      (list recs other f-dom-min f-anchor f-min-slope
            o-dom-min o-anchor o-min-slope))))

(defun mstrick-inf-conv-rollback (mstrick token)
  "Undoes an MSTRICK-INF-CONV-WITH-ROLLBACK, consuming TOKEN. Returns the
restored consumed operand."
  (declare (optimize (speed 3)))
  (destructuring-bind (recs other f-dom-min f-anchor f-min-slope
                       o-dom-min o-anchor o-min-slope)
      token
    (multiple-value-bind (a b)
        (undo-union-by-slope (%mstrick-segments mstrick) recs)
      (setf (%mstrick-segments mstrick) a
            (%mstrick-dom-min mstrick) f-dom-min
            (%mstrick-anchor-value mstrick) f-anchor
            (%mstrick-min-slope mstrick) f-min-slope
            (%mstrick-segments other) b
            (%mstrick-dom-min other) o-dom-min
            (%mstrick-anchor-value other) o-anchor
            (%mstrick-min-slope other) o-min-slope)
      other)))

(defun mstrick-pointwise-add-with-rollback (mstrick other)
  "MSTRICK-POINTWISE-ADD returning an opaque rollback token for
MSTRICK-POINTWISE-ADD-ROLLBACK. The undo cost equals the forward cost."
  (declare (optimize (speed 3)))
  (let ((lo (max (%mstrick-dom-min mstrick) (%mstrick-dom-min other)))
        (hi (min (mstrick-dom-max mstrick) (mstrick-dom-max other))))
    (unless (<= lo hi)
      (error "mstrick-pointwise-add: disjoint effective domains"))
    (let* ((self-min (mstrick-restrict-dom-min mstrick lo))
           (self-max (mstrick-restrict-dom-max mstrick hi))
           (other-min (mstrick-restrict-dom-min other lo))
           (other-max (mstrick-restrict-dom-max other hi))
           ;; Post-restriction snapshots: the merge is undone onto these, and
           ;; the restriction rollbacks then restore the original fields.
           (f-dom-min (%mstrick-dom-min mstrick))
           (f-anchor (%mstrick-anchor-value mstrick))
           (f-min-slope (%mstrick-min-slope mstrick))
           (o-dom-min (%mstrick-dom-min other))
           (o-anchor (%mstrick-anchor-value other))
           (o-min-slope (%mstrick-min-slope other)))
      (incf (%mstrick-anchor-value mstrick) o-anchor)
      (incf (%mstrick-min-slope mstrick) o-min-slope)
      (multiple-value-bind (segments recs)
          (union-by-width-journaled (%mstrick-segments mstrick)
                                    (%mstrick-segments other))
        (setf (%mstrick-segments mstrick) segments)
        (unless segments
          (setf (%mstrick-min-slope mstrick) 0))
        (list recs other f-dom-min f-anchor f-min-slope
              o-dom-min o-anchor o-min-slope
              self-min self-max other-min other-max)))))

(defun mstrick-pointwise-add-rollback (mstrick token)
  "Undoes an MSTRICK-POINTWISE-ADD-WITH-ROLLBACK, consuming TOKEN. Returns
the restored consumed operand."
  (declare (optimize (speed 3)))
  (destructuring-bind (recs other f-dom-min f-anchor f-min-slope
                       o-dom-min o-anchor o-min-slope
                       self-min self-max other-min other-max)
      token
    (multiple-value-bind (a b)
        (undo-union-by-width (%mstrick-segments mstrick) recs)
      (setf (%mstrick-segments mstrick) a
            (%mstrick-dom-min mstrick) f-dom-min
            (%mstrick-anchor-value mstrick) f-anchor
            (%mstrick-min-slope mstrick) f-min-slope
            (%mstrick-segments other) b
            (%mstrick-dom-min other) o-dom-min
            (%mstrick-anchor-value other) o-anchor
            (%mstrick-min-slope other) o-min-slope)
      (mstrick-restrict-dom-max-rollback other other-max)
      (mstrick-restrict-dom-min-rollback other other-min)
      (mstrick-restrict-dom-max-rollback mstrick self-max)
      (mstrick-restrict-dom-min-rollback mstrick self-min)
      other)))

;; The discrete envelope operations below read the stored f through its grid
;; samples f|_Z -- a discrete convex sequence on an integer interval -- act on
;; those samples, and re-close (interpolate), so the results keep integer
;; breakpoints, integer slopes, and integer values. The two operations are
;; conjugate to each other: point absorption is the pointwise max of f* with
;; the point's Fenchel line, spliced on the slope axis instead of the width
;; axis.

(declaim (inline min-grid-gt max-grid-lt))
(defun min-grid-gt (m d)
  "Returns the smallest integer T with D*T > M, for D > 0."
  (declare (fixnum m d))
  (the fixnum (+ (floor m d) 1)))
(defun max-grid-lt (n d)
  "Returns the largest integer T with D*T < N, for D > 0."
  (declare (fixnum n d))
  (floor (the fixnum (- n 1)) d))

(defun mstrick-max-affine (mstrick a b)
  "Replaces f by the interpolation of z |-> max(f(z), A*z + B) over the
integer grid -- the discrete pointwise max with the cut line A*x + B. The
effective domain is unchanged (f = +inf outside wins the max). Values at
integers match the real max exactly; strictly between integers the result may
exceed it (the rational crossing of the line with a segment is rounded away
upward).

Conjugate view: convex-hull the point (A, -B) into epi f* on the integer
grid."
  (declare (optimize (speed 3))
           (fixnum a b))
  (let ((segments (%mstrick-segments mstrick))
        (dom-min (%mstrick-dom-min mstrick))
        (min-slope (%mstrick-min-slope mstrick))
        (anchor-value (%mstrick-anchor-value mstrick)))
    (when (null segments)
      (setf (%mstrick-anchor-value mstrick)
            (max anchor-value (the+ fixnum (* a dom-min) b)))
      (return-from mstrick-max-affine mstrick))
    ;; Exact no-op guard: sup_x (line(x) - f(x)) = f*(A) + B, attained at a
    ;; graph vertex (an integer), so <= 0 means no integer improves.
    (when (<= (the+ fixnum (mstrick-conj-value mstrick a) b) 0)
      (return-from mstrick-max-affine mstrick))
    (let* ((dom-max (mstrick-dom-max mstrick))
           (f-dom-max (the+ fixnum anchor-value (link-rise segments min-slope)))
           ;; Improving integer interval [U, V] -- the integers where the line
           ;; strictly exceeds f. A weak tie at a domain end is folded into
           ;; that end.
           (u (if (<= anchor-value (the+ fixnum (* a dom-min) b))
                  dom-min
                  ;; The cut rises through the leftmost crossed segment of
                  ;; slope S < A; on it the improving integers satisfy
                  ;; (A - S)*t > f(x_l) - S*x_l - B.
                  (let* ((s (intercept-leftmost segments anchor-value dom-min
                                                (- b) a min-slope))
                         (x-l (the+ fixnum dom-min
                                    (width-sum-lt segments s min-slope)))
                         (f-x-l (mstrick-value mstrick x-l)))
                    (min-grid-gt (the fixnum
                                      (- f-x-l (the+ fixnum (* s x-l) b)))
                                 (- a s)))))
           (v (if (<= f-dom-max (the+ fixnum (* a dom-max) b))
                  dom-max
                  ;; Mirror: the rightmost crossed segment has slope S > A;
                  ;; anchored at its left endpoint X-R, the improving integers
                  ;; satisfy (S - A)*t < B + S*x_r - f(x_r).
                  (let* ((s (intercept-rightmost segments anchor-value dom-min
                                                 (- b) a min-slope))
                         (x-r (the+ fixnum dom-min
                                    (width-sum-lt segments s min-slope)))
                         (f-x-r (mstrick-value mstrick x-r)))
                    (max-grid-lt (the fixnum
                                      (- (the+ fixnum b (* s x-r)) f-x-r))
                                 (- s a))))))
      (declare (fixnum u v))
      (assert (and (<= dom-min u) (<= u v) (<= v dom-max)))
      ;; Values pinning the two glue segments, read before mutating.
      (let ((glue-left (when (< dom-min u)
                         (the fixnum
                              (- (the+ fixnum (* a u) b)
                                 (mstrick-value mstrick (- u 1))))))
            (glue-right (when (< v dom-max)
                          (the fixnum
                               (- (mstrick-value mstrick (+ v 1))
                                  (the+ fixnum (* a v) b))))))
        ;; Splice on the width axis: keep f over [DOM-MIN, U-1] and
        ;; [V+1, DOM-MAX], drop the middle wholesale, and re-tile [U-1, V+1]
        ;; with glue/body/glue.
        (multiple-value-bind (keep-left rest rest-first)
            (if (< dom-min u)
                (split-by-width-idx segments (- (- u 1) dom-min) min-slope)
                (values nil segments min-slope))
          (let ((rest-anchor (or rest-first min-slope)))
            (multiple-value-bind (middle keep-right keep-right-first)
                (if (< v dom-max)
                    (split-by-width-idx rest
                                        (- (+ v 1)
                                           (if (< dom-min u) (- u 1) dom-min))
                                        rest-anchor)
                    (values rest nil nil))
              (declare (ignore middle))
              (multiple-value-bind (new-segments new-min-slope)
                  (concat-kept keep-left keep-right min-slope
                               keep-right-first rest-anchor)
                ;; Glue [U-1, U], body [U, V], glue [V, V+1]; INSERT fuses
                ;; equal slopes. Widths telescope to the old span.
                (when glue-left
                  (multiple-value-setq (new-segments new-min-slope)
                    (insert new-segments glue-left 1 new-min-slope)))
                (when (< u v)
                  (multiple-value-setq (new-segments new-min-slope)
                    (insert new-segments a (- v u) new-min-slope)))
                (when glue-right
                  (multiple-value-setq (new-segments new-min-slope)
                    (insert new-segments glue-right 1 new-min-slope)))
                (setf (%mstrick-segments mstrick) new-segments
                      (%mstrick-min-slope mstrick) new-min-slope)
                (when (= u dom-min)
                  (setf (%mstrick-anchor-value mstrick)
                        (the+ fixnum (* a dom-min) b))))))))))
  mstrick)

(defun mstrick-convex-hull-with-point (mstrick x y)
  "Replaces f by the integer biconjugate of min(f, delta_{(X, Y)}) -- the
greatest function with integer breakpoints and slopes minorizing the real hull
conv(epi f union {(X, Y)}). May extend the effective domain to X; f(X) = Y
holds afterwards whenever Y < f(X) held before (with f = +inf outside the
domain); strictly fractional hull edges are rounded away downward.

Conjugate view: the discrete pointwise max of f* with the point's Fenchel line
p |-> X*p - Y."
  (declare (optimize (speed 3))
           (fixnum x y))
  (let ((dom-min (%mstrick-dom-min mstrick))
        (dom-max (mstrick-dom-max mstrick)))
    ;; Exact no-op guard: a point on or above the graph changes nothing.
    (when (and (<= dom-min x) (<= x dom-max)
               (<= (mstrick-value mstrick x) y))
      (return-from mstrick-convex-hull-with-point mstrick))
    (when (null (%mstrick-segments mstrick))
      ;; Single-point domain: the result is the two-point integer hull -- the
      ;; max of the floor(sigma)-slope line through the left point and the
      ;; ceiling(sigma)-slope line through the right point, sigma the chord
      ;; slope.
      (let ((x0 dom-min)
            (y0 (%mstrick-anchor-value mstrick)))
        (when (= x x0)
          (setf (%mstrick-anchor-value mstrick) y)
          (return-from mstrick-convex-hull-with-point mstrick))
        (let (zl yl zr yr)
          (if (< x x0)
              (setq zl x yl y zr x0 yr y0)
              (setq zl x0 yl y0 zr x yr y))
          (locally (declare (fixnum zl yl zr yr))
            (let* ((span (- zr zl))
                   (rise (- yr yl))
                   (q (floor rise span))
                   (w-hi (the fixnum (- rise (the fixnum (* q span)))))
                   (w-lo (the fixnum (- span w-hi))))
              (declare (fixnum span rise q))
              (setf (%mstrick-dom-min mstrick) zl
                    (%mstrick-anchor-value mstrick) yl)
              (multiple-value-bind (m ms) (insert nil q w-lo 0)
                (multiple-value-setq (m ms) (insert m (+ q 1) w-hi ms))
                (setf (%mstrick-segments mstrick) m
                      (%mstrick-min-slope mstrick) ms)))))
        (return-from mstrick-convex-hull-with-point mstrick)))
    (let ((segments (%mstrick-segments mstrick))
          (min-slope (%mstrick-min-slope mstrick))
          (anchor-value (%mstrick-anchor-value mstrick)))
      ;; Improving slope interval [P-L, P-R] = {p integer : f*(p) < X*p - Y},
      ;; nonempty since Y < f(X). A side is bounded exactly when X lies
      ;; strictly inside the domain on that side; an unbounded side is a
      ;; domain extension.
      (let* ((left-bounded (< dom-min x))
             (right-bounded (< x dom-max))
             (p-l
               (if left-bounded
                   (multiple-value-bind (key fstar adj)
                       (conj-intercept-leftmost segments x y dom-min
                                                anchor-value min-slope)
                     (if key
                         ;; The improving integers satisfy
                         ;; (X - ADJ)*p > f*(KEY) + Y - ADJ*KEY.
                         (min-grid-gt (the fixnum
                                           (- (the+ fixnum fstar y)
                                              (the fixnum (* adj key))))
                                      (- x adj))
                         ;; The cut line clears no conjugate vertex: the
                         ;; crossing sits in the right-infinite conjugate
                         ;; segment, possible only for X > DOM-MAX.
                         (let ((f-dom-max (the+ fixnum anchor-value
                                                (link-rise segments min-slope))))
                           (min-grid-gt (the fixnum (- y f-dom-max))
                                        (- x dom-max)))))
                   0))
             (p-r
               (if right-bounded
                   (multiple-value-bind (key fstar adj)
                       (conj-intercept-rightmost segments x y dom-min
                                                 anchor-value min-slope)
                     (if key
                         ;; Mirror: (ADJ - X)*p < ADJ*KEY - f*(KEY) - Y.
                         (max-grid-lt (the fixnum
                                           (- (the fixnum (* adj key))
                                              (the+ fixnum fstar y)))
                                      (- adj x))
                         ;; Mirror fallback: the crossing sits in the
                         ;; left-infinite conjugate segment, possible only for
                         ;; X < DOM-MIN.
                         (max-grid-lt (the fixnum (- anchor-value y))
                                      (- dom-min x))))
                   0)))
        (declare (fixnum p-l p-r))
        (assert (or (not (and left-bounded right-bounded)) (<= p-l p-r)))
        ;; Glue abscissae -- the conjugate slopes of the result on
        ;; [P-L - 1, P-L] and [P-R, P-R + 1] -- read before mutating.
        (let ((g-l (when left-bounded
                     (the fixnum
                          (- (the fixnum (- (the fixnum (* x p-l)) y))
                             (mstrick-conj-value mstrick (- p-l 1))))))
              (g-r (when right-bounded
                     (the fixnum
                          (- (mstrick-conj-value mstrick (+ p-r 1))
                             (the fixnum (- (the fixnum (* x p-r)) y)))))))
          ;; Splice on the slope axis: drop the stored kinks at slopes in
          ;; [P-L, P-R] (all slopes <= P-R on a left extension, all slopes >=
          ;; P-L on a right extension).
          (multiple-value-bind (keep-left rest rest-first)
              (if left-bounded
                  (split-by-slope segments p-l min-slope)
                  (values nil segments min-slope))
            (let ((rest-anchor (or rest-first min-slope)))
              (multiple-value-bind (middle keep-right keep-right-first)
                  (if right-bounded
                      (split-by-slope rest (+ p-r 1) rest-anchor)
                      (values rest nil nil))
                ;; Tangency abscissae bracketing the re-tiled slope window.
                (let* ((t-l (the+ fixnum dom-min (node-width-sum keep-left)))
                       (t-r (the+ fixnum t-l (node-width-sum middle))))
                  (multiple-value-bind (new-segments new-min-slope)
                      (concat-kept keep-left keep-right min-slope
                                   keep-right-first rest-anchor)
                    ;; Up to four new kinks; zero widths are skipped by
                    ;; INSERT, and the two inner ones fuse when P-L = P-R. The
                    ;; result's slope jumps from P-L to P-R at the absorbed
                    ;; vertex's abscissa X.
                    (when g-l
                      (multiple-value-setq (new-segments new-min-slope)
                        (insert new-segments (- p-l 1) (- g-l t-l) new-min-slope))
                      (multiple-value-setq (new-segments new-min-slope)
                        (insert new-segments p-l (- x g-l) new-min-slope)))
                    (when g-r
                      (multiple-value-setq (new-segments new-min-slope)
                        (insert new-segments p-r (- g-r x) new-min-slope))
                      (multiple-value-setq (new-segments new-min-slope)
                        (insert new-segments (+ p-r 1) (- t-r g-r) new-min-slope)))
                    (setf (%mstrick-segments mstrick) new-segments
                          (%mstrick-min-slope mstrick)
                          (if new-segments new-min-slope 0))
                    (unless left-bounded
                      ;; Left extension (or the X = DOM-MIN boundary): the
                      ;; absorbed vertex becomes the left domain end.
                      (setf (%mstrick-dom-min mstrick) x
                            (%mstrick-anchor-value mstrick) y)))))))))))
  mstrick)

(declaim (inline mstrick-map-segments))
(defun mstrick-map-segments (function mstrick)
  "Successively applies FUNCTION to each stored segment of f in increasing
order of slope. FUNCTION must take two arguments: the absolute SLOPE and the
WIDTH."
  (let ((acc (%mstrick-min-slope mstrick)))
    (declare (fixnum acc))
    (labels ((recur (node)
               (when node
                 (recur (%node-left node))
                 (incf acc (%node-slope-gap node))
                 (funcall function acc (%node-width node))
                 (recur (%node-right node)))))
      (recur (%mstrick-segments mstrick)))))

(defmethod print-object ((object mstrick) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A ~A"
            (%mstrick-dom-min object)
            (%mstrick-anchor-value object))
    (mstrick-map-segments
     (lambda (slope width)
       (format stream " <~A . ~A>" slope width))
     object)))
