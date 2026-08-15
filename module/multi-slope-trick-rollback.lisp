(defpackage :cp/multi-slope-trick-rollback
  (:use :cl :cp/multi-slope-trick)
  (:import-from :cp/multi-slope-trick
                #:int #:the+ #:node #:make-node #:random-priority
                #:%node-width #:%node-slope-gap #:%node-slope-gap-sum
                #:%node-priority #:%node-left #:%node-right
                #:node-width-sum #:node-slope-gap-sum #:pull-up
                #:set-leftmost-slope-gap #:add-to-leftmost-slope-gap
                #:take-leftmost-slope-gap #:leftmost-width
                #:reanchor-after-left-split
                #:%mstrick-dom-min #:%mstrick-anchor-value
                #:%mstrick-min-slope #:%mstrick-segments)
  (:export #:mstrick-inf-conv-with-rollback #:mstrick-inf-conv-rollback
           #:mstrick-pointwise-add-with-rollback #:mstrick-pointwise-add-rollback)
  (:documentation
   "Provides journaled forms of the destructive binary operations of
CP/MULTI-SLOPE-TRICK."))
(in-package :cp/multi-slope-trick-rollback)

;; Undo journals. A journaled operation produces the same result as its plain
;; counterpart and additionally returns a list of records -- one per recursion
;; frame, pushed in post-order and popped in exact reverse by the undo, whose
;; recursion mirrors the forward shape, so the undo cost equals the forward
;; cost. A record stores only the node fields the frame overwrote (WIDTH,
;; SLOPE-GAP) and any node the frame detached; aggregates are recomputed by
;; PULL-UP on the unwind, which reproduces them exactly since all arithmetic
;; is exact. Records are either a bare keyword or a cons headed by one.

(declaim (ftype (function * (values (or null node) int list &optional))
                remove-leftmost-journaled))
(defun remove-leftmost-journaled (node pred-slope)
  "REMOVE-LEFTMOST with an undo journal: additionally returns the record list
consumed by UNDO-REMOVE-LEFTMOST."
  (declare (optimize (speed 3))
           (node node)
           (int pred-slope))
  (let ((recs nil))
    (labels ((recur (node pred-slope)
               (declare (node node) (int pred-slope))
               (let* ((left (%node-left node))
                      (node-slope (the+ int pred-slope (node-slope-gap-sum left)
                                        (%node-slope-gap node))))
                 (if left
                     (let ((old-gap (%node-slope-gap node)))
                       (multiple-value-bind (new-left new-anchor) (recur left pred-slope)
                         (declare (int new-anchor))
                         (cond (new-left
                                (setf (%node-left node) new-left
                                      (%node-slope-gap node)
                                      (the int
                                           (- node-slope
                                              (the+ int new-anchor
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
                             (values new-root (the+ int pred-slope gap)))
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

(declaim (ftype (function * (values (or null node) int list &optional))
                union-by-slope-journaled))
(defun union-by-slope-journaled (a b a-min-slope b-min-slope)
  "UNION-BY-SLOPE with an undo journal: additionally returns the record list
consumed by UNDO-UNION-BY-SLOPE."
  (declare (optimize (speed 3))
           ((or null node) a b)
           (int a-min-slope b-min-slope))
  (let ((recs nil))
    (labels
        ((split-j (node slope min-slope)
           (declare ((or null node) node)
                    (int slope min-slope))
           (if (null node)
               (progn (push :s-empty recs) (values nil nil nil))
               (let* ((left (%node-left node))
                      (node-slope (the+ int min-slope (node-slope-gap-sum left)
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
                    (int a-anchor b-anchor))
           (cond ((null a) (push :u-trivial-a recs) (values b b-anchor))
                 ((null b) (push :u-trivial-b recs) (values a a-anchor))
                 (t
                  (let (r other r-anchor o-anchor root-from-a)
                    (if (>= (%node-priority a) (%node-priority b))
                        (setq r a other b r-anchor a-anchor o-anchor b-anchor
                              root-from-a t)
                        (setq r b other a r-anchor b-anchor o-anchor a-anchor
                              root-from-a nil))
                    (locally (declare (node r other) (int r-anchor o-anchor))
                      (let ((old-width (%node-width r))
                            (old-gap (%node-slope-gap r))
                            (r-slope (the+ int r-anchor
                                           (node-slope-gap-sum (%node-left r))
                                           (%node-slope-gap r)))
                            (collision nil)
                            (detach-gap nil))
                        (multiple-value-bind (o-less o-geq o-geq-first)
                            (split-j other r-slope o-anchor)
                          (let ((o-geq-anchor r-slope))
                            (declare (int o-geq-anchor))
                            (when o-geq-first
                              (if (= (the int o-geq-first) r-slope)
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
                              (declare (int r-right-anchor))
                              (let ((rr (%node-right r)))
                                (when rr
                                  (multiple-value-bind (gap rr2)
                                      (take-leftmost-slope-gap rr)
                                    (setq r-right rr2
                                          r-right-anchor (the+ int r-slope gap)
                                          detach-gap gap))))
                              (multiple-value-bind (left left-anchor)
                                  (union-j r-left o-less r-anchor o-anchor)
                                (multiple-value-bind (right right-anchor)
                                    (union-j r-right o-geq r-right-anchor o-geq-anchor)
                                  (setf (%node-slope-gap r)
                                        (if left
                                            (the int
                                                 (- r-slope
                                                    (the+ int left-anchor
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
                    (int idx))
           (if (null node)
               (progn (push :ws-empty recs) (values nil nil))
               (let* ((left (%node-left node))
                      (ls (node-width-sum left))
                      (end-b (the+ int ls (%node-width node))))
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

;; The journaled binary operations below produce the same result as their
;; plain counterparts and return an opaque rollback token. The rollback
;; restores MSTRICK to its exact pre-operation state and returns the consumed
;; OTHER operand (the same object), also exactly restored. Tokens must be
;; consumed strictly LIFO across nested calls. MSTRICK-TRANSLATE and
;; MSTRICK-ADD-CONST need no token: applying -DELTA or -C is the exact
;; inverse.

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
