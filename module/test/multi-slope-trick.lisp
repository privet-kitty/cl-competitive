(defpackage :cp/test/multi-slope-trick
  (:use :cl :fiveam :cp/multi-slope-trick :cp/shuffle)
  (:import-from :cp/multi-slope-trick
                #:%mstrick-segments #:%mstrick-min-slope #:%mstrick-anchor-value
                #:%mstrick-dom-min
                #:%node-width #:%node-slope-gap #:%node-width-sum
                #:%node-slope-gap-sum #:%node-bregman #:%node-priority
                #:%node-left #:%node-right
                #:conj-intercept-leftmost #:conj-intercept-rightmost)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/multi-slope-trick)
(in-suite base-suite)

;;;
;;; Test helpers on the treap representation
;;;

(defun segs (f)
  "Decodes the stored segments as a list of (slope . width) conses."
  (let (result)
    (mstrick-map-segments
     (lambda (slope width) (push (cons slope width) result))
     f)
    (nreverse result)))

(defun vals (f from to)
  (loop for x from from to to collect (mstrick-value f x)))

(defun validate (f)
  "Checks the structural invariants of the stored treap: heap order on
priorities, consistent aggregates, positive widths, strictly increasing
slopes, standalone leftmost gap, MIN-SLOPE consistency."
  (let ((segments (%mstrick-segments f)))
    (labels ((recur (node)
               (if (null node)
                   (values 0 0 0)
                   (let ((left (%node-left node))
                         (right (%node-right node)))
                     (when (and left (> (%node-priority left) (%node-priority node)))
                       (error "validate: heap violation on the left"))
                     (when (and right (> (%node-priority right) (%node-priority node)))
                       (error "validate: heap violation on the right"))
                     (unless (>= (%node-width node) 1)
                       (error "validate: non-positive width ~A" (%node-width node)))
                     (multiple-value-bind (lw lg lb) (recur left)
                       (multiple-value-bind (rw rg rb) (recur right)
                         (let ((ws (+ lw (%node-width node) rw))
                               (gs (+ lg (%node-slope-gap node) rg))
                               (breg (+ lb (* lw (%node-slope-gap node)) rb
                                        (* (+ lw (%node-width node)) rg))))
                           (unless (= ws (%node-width-sum node))
                             (error "validate: width-sum mismatch"))
                           (unless (= gs (%node-slope-gap-sum node))
                             (error "validate: slope-gap-sum mismatch"))
                           (unless (= breg (%node-bregman node))
                             (error "validate: bregman mismatch"))
                           (values ws gs breg))))))))
      (recur segments))
    (let ((first t))
      (labels ((walk (node)
                 (when node
                   (walk (%node-left node))
                   (cond (first
                          (unless (zerop (%node-slope-gap node))
                            (error "validate: leftmost slope-gap ~A is not 0"
                                   (%node-slope-gap node)))
                          (setq first nil))
                         (t
                          (unless (>= (%node-slope-gap node) 1)
                            (error "validate: non-increasing slopes"))))
                   (walk (%node-right node)))))
        (walk segments)))
    (unless segments
      (unless (zerop (%mstrick-min-slope f))
        (error "validate: empty tree with min-slope ~A" (%mstrick-min-slope f)))))
  t)

;;;
;;; Naive oracle: explicit breakpoint/slope lists, linear walks throughout.
;;; The discrete envelope operations are computed on dense integer samples --
;;; point absorption by a brute-force double discrete Legendre-Fenchel
;;; transform -- so they share no structure with the treap implementation.
;;;

(defstruct (pl (:constructor %make-pl (domain slopes anchor-value))
               (:copier nil)
               (:predicate nil))
  "Naive convex piecewise-linear function: DOMAIN is the ascending list of
breakpoints x_0 < ... < x_n, SLOPES the list (m_1 ... m_n) with m_j the slope
on [x_{j-1}, x_j], and ANCHOR-VALUE = f(x_0)."
  (domain nil :type list)
  (slopes nil :type list)
  (anchor-value 0 :type fixnum))

(defun make-pl-point (x y)
  (%make-pl (list x) nil y))

(defun pl-copy (pl)
  (%make-pl (copy-list (pl-domain pl)) (copy-list (pl-slopes pl))
            (pl-anchor-value pl)))

(defun pl-dom-min (pl) (first (pl-domain pl)))
(defun pl-dom-max (pl) (car (last (pl-domain pl))))

(defun pl-value (pl x)
  (let ((x0 (pl-dom-min pl))
        (xn (pl-dom-max pl)))
    (if (or (< x x0) (< xn x))
        +positive-inf+
        (let ((v (pl-anchor-value pl)))
          (loop for (l r) on (pl-domain pl)
                for m in (pl-slopes pl)
                do (if (<= x r)
                       (return (+ v (* m (- x l))))
                       (incf v (* m (- r l))))
                finally (return v))))))

(defun pl-graph-points (pl)
  "Returns the graph vertices as a list of (x . f(x)) conses."
  (let ((v (pl-anchor-value pl))
        (points nil))
    (push (cons (pl-dom-min pl) v) points)
    (loop for (l r) on (pl-domain pl)
          for m in (pl-slopes pl)
          do (incf v (* m (- r l)))
             (push (cons r v) points))
    (nreverse points)))

(defun pl-conj-value (pl p)
  (loop for (x . fx) in (pl-graph-points pl)
        maximize (- (* p x) fx)))

(defun pl-value-range (pl)
  (let ((values (mapcar #'cdr (pl-graph-points pl))))
    (- (reduce #'max values) (reduce #'min values))))

(defun pl-subdiff (pl x)
  (let ((x0 (pl-dom-min pl))
        (xn (pl-dom-max pl)))
    (cond ((< x x0) (values +negative-inf+ +negative-inf+))
          ((< xn x) (values +positive-inf+ +positive-inf+))
          (t
           (values (if (= x x0)
                       +negative-inf+
                       (loop for (l r) on (pl-domain pl)
                             for m in (pl-slopes pl)
                             when (and (< l x) (<= x r)) return m))
                   (if (= x xn)
                       +positive-inf+
                       (loop for (l r) on (pl-domain pl)
                             for m in (pl-slopes pl)
                             when (and (<= l x) (< x r)) return m)))))))

(defun pl-arg-subdiff (pl p)
  (let ((lt (count-if (lambda (m) (< m p)) (pl-slopes pl)))
        (le (count-if (lambda (m) (<= m p)) (pl-slopes pl))))
    (values (nth lt (pl-domain pl)) (nth le (pl-domain pl)))))

(defun pl-segments (pl)
  "Returns the segments as a fresh list of (slope . width) conses."
  (loop for (l r) on (pl-domain pl)
        for m in (pl-slopes pl)
        collect (cons m (- r l))))

(defun pl-rebuild (pl dom-min segments)
  "Relays the slope-sorted SEGMENTS out from DOM-MIN, dropping zero widths."
  (let ((domain (list dom-min))
        (slopes nil)
        (x dom-min))
    (loop for (m . w) in segments
          unless (zerop w)
            do (incf x w)
               (push m slopes)
               (push x domain))
    (setf (pl-domain pl) (nreverse domain)
          (pl-slopes pl) (nreverse slopes)))
  pl)

(defun pl-merge (pl)
  "Coalesces adjacent segments with equal slopes."
  (let ((new-domain (list (pl-dom-min pl)))
        (new-slopes nil))
    (loop for r in (rest (pl-domain pl))
          for m in (pl-slopes pl)
          do (if (and new-slopes (= m (first new-slopes)))
                 (setf (first new-domain) r)
                 (progn (push m new-slopes)
                        (push r new-domain))))
    (setf (pl-domain pl) (nreverse new-domain)
          (pl-slopes pl) (nreverse new-slopes)))
  pl)

(defun pl-splice-segment (pl slope width sign)
  (unless (zerop width)
    (let ((w-abs (abs width))
          (segments (pl-segments pl)))
      (if (plusp sign)
          (let ((cell (assoc slope segments)))
            (if cell
                (incf (cdr cell) w-abs)
                (setq segments (merge 'list (list (cons slope w-abs)) segments
                                      #'< :key #'car))))
          (let ((cell (find-if (lambda (c) (and (= (car c) slope)
                                                (<= w-abs (cdr c))))
                               segments)))
            (unless cell
              (error "pl-remove-segment: segment (~A, ~A) not stored" slope width))
            (decf (cdr cell) w-abs)))
      (when (minusp width)
        (incf (pl-anchor-value pl) (* sign slope width)))
      (let ((dom-min (if (plusp width)
                         (pl-dom-min pl)
                         (+ (pl-dom-min pl) (* sign width)))))
        (pl-rebuild pl dom-min segments)
        (pl-merge pl))))
  pl)

(defun pl-insert-segment (pl slope width)
  (pl-splice-segment pl slope width 1))

(defun pl-remove-segment (pl slope width)
  (pl-splice-segment pl slope width -1))

(defun pl-translate (pl delta)
  (setf (pl-domain pl) (mapcar (lambda (x) (+ x delta)) (pl-domain pl)))
  pl)

(defun pl-ensure-breakpoint (pl x)
  "Ensures a breakpoint at X (within the effective domain), returning its
index in DOMAIN."
  (let* ((domain (pl-domain pl))
         (idx (position-if (lambda (d) (<= x d)) domain)))
    (if (= x (nth idx domain))
        idx
        (progn
          (setf (pl-domain pl)
                (append (subseq domain 0 idx) (list x) (nthcdr idx domain))
                (pl-slopes pl)
                (append (subseq (pl-slopes pl) 0 idx)
                        (list (nth (1- idx) (pl-slopes pl)))
                        (nthcdr idx (pl-slopes pl))))
          idx))))

(defun pl-restrict-dom-max (pl c)
  (when (< c (pl-dom-max pl))
    (assert (<= (pl-dom-min pl) c))
    (let ((k (pl-ensure-breakpoint pl c)))
      (setf (pl-domain pl) (subseq (pl-domain pl) 0 (1+ k))
            (pl-slopes pl) (subseq (pl-slopes pl) 0 k))
      (pl-merge pl)))
  pl)

(defun pl-restrict-dom-min (pl c)
  (when (< (pl-dom-min pl) c)
    (assert (<= c (pl-dom-max pl)))
    (let ((k (pl-ensure-breakpoint pl c)))
      (setf (pl-anchor-value pl) (pl-value pl c)
            (pl-domain pl) (nthcdr k (pl-domain pl))
            (pl-slopes pl) (nthcdr k (pl-slopes pl)))
      (pl-merge pl)))
  pl)

(defun pl-add-kink (pl kink left-slope right-slope)
  (let ((x0 (pl-dom-min pl))
        (xn (pl-dom-max pl)))
    (when (and (< x0 kink) (< kink xn))
      (pl-ensure-breakpoint pl kink))
    (setf (pl-slopes pl)
          (loop for r in (rest (pl-domain pl))
                for m in (pl-slopes pl)
                collect (if (<= r kink)
                            (+ m left-slope)
                            (+ m right-slope))))
    (let ((anchor-rate (cond ((null (pl-slopes pl))
                              (if (< kink x0) right-slope left-slope))
                             ((<= (second (pl-domain pl)) kink) left-slope)
                             (t right-slope))))
      (incf (pl-anchor-value pl) (* anchor-rate (- x0 kink))))
    (pl-merge pl)))

(defun pl-inf-conv (pl other)
  (let ((segments (sort (append (pl-segments pl) (pl-segments other))
                        #'< :key #'car))
        (dom-min (+ (pl-dom-min pl) (pl-dom-min other))))
    (incf (pl-anchor-value pl) (pl-anchor-value other))
    (pl-rebuild pl dom-min segments)
    (pl-merge pl)))

(defun pl-pointwise-add (pl other)
  (let* ((g (pl-copy other))
         (lo (max (pl-dom-min pl) (pl-dom-min g)))
         (hi (min (pl-dom-max pl) (pl-dom-max g))))
    (assert (<= lo hi))
    (pl-restrict-dom-min pl lo)
    (pl-restrict-dom-max pl hi)
    (pl-restrict-dom-min g lo)
    (pl-restrict-dom-max g hi)
    (if (= lo hi)
        (setf (pl-domain pl) (list lo)
              (pl-slopes pl) nil
              (pl-anchor-value pl) (+ (pl-anchor-value pl) (pl-anchor-value g)))
        (let* ((cuts (sort (remove-duplicates
                            (append (pl-domain pl) (pl-domain g)))
                           #'<))
               (slopes (loop for (x next) on cuts
                             while next
                             collect (+ (nth-value 1 (pl-subdiff pl x))
                                        (nth-value 1 (pl-subdiff g x))))))
          (incf (pl-anchor-value pl) (pl-anchor-value g))
          (setf (pl-domain pl) cuts
                (pl-slopes pl) slopes)
          (pl-merge pl)))
    pl))

(defun pl-set-from-samples (pl dom-min values)
  "Rebuilds PL as the interpolant of the convex integer samples VALUES at
DOM-MIN, DOM-MIN + 1, ..."
  (setf (pl-anchor-value pl) (first values)
        (pl-domain pl) (loop for i from 0 below (length values)
                             collect (+ dom-min i))
        (pl-slopes pl) (loop for (a b) on values
                             while b
                             collect (- b a)))
  (pl-merge pl))

(defun pl-max-affine (pl a b)
  "Discrete pointwise max: sample max(f, line) on the integer grid of the
domain and interpolate."
  (let ((x0 (pl-dom-min pl)))
    (pl-set-from-samples
     pl x0
     (loop for z from x0 to (pl-dom-max pl)
           collect (max (pl-value pl z) (+ (* a z) b))))))

(defun pl-convex-hull-with-points (pl points)
  "Single-shot discrete point absorption of POINTS, a list of (x . y) conses:
the integer biconjugate of the min of the grid samples with the points,
computed by a brute-force double discrete Legendre-Fenchel transform over a
data-covering slope range."
  (let* ((lo (reduce #'min points :key #'car :initial-value (pl-dom-min pl)))
         (hi (reduce #'max points :key #'car :initial-value (pl-dom-max pl)))
         ;; The min-with-points samples; NIL encodes +inf.
         (m (coerce (loop for z from lo to hi
                          collect (let ((v (pl-value pl z)))
                                    (if (= v +positive-inf+) nil v)))
                    'vector)))
    (loop for (x . y) in points
          do (let ((cur (aref m (- x lo))))
               (setf (aref m (- x lo)) (if cur (min cur y) y))))
    (let* ((finite (loop for z from lo
                         for v across m
                         when v collect (cons z v)))
           (vlo (reduce #'min finite :key #'cdr))
           (vhi (reduce #'max finite :key #'cdr))
           (bound (+ (- vhi vlo) 2))
           (conj (loop for p from (- bound) to bound
                       collect (cons p (loop for (z . v) in finite
                                             maximize (- (* p z) v))))))
      (pl-set-from-samples
       pl lo
       (loop for z from lo to hi
             collect (loop for (p . c) in conj
                           maximize (- (* p z) c)))))))

(defun pl-convex-hull-with-point (pl x y)
  (pl-convex-hull-with-points pl (list (cons x y))))

;;;
;;; Comparison drivers
;;;

(defun check-against (f pl)
  "Full agreement check: structure, domain, values on a window spilling past
both ends, conjugate values on a bounded slope range."
  (validate f)
  (let ((dom-min (pl-dom-min pl))
        (dom-max (pl-dom-max pl)))
    (unless (= (mstrick-dom-min f) dom-min)
      (error "dom-min mismatch: ~A vs ~A" (mstrick-dom-min f) dom-min))
    (unless (= (mstrick-dom-max f) dom-max)
      (error "dom-max mismatch: ~A vs ~A" (mstrick-dom-max f) dom-max))
    (loop for z from (- dom-min 2) to (+ dom-max 2)
          do (let ((sv (mstrick-value f z))
                   (pv (pl-value pl z)))
               (unless (= sv pv)
                 (error "value(~A) mismatch: ~A vs ~A" z sv pv))))
    (let ((bound (min (+ (pl-value-range pl) 2) 60)))
      (loop for p from (- bound) to bound
            do (let ((sv (mstrick-conj-value f p))
                     (pv (pl-conj-value pl p)))
                 (unless (= sv pv)
                   (error "conj-value(~A) mismatch: ~A vs ~A" p sv pv)))))))

(defun check-values (f pl)
  (loop repeat 8
        do (let* ((p (- (random 25) 12))
                  (sv (mstrick-conj-value f p))
                  (pv (pl-conj-value pl p)))
             (unless (= sv pv)
               (error "conj-value(~A) mismatch: ~A vs ~A" p sv pv))))
  (loop repeat 10
        do (let* ((x (- (random 51) 25))
                  (sv (mstrick-value f x))
                  (pv (pl-value pl x)))
             (unless (= sv pv)
               (error "value(~A) mismatch: ~A vs ~A" x sv pv)))))

(defun check-subdiff (f pl)
  (loop repeat 10
        do (let ((x (- (random 43) 21)))
             (multiple-value-bind (sl sr) (mstrick-subdiff f x)
               (multiple-value-bind (nl nr) (pl-subdiff pl x)
                 (unless (and (= sl nl) (= sr nr))
                   (error "subdiff(~A) mismatch: (~A ~A) vs (~A ~A)" x sl sr nl nr))))))
  (loop repeat 10
        do (let ((p (- (random 43) 21)))
             (multiple-value-bind (sl sr) (mstrick-arg-subdiff f p)
               (multiple-value-bind (nl nr) (pl-arg-subdiff pl p)
                 (unless (and (= sl nl) (= sr nr))
                   (error "arg-subdiff(~A) mismatch: (~A ~A) vs (~A ~A)"
                          p sl sr nl nr)))))))

(defun random-start (&optional (max-inserts 5))
  "Builds the same random convex start function into both representations."
  (let* ((x0 (- (random 11) 5))
         (y0 (- (random 17) 8))
         (f (make-mstrick x0 y0))
         (pl (make-pl-point x0 y0)))
    (dotimes (_ (random max-inserts))
      (let ((slope (- (random 13) 6))
            (width (- (random 9) 4)))
        (mstrick-insert-segment f slope width)
        (pl-insert-segment pl slope width)))
    (values f pl)))

(defun snap-tree (node)
  "Deep-copies the treap into nested lists -- every field, aggregates and
priorities included -- for exact restoration checks after a rollback."
  (and node
       (list (%node-width node) (%node-slope-gap node) (%node-priority node)
             (%node-width-sum node) (%node-slope-gap-sum node)
             (%node-bregman node)
             (snap-tree (%node-left node)) (snap-tree (%node-right node)))))

(defun snap-state (f)
  (list (%mstrick-dom-min f) (%mstrick-anchor-value f)
        (%mstrick-min-slope f) (snap-tree (%mstrick-segments f))))

;;;
;;; Hand tests
;;;

(test multi-slope-trick/hand-point-and-segments
  ;; The single graph point (0, -5).
  (let ((f (make-mstrick 0 -5)))
    (is (= -5 (mstrick-value f 0)))
    (is (= +positive-inf+ (mstrick-value f 1)))
    (is (= +positive-inf+ (mstrick-value f -1)))
    ;; f*(p) = 0*p + 5.
    (is (equal '(5 5 5) (loop for p from -1 to 1 collect (mstrick-conj-value f p)))))
  ;; f*(p) = 3p for the point (3, 0).
  (let ((f (make-mstrick 3 0)))
    (is (equal '(-3 0 3) (loop for p from -1 to 1 collect (mstrick-conj-value f p)))))
  ;; Positive width: domain grows rightward, anchor unchanged.
  (let ((f (make-mstrick 0 0)))
    (mstrick-insert-segment f 2 1)
    (is (= 0 (mstrick-dom-min f)))
    (is (= 1 (mstrick-dom-max f)))
    (is (equal '(0 2) (vals f 0 1)))
    (validate f))
  ;; Negative width: domain grows leftward, f(x) = 2x on [-1, 0].
  (let ((f (make-mstrick 0 0)))
    (mstrick-insert-segment f 2 -1)
    (is (= -1 (mstrick-dom-min f)))
    (is (= 0 (mstrick-dom-max f)))
    (is (equal '(-2 0) (vals f -1 0)))
    (validate f))
  ;; Translation moves the domain, values ride along.
  (let ((f (make-mstrick 0 -10)))
    (mstrick-translate f 2)
    (is (= +positive-inf+ (mstrick-value f 0)))
    (is (= -10 (mstrick-value f 2)))))

(test multi-slope-trick/hand-worked-example
  ;; f: dom [0, 3], slope -5 on [0, 1], slope 0 on [1, 3], anchor f(0) = 0.
  (let ((f (make-mstrick 0 0)))
    (mstrick-insert-segment f -5 1)
    (mstrick-insert-segment f 0 2)
    (validate f)
    (is (equal '((-5 . 1) (0 . 2)) (segs f)))
    (is (equal '(0 -5 -5 -5) (vals f 0 3)))
    ;; f*(p) = max(0, p + 5, 3p + 5).
    (is (equal '(0 0 0 1 2 3 4 5 8 11)
               (loop for p from -7 to 2 collect (mstrick-conj-value f p))))
    (is (equal (list +negative-inf+ -5) (multiple-value-list (mstrick-subdiff f 0))))
    (is (equal '(-5 0) (multiple-value-list (mstrick-subdiff f 1))))
    (is (equal '(0 0) (multiple-value-list (mstrick-subdiff f 2))))
    (is (equal (list 0 +positive-inf+) (multiple-value-list (mstrick-subdiff f 3))))
    (is (equal (list +negative-inf+ +negative-inf+)
               (multiple-value-list (mstrick-subdiff f -1))))
    (is (equal (list +positive-inf+ +positive-inf+)
               (multiple-value-list (mstrick-subdiff f 4))))
    (is (equal '(0 0) (multiple-value-list (mstrick-arg-subdiff f -6))))
    (is (equal '(0 1) (multiple-value-list (mstrick-arg-subdiff f -5))))
    (is (equal '(1 1) (multiple-value-list (mstrick-arg-subdiff f -1))))
    (is (equal '(1 3) (multiple-value-list (mstrick-arg-subdiff f 0))))
    (is (equal '(3 3) (multiple-value-list (mstrick-arg-subdiff f 1))))))

(test multi-slope-trick/hand-remove-segment
  ;; Build f, perturb it with an extra segment, then remove it: bit-exact
  ;; decode restoration.
  (let ((f (make-mstrick 0 0)))
    (mstrick-insert-segment f 1 2)
    (mstrick-insert-segment f 3 1)
    (let ((before (segs f))
          (values-before (vals f 0 3)))
      (mstrick-insert-segment f 2 2)
      (mstrick-remove-segment f 2 2)
      (validate f)
      (is (equal before (segs f)))
      (is (equal values-before (vals f 0 3)))
      ;; Negative-width round trip.
      (mstrick-insert-segment f -4 -3)
      (mstrick-remove-segment f -4 -3)
      (validate f)
      (is (equal before (segs f)))
      (is (equal values-before (vals f 0 3)))))
  ;; Removing an absent segment errors.
  (let ((f (make-mstrick 0 0)))
    (mstrick-insert-segment f 1 2)
    (signals error (mstrick-remove-segment f 5 1))
    (signals error (mstrick-remove-segment f 1 3))))

(test multi-slope-trick/hand-restrict
  ;; V shape: dom [-2, 2], f = |x|.
  (flet ((make-v ()
           (let ((f (make-mstrick -2 2)))
             (mstrick-insert-segment f -1 2)
             (mstrick-insert-segment f 1 2)
             f)))
    (let* ((f (make-v))
           (rest (mstrick-restrict-dom-max f 1)))
      (is (= 1 (mstrick-dom-max f)))
      (is (equal '(2 1 0 1) (vals f -2 1)))
      (validate f)
      (mstrick-restrict-dom-max-rollback f rest)
      (validate f)
      (is (equal '(2 1 0 1 2) (vals f -2 2)))
      (is (equal '((-1 . 2) (1 . 2)) (segs f))))
    (let* ((f (make-v))
           (rest (mstrick-restrict-dom-min f 0)))
      (is (= 0 (mstrick-dom-min f)))
      (is (= 0 (%mstrick-anchor-value f)))
      (is (equal '(0 1 2) (vals f 0 2)))
      (validate f)
      (mstrick-restrict-dom-min-rollback f rest)
      (validate f)
      (is (equal '(2 1 0 1 2) (vals f -2 2)))
      (is (equal '((-1 . 2) (1 . 2)) (segs f))))
    ;; Collapse to a single point at either end, then roll back.
    (let* ((f (make-v))
           (rest (mstrick-restrict-dom-min f 2)))
      (is (= 2 (mstrick-dom-min f)))
      (is (= 2 (mstrick-dom-max f)))
      (is (= 2 (mstrick-value f 2)))
      (is (null (%mstrick-segments f)))
      (mstrick-restrict-dom-min-rollback f rest)
      (validate f)
      (is (equal '(2 1 0 1 2) (vals f -2 2))))
    (let* ((f (make-v))
           (rest (mstrick-restrict-dom-max f -2)))
      (is (= -2 (mstrick-dom-max f)))
      (is (= 2 (mstrick-value f -2)))
      (is (null (%mstrick-segments f)))
      (mstrick-restrict-dom-max-rollback f rest)
      (validate f)
      (is (equal '(2 1 0 1 2) (vals f -2 2))))
    ;; Nested restricts roll back in LIFO order.
    (let* ((f (make-v))
           (rest-min (mstrick-restrict-dom-min f -1))
           (rest-max (mstrick-restrict-dom-max f 1)))
      (is (equal '(1 0 1) (vals f -1 1)))
      (mstrick-restrict-dom-max-rollback f rest-max)
      (mstrick-restrict-dom-min-rollback f rest-min)
      (validate f)
      (is (equal '(2 1 0 1 2) (vals f -2 2)))
      (is (equal '((-1 . 2) (1 . 2)) (segs f))))
    ;; Beyond-domain restriction errors.
    (let ((f (make-v)))
      (signals error (mstrick-restrict-dom-max f -3))
      (signals error (mstrick-restrict-dom-min f 3)))))

(test multi-slope-trick/hand-add-kink
  ;; Right rate interior: f = 0 on [0, 2]; add max(0, 3(x - 1)).
  (let ((f (make-mstrick 0 0)))
    (mstrick-insert-segment f 0 2)
    (mstrick-add-kink f 1 0 3)
    (validate f)
    (is (equal '(0 0 3) (vals f 0 2)))
    (is (equal '((0 . 1) (3 . 1)) (segs f))))
  ;; Left rate interior: add max(0, -3(x - 1)).
  (let ((f (make-mstrick 0 0)))
    (mstrick-insert-segment f 0 2)
    (mstrick-add-kink f 1 -3 0)
    (validate f)
    (is (equal '(3 0 0) (vals f 0 2)))
    (is (equal '((-3 . 1) (0 . 1)) (segs f))))
  ;; Two-sided interior: f = 0 on [-1, 1]; add max(-2x, 3x).
  (let ((f (make-mstrick -1 0)))
    (mstrick-insert-segment f 0 2)
    (mstrick-add-kink f 0 -2 3)
    (validate f)
    (is (equal '(2 0 3) (vals f -1 1)))
    (is (equal '((-2 . 1) (3 . 1)) (segs f))))
  ;; Uniform rates: h is linear.
  (let ((f (make-mstrick 0 0)))
    (mstrick-insert-segment f 0 2)
    (mstrick-add-kink f 5 2 2)
    (validate f)
    (is (equal '(-10 -8 -6) (vals f 0 2))))
  ;; Kink left of the whole domain: only the right rate applies.
  (let ((f (make-mstrick 0 0)))
    (mstrick-insert-segment f 0 2)
    (mstrick-add-kink f -3 -1 2)
    (validate f)
    (is (equal '(6 8 10) (vals f 0 2))))
  ;; Kink at the domain ends.
  (let ((f (make-mstrick 0 0)))
    (mstrick-insert-segment f 0 2)
    (mstrick-add-kink f 2 1 5)
    (validate f)
    (is (equal '(-2 -1 0) (vals f 0 2))))
  (let ((f (make-mstrick 0 0)))
    (mstrick-insert-segment f 0 2)
    (mstrick-add-kink f 0 -4 1)
    (validate f)
    (is (equal '(0 1 2) (vals f 0 2))))
  ;; Point domain: h is read on the side of the kink the point falls on.
  (let ((f (make-mstrick 2 1)))
    (mstrick-add-kink f 3 2 7)
    (is (= -1 (mstrick-value f 2)))
    (mstrick-add-kink f 2 5 9)
    (is (= -1 (mstrick-value f 2)))
    (mstrick-add-kink f 0 5 9)
    (is (= 17 (mstrick-value f 2))))
  ;; Two kinks at the same position stack their slope gaps.
  (let ((f (make-mstrick 0 0)))
    (mstrick-insert-segment f 0 2)
    (mstrick-add-kink f 1 0 3)
    (mstrick-add-kink f 1 0 2)
    (validate f)
    (is (equal '(0 0 5) (vals f 0 2)))
    (is (equal '((0 . 1) (5 . 1)) (segs f))))
  ;; Negation rollback: perturbing kinks then adding their negations restores
  ;; the decode, fusing the split boundary back into one node.
  (let ((f (make-mstrick 0 0)))
    (mstrick-insert-segment f 0 4)
    (let ((before (segs f)))
      (mstrick-add-kink f 1 -2 3)
      (mstrick-add-kink f 3 -1 0)
      (mstrick-add-kink f 3 1 0)
      (mstrick-add-kink f 1 2 -3)
      (validate f)
      (is (equal before (segs f)))
      (is (equal '(0 0 0 0 0) (vals f 0 4))))))

(test multi-slope-trick/hand-inf-conv
  ;; f: anchor (1, 2), slopes 1 (width 2) then 4 (width 1) -- dom [1, 4];
  ;; g: anchor (0, 1), slope 2 (width 1) -- dom [0, 1].
  (let ((f (make-mstrick 1 2))
        (g (make-mstrick 0 1)))
    (mstrick-insert-segment f 1 2)
    (mstrick-insert-segment f 4 1)
    (mstrick-insert-segment g 2 1)
    (mstrick-inf-conv f g)
    (validate f)
    (is (= 1 (mstrick-dom-min f)))
    (is (= 5 (mstrick-dom-max f)))
    (is (equal '(3 4 5 7 11) (vals f 1 5)))
    (is (equal '((1 . 2) (2 . 1) (4 . 1)) (segs f))))
  ;; A point operand translates.
  (let ((f (make-mstrick 1 2))
        (g (make-mstrick 3 4)))
    (mstrick-inf-conv f g)
    (is (= 4 (mstrick-dom-min f)))
    (is (= 6 (mstrick-value f 4))))
  ;; Equal slopes fuse into a single node.
  (let ((f (make-mstrick 0 0))
        (g (make-mstrick 0 0)))
    (mstrick-insert-segment f 1 2)
    (mstrick-insert-segment g 1 3)
    (mstrick-inf-conv f g)
    (validate f)
    (is (equal '((1 . 5)) (segs f)))))

(test multi-slope-trick/hand-pointwise-add
  ;; f: anchor (1, 2), slopes 1 on [1, 3], 4 on [3, 4]; g: anchor (0, 1),
  ;; slope 2 on [0, 3]. Common window [1, 3].
  (let ((f (make-mstrick 1 2))
        (g (make-mstrick 0 1)))
    (mstrick-insert-segment f 1 2)
    (mstrick-insert-segment f 4 1)
    (mstrick-insert-segment g 2 3)
    (mstrick-pointwise-add f g)
    (validate f)
    (is (= 1 (mstrick-dom-min f)))
    (is (= 3 (mstrick-dom-max f)))
    (is (equal '(5 8 11) (vals f 1 3))))
  ;; A point operand collapses the sum to that point.
  (let ((f (make-mstrick 1 2))
        (g (make-mstrick 2 5)))
    (mstrick-insert-segment f 1 2)
    (mstrick-pointwise-add f g)
    (is (= 2 (mstrick-dom-min f)))
    (is (= 2 (mstrick-dom-max f)))
    (is (= 8 (mstrick-value f 2)))
    (is (null (%mstrick-segments f))))
  ;; Disjoint effective domains error.
  (let ((f (make-mstrick 0 0))
        (g (make-mstrick 5 0)))
    (signals error (mstrick-pointwise-add f g))))

(test multi-slope-trick/hand-max-affine
  ;; f = 0 on [0, 3]; the line x - 3 improves nothing strictly at integers
  ;; (weak tie at x = 3) -- no-op.
  (let ((f (make-mstrick 0 0)))
    (mstrick-insert-segment f 0 3)
    (mstrick-max-affine f 1 -3)
    (is (equal '(0 0 0 0) (vals f 0 3)))
    ;; The line x - 1 strictly improves x = 2 and x = 3.
    (mstrick-max-affine f 1 -1)
    (validate f)
    (is (equal '(0 0 1 2) (vals f 0 3))))
  ;; f = |x| on [-2, 2]; max with the constant 1: only x = 0 improves.
  (let ((f (make-mstrick -2 2)))
    (mstrick-insert-segment f -1 2)
    (mstrick-insert-segment f 1 2)
    (mstrick-max-affine f 0 1)
    (validate f)
    (is (equal '(2 1 1 1 2) (vals f -2 2)))
    (is (equal '((-1 . 1) (0 . 2) (1 . 1)) (segs f)))
    ;; Tangent line (tie only): no-op.
    (let ((before (segs f)))
      (mstrick-max-affine f 1 -1)
      (is (equal before (segs f)))))
  ;; The constant 2 ties at both ends of |x| and improves the interior.
  (let ((f (make-mstrick -2 2)))
    (mstrick-insert-segment f -1 2)
    (mstrick-insert-segment f 1 2)
    (mstrick-max-affine f 0 2)
    (validate f)
    (is (= 2 (%mstrick-anchor-value f)))
    (is (equal '(2 2 2 2 2) (vals f -2 2)))
    (is (equal '((0 . 4)) (segs f))))
  ;; The line -x + 1 coincides with the left arm of |x| + 1: improving
  ;; integers are [-2, 0], the right glue has slope 0.
  (let ((f (make-mstrick -2 2)))
    (mstrick-insert-segment f -1 2)
    (mstrick-insert-segment f 1 2)
    (mstrick-max-affine f -1 1)
    (validate f)
    (is (equal '(3 2 1 1 2) (vals f -2 2)))
    (is (equal '((-1 . 2) (0 . 1) (1 . 1)) (segs f)))
    (is (= 3 (%mstrick-anchor-value f))))
  ;; Single-point domain.
  (let ((f (make-mstrick 2 -1)))
    (mstrick-max-affine f 3 0)
    (is (= 6 (mstrick-value f 2)))
    (mstrick-max-affine f -1 0)
    (is (= 6 (mstrick-value f 2)))))

(test multi-slope-trick/hand-convex-hull-with-point
  ;; f = 0 on [0, 3], absorb (3, -1): the integer biconjugate below the -1/3
  ;; chord.
  (let ((f (make-mstrick 0 0)))
    (mstrick-insert-segment f 0 3)
    (mstrick-convex-hull-with-point f 3 -1)
    (validate f)
    (is (equal '(0 -1 -1 -1) (vals f 0 3)))
    (is (equal '((-1 . 1) (0 . 2)) (segs f))))
  ;; Point + point: hulling (0, 0) with (2w, w) gives max(0, x - w).
  (let ((f (make-mstrick 0 0)))
    (mstrick-convex-hull-with-point f 6 3)
    (validate f)
    (is (equal '(0 0 0 0 1 2 3) (vals f 0 6)))
    (is (equal '((0 . 3) (1 . 3)) (segs f))))
  ;; Integer chord slopes: exact, single segment, no glue.
  (let ((f (make-mstrick 0 0)))
    (mstrick-convex-hull-with-point f 4 8)
    (is (equal '((2 . 4)) (segs f))))
  (let ((f (make-mstrick 0 0)))
    (mstrick-convex-hull-with-point f -3 3)
    (is (= -3 (mstrick-dom-min f)))
    (is (= 3 (%mstrick-anchor-value f)))
    (is (equal '((-1 . 3)) (segs f))))
  ;; Single improving slope: the two inner splice nodes fuse into the vertex
  ;; at x = 1.
  (let ((f (make-mstrick 0 0)))
    (mstrick-insert-segment f 0 2)
    (mstrick-convex-hull-with-point f 1 -1)
    (validate f)
    (is (equal '(0 -1 0) (vals f 0 2)))
    (is (equal '((-1 . 1) (1 . 1)) (segs f))))
  ;; Domain extensions.
  (let ((f (make-mstrick 0 0)))
    (mstrick-insert-segment f 0 2)
    (mstrick-convex-hull-with-point f 4 1)
    (validate f)
    (is (= 4 (mstrick-dom-max f)))
    (is (equal '(0 0 0 0 1) (vals f 0 4))))
  (let ((f (make-mstrick 0 0)))
    (mstrick-insert-segment f 0 2)
    (mstrick-convex-hull-with-point f -3 -2)
    (validate f)
    (is (= -3 (mstrick-dom-min f)))
    (is (= -2 (%mstrick-anchor-value f)))
    (is (equal '(-2 -2 -2 -2 -1 0) (vals f -3 2)))
    (is (equal '((0 . 3) (1 . 2)) (segs f))))
  ;; Boundary case x = dom-min with y below the anchor.
  (let ((f (make-mstrick 0 0)))
    (mstrick-insert-segment f 1 2)
    (mstrick-convex-hull-with-point f 0 -2)
    (validate f)
    (is (= 0 (mstrick-dom-min f)))
    (is (= -2 (%mstrick-anchor-value f)))
    (is (equal '(-2 0 2) (vals f 0 2))))
  ;; Mirror boundary: x = dom-max with y below f(dom-max); the -1/2 chord
  ;; rounds down at x = 1.
  (let ((f (make-mstrick 0 0)))
    (mstrick-insert-segment f 1 2)
    (mstrick-convex-hull-with-point f 2 -1)
    (validate f)
    (is (= 2 (mstrick-dom-max f)))
    (is (equal '(0 -1 -1) (vals f 0 2))))
  ;; A point on or above the graph changes nothing.
  (let ((f (make-mstrick 0 0)))
    (mstrick-insert-segment f 1 3)
    (let ((before (segs f)))
      (mstrick-convex-hull-with-point f 2 2)
      (mstrick-convex-hull-with-point f 1 5)
      (is (equal before (segs f)))
      (is (= 0 (mstrick-dom-min f)))
      (is (= 0 (%mstrick-anchor-value f))))))

;;;
;;; Randomized tests against the naive oracle
;;;

(test multi-slope-trick/random-core-ops
  (let ((*random-state* (sb-ext:seed-random-state 0))
        (*test-dribble* nil))
    (finishes
      (dotimes (_ 5000)
        (let* ((init-slope (- (random 11) 5))
               (f (make-mstrick init-slope 0))
               (pl (make-pl-point init-slope 0))
               (add-history nil)
               ;; Kinks still exactly invertible by the negated call: domain
               ;; restrictions and other kinks keep every breakpoint position,
               ;; while INSERT-SEGMENT/REMOVE-SEGMENT move breakpoints (the
               ;; history is cleared there) and TRANSLATE shifts them (the
               ;; history shifts too).
               (kink-history nil))
          (dotimes (_ 100)
            (case (random 19)
              ((0 1 2 3 4 5)
               (let ((a (- (random 21) 10))
                     (width (- (random 21) 10)))
                 (mstrick-insert-segment f a width)
                 (pl-insert-segment pl a width)
                 (push (cons a width) add-history)
                 (setq kink-history nil)))
              ((6)
               (let ((delta (- (random 21) 10)))
                 (mstrick-translate f delta)
                 (pl-translate pl delta)
                 (setq kink-history
                       (loop for (kink l r) in kink-history
                             collect (list (+ kink delta) l r)))))
              ((7 8)
               (let ((rollback-p (zerop (random 3))))
                 (if (zerop (random 2))
                     (let* ((c (+ (mstrick-dom-min f) (random 15)))
                            (rest (mstrick-restrict-dom-max f c)))
                       (if rollback-p
                           (mstrick-restrict-dom-max-rollback f rest)
                           (progn
                             (pl-restrict-dom-max pl c)
                             (setq add-history nil))))
                     (let* ((c (- (mstrick-dom-max f) (random 15)))
                            (rest (mstrick-restrict-dom-min f c)))
                       (if rollback-p
                           (mstrick-restrict-dom-min-rollback f rest)
                           (progn
                             (pl-restrict-dom-min pl c)
                             (setq add-history nil)))))))
              ((9 10)
               (check-values f pl))
              ((11 12)
               (check-subdiff f pl))
              ((13 14)
               (when add-history
                 (let* ((idx (random (length add-history)))
                        (entry (nth idx add-history)))
                   (mstrick-remove-segment f (car entry) (cdr entry))
                   (pl-remove-segment pl (car entry) (cdr entry))
                   (setq add-history
                         (append (subseq add-history 0 idx)
                                 (nthcdr (1+ idx) add-history))
                         kink-history nil))))
              ((15 16)
               ;; ADD-KINK shifts segment slopes, invalidating the recorded
               ;; REMOVE-SEGMENT keys.
               (let* ((kink (+ (mstrick-dom-min f) (- (random 22) 3)))
                      (a (- (random 21) 10))
                      (b (- (random 21) 10))
                      (l (min a b))
                      (r (max a b)))
                 (mstrick-add-kink f kink l r)
                 (pl-add-kink pl kink l r)
                 (push (list kink l r) kink-history)
                 (setq add-history nil)))
              ((17 18)
               ;; Undo a recorded kink by adding its negation.
               (when kink-history
                 (let* ((idx (random (length kink-history)))
                        (entry (nth idx kink-history)))
                   (destructuring-bind (kink l r) entry
                     (mstrick-add-kink f kink (- l) (- r))
                     (pl-add-kink pl kink (- l) (- r)))
                   (setq kink-history
                         (append (subseq kink-history 0 idx)
                                 (nthcdr (1+ idx) kink-history))
                         add-history nil))))))
          (validate f)
          (check-values f pl)
          (check-subdiff f pl))))))

(test multi-slope-trick/random-restrict-rollback
  ;; Restrict + rollback round trips restore the decode and fields exactly,
  ;; including nested LIFO pairs.
  (let ((*random-state* (sb-ext:seed-random-state 1))
        (*test-dribble* nil))
    (finishes
      (dotimes (_ 5000)
        (multiple-value-bind (f pl) (random-start 8)
          (declare (ignore pl))
          (let ((dom-min (mstrick-dom-min f))
                (dom-max (mstrick-dom-max f))
                (anchor (%mstrick-anchor-value f))
                (min-slope (%mstrick-min-slope f))
                (decode (segs f)))
            (flet ((check-restored ()
                     (validate f)
                     (unless (and (= dom-min (mstrick-dom-min f))
                                  (= dom-max (mstrick-dom-max f))
                                  (= anchor (%mstrick-anchor-value f))
                                  (= min-slope (%mstrick-min-slope f))
                                  (equal decode (segs f)))
                       (error "rollback did not restore the function"))))
              (let ((span (- dom-max dom-min)))
                (let ((rest (mstrick-restrict-dom-max
                             f (+ dom-min (random (1+ span))))))
                  (validate f)
                  (mstrick-restrict-dom-max-rollback f rest)
                  (check-restored))
                (let ((rest (mstrick-restrict-dom-min
                             f (+ dom-min (random (1+ span))))))
                  (validate f)
                  (mstrick-restrict-dom-min-rollback f rest)
                  (check-restored))
                (let* ((lo (+ dom-min (random (1+ span))))
                       (hi (+ lo (random (1+ (- dom-max lo)))))
                       (rest-min (mstrick-restrict-dom-min f lo))
                       (rest-max (mstrick-restrict-dom-max f hi)))
                  (validate f)
                  (mstrick-restrict-dom-max-rollback f rest-max)
                  (mstrick-restrict-dom-min-rollback f rest-min)
                  (check-restored))))))))))

(test multi-slope-trick/random-inf-conv-rollback
  ;; The journaled union merges exactly like the plain one, and the rollback
  ;; restores both operands exactly -- shape, priorities, and every field.
  (let ((*random-state* (sb-ext:seed-random-state 13))
        (*test-dribble* nil))
    (finishes
      (dotimes (_ 3000)
        (multiple-value-bind (f pl) (random-start 6)
          (multiple-value-bind (g pl-g) (random-start 6)
            (let ((state-f (snap-state f))
                  (state-g (snap-state g))
                  (token (mstrick-inf-conv-with-rollback f g)))
              (pl-inf-conv pl pl-g)
              (check-against f pl)
              (let ((other (mstrick-inf-conv-rollback f token)))
                (unless (eq other g)
                  (error "rollback did not return the consumed operand"))
                (validate f)
                (validate g)
                (unless (and (equal state-f (snap-state f))
                             (equal state-g (snap-state g)))
                  (error "inf-conv rollback did not restore the operands"))))))))))

(test multi-slope-trick/random-pointwise-add-rollback
  (let ((*random-state* (sb-ext:seed-random-state 14))
        (*test-dribble* nil))
    (finishes
      (dotimes (_ 3000)
        (multiple-value-bind (f pl) (random-start 6)
          (multiple-value-bind (g pl-g) (random-start 6)
            ;; Slide G onto F's domain so the windows overlap.
            (let ((delta (- (+ (mstrick-dom-min f)
                               (random (1+ (- (mstrick-dom-max f)
                                              (mstrick-dom-min f)))))
                            (mstrick-dom-min g))))
              (mstrick-translate g delta)
              (pl-translate pl-g delta))
            (let ((state-f (snap-state f))
                  (state-g (snap-state g))
                  (token (mstrick-pointwise-add-with-rollback f g)))
              (pl-pointwise-add pl pl-g)
              (check-against f pl)
              (let ((other (mstrick-pointwise-add-rollback f token)))
                (unless (eq other g)
                  (error "rollback did not return the consumed operand"))
                (validate f)
                (validate g)
                (unless (and (equal state-f (snap-state f))
                             (equal state-g (snap-state g)))
                  (error "pointwise-add rollback did not restore the operands"))))))))))

(test multi-slope-trick/random-nested-rollback
  ;; A stack of journaled merges, restricts, and translates unwound in LIFO
  ;; order -- the usage pattern of a divide-and-conquer consumer -- restores
  ;; every intermediate state exactly.
  (let ((*random-state* (sb-ext:seed-random-state 15))
        (*test-dribble* nil))
    (finishes
      (dotimes (_ 1500)
        (multiple-value-bind (f pl) (random-start 8)
          (declare (ignore pl))
          (let ((states (list (snap-state f)))
                (stack nil))
            (dotimes (_ 5)
              (multiple-value-bind (g pl-g) (random-start 5)
                (declare (ignore pl-g))
                (ecase (random 4)
                  (0 (push (list :inf-conv g (snap-state g)
                                 (mstrick-inf-conv-with-rollback f g))
                           stack))
                  (1 (mstrick-translate
                      g (- (+ (mstrick-dom-min f)
                              (random (1+ (- (mstrick-dom-max f)
                                             (mstrick-dom-min f)))))
                           (mstrick-dom-min g)))
                     (push (list :pointwise-add g (snap-state g)
                                 (mstrick-pointwise-add-with-rollback f g))
                           stack))
                  (2 (let ((c (+ (mstrick-dom-min f)
                                 (random (1+ (- (mstrick-dom-max f)
                                                (mstrick-dom-min f)))))))
                       (push (list :restrict nil nil
                                   (mstrick-restrict-dom-min f c))
                             stack)))
                  (3 (let ((delta (- (random 9) 4)))
                       (mstrick-translate f delta)
                       (push (list :translate nil nil delta) stack))))
                (push (snap-state f) states)))
            (pop states)
            (dolist (entry stack)
              (destructuring-bind (kind g state-g token) entry
                (ecase kind
                  (:inf-conv
                   (let ((other (mstrick-inf-conv-rollback f token)))
                     (unless (and (eq other g) (equal state-g (snap-state g)))
                       (error "nested inf-conv rollback failed"))))
                  (:pointwise-add
                   (let ((other (mstrick-pointwise-add-rollback f token)))
                     (unless (and (eq other g) (equal state-g (snap-state g)))
                       (error "nested pointwise-add rollback failed"))))
                  (:restrict (mstrick-restrict-dom-min-rollback f token))
                  (:translate (mstrick-translate f (- token))))
                (unless (equal (pop states) (snap-state f))
                  (error "nested rollback did not restore the function"))))))))))

(test multi-slope-trick/repeated-interior-splits-stay-balanced
  ;; Each ADD-KINK cuts the rightmost wide segment strictly inside. The cut
  ;; fragment must draw a fresh priority: inheriting the cut node's priority
  ;; would accumulate an equal-priority run that concatenation arranges as a
  ;; chain, degrading the treap to a list.
  (let ((*random-state* (sb-ext:seed-random-state 16))
        (*test-dribble* nil))
    (finishes
      (let* ((n 2000)
             (f (make-mstrick 0 0)))
        (mstrick-insert-segment f 0 (+ (* 2 n) 1))
        (loop for k from 1 to n
              do (mstrick-add-kink f k 0 1))
        (validate f)
        ;; f(x) = sum over k in [1, N] of max(0, x - k) on [0, 2N + 1].
        (dolist (x (list 0 1 2 100 n (* 2 n)))
          (let ((expected (loop for k from 1 to n sum (max 0 (- x k)))))
            (unless (= (mstrick-value f x) expected)
              (error "value(~A) mismatch after repeated kinks" x))))
        (labels ((depth (node)
                   (if (null node)
                       0
                       (+ 1 (max (depth (%node-left node))
                                 (depth (%node-right node)))))))
          (let ((depth (depth (%mstrick-segments f))))
            ;; About 2000 nodes: the expected treap depth is a few dozen,
            ;; while an equal-priority chain would reach the node count.
            (unless (<= depth 120)
              (error "degenerate treap: depth ~A after ~A kinks" depth n))))))))

(test multi-slope-trick/random-inf-conv
  (let ((*random-state* (sb-ext:seed-random-state 2))
        (*test-dribble* nil))
    (finishes
      (dotimes (_ 1500)
        (multiple-value-bind (f pl) (random-start 6)
          (multiple-value-bind (g pl-g) (random-start 6)
            (mstrick-inf-conv f g)
            (pl-inf-conv pl pl-g)
            (check-against f pl)))))))

(test multi-slope-trick/random-inf-conv-cascade
  ;; Fold several operands so unions hit trees built by earlier unions.
  (let ((*random-state* (sb-ext:seed-random-state 3))
        (*test-dribble* nil))
    (finishes
      (dotimes (_ 300)
        (multiple-value-bind (f pl) (random-start 6)
          (dotimes (_ 6)
            (multiple-value-bind (g pl-g) (random-start 6)
              (mstrick-inf-conv f g)
              (pl-inf-conv pl pl-g)))
          (check-against f pl))))))

(test multi-slope-trick/random-pointwise-add
  (let ((*random-state* (sb-ext:seed-random-state 4))
        (*test-dribble* nil))
    (finishes
      (dotimes (_ 1500)
        (multiple-value-bind (f pl) (random-start 6)
          (multiple-value-bind (g pl-g) (random-start 6)
            ;; Slide G onto F's domain so the windows overlap.
            (let ((delta (- (+ (mstrick-dom-min f)
                               (random (1+ (- (mstrick-dom-max f)
                                              (mstrick-dom-min f)))))
                            (mstrick-dom-min g))))
              (mstrick-translate g delta)
              (pl-translate pl-g delta))
            (mstrick-pointwise-add f g)
            (pl-pointwise-add pl pl-g)
            (check-against f pl)))))))

(test multi-slope-trick/random-pointwise-add-cascade
  ;; Cascaded sums over one shared window keep refining the same tree.
  (let ((*random-state* (sb-ext:seed-random-state 5))
        (*test-dribble* nil))
    (finishes
      (dotimes (_ 300)
        (let ((f (make-mstrick 0 0))
              (pl (make-pl-point 0 0)))
          (mstrick-insert-segment f 0 12)
          (pl-insert-segment pl 0 12)
          (dotimes (_ 6)
            ;; Start the operand inside F's current window so the domains
            ;; always overlap.
            (let* ((x0 (+ (mstrick-dom-min f)
                          (random (1+ (- (mstrick-dom-max f)
                                         (mstrick-dom-min f))))))
                   (g (make-mstrick x0 (- (random 9) 4)))
                   (pl-g (make-pl-point x0 (%mstrick-anchor-value g))))
              (dotimes (_ (random 4))
                (let ((slope (- (random 13) 6))
                      (width (1+ (random 3))))
                  (mstrick-insert-segment g slope width)
                  (pl-insert-segment pl-g slope width)))
              (mstrick-pointwise-add f g)
              (pl-pointwise-add pl pl-g)))
          (check-against f pl))))))

(test multi-slope-trick/random-conj-intercept
  ;; The two conjugate-vertex descents match a linear scan over the decode.
  (let ((*random-state* (sb-ext:seed-random-state 6))
        (*test-dribble* nil))
    (flet ((ref-leftmost (f x y)
             (let ((dom-min (mstrick-dom-min f))
                   (cum 0))
               (loop for (key . w) in (segs f)
                     do (let ((left-adj (+ dom-min cum)))
                          (when (<= x left-adj)
                            (return nil))
                          (let ((fstar (mstrick-conj-value f key)))
                            (when (<= fstar (- (* x key) y))
                              (return (list key fstar left-adj)))))
                        (incf cum w)
                     finally (return nil))))
           (ref-rightmost (f x y)
             (let* ((dom-min (mstrick-dom-min f))
                    (all (segs f))
                    (cum (reduce #'+ all :key #'cdr)))
               (loop for (key . w) in (reverse all)
                     do (let ((right-adj (+ dom-min cum)))
                          (when (<= right-adj x)
                            (return nil))
                          (let ((fstar (mstrick-conj-value f key)))
                            (when (<= fstar (- (* x key) y))
                              (return (list key fstar right-adj)))))
                        (decf cum w)
                     finally (return nil)))))
      (finishes
        (dotimes (_ 5000)
          (multiple-value-bind (f pl) (random-start 8)
            (declare (ignore pl))
            (dotimes (_ 4)
              (let ((x (+ (mstrick-dom-min f) (- (random 12) 3)))
                    (y (- (random 33) 16)))
                (let ((got (multiple-value-bind (key fstar adj)
                               (conj-intercept-leftmost
                                (%mstrick-segments f) x y
                                (%mstrick-dom-min f)
                                (%mstrick-anchor-value f)
                                (%mstrick-min-slope f))
                             (and key (list key fstar adj))))
                      (want (ref-leftmost f x y)))
                  (unless (equal got want)
                    (error "conj-intercept-leftmost(~A, ~A): ~A vs ~A"
                           x y got want)))
                (let ((got (multiple-value-bind (key fstar adj)
                               (conj-intercept-rightmost
                                (%mstrick-segments f) x y
                                (%mstrick-dom-min f)
                                (%mstrick-anchor-value f)
                                (%mstrick-min-slope f))
                             (and key (list key fstar adj))))
                      (want (ref-rightmost f x y)))
                  (unless (equal got want)
                    (error "conj-intercept-rightmost(~A, ~A): ~A vs ~A"
                           x y got want)))))))))))

(test multi-slope-trick/random-max-affine
  (let ((*random-state* (sb-ext:seed-random-state 7))
        (*test-dribble* nil))
    (finishes
      (dotimes (_ 1000)
        (multiple-value-bind (f pl) (random-start)
          (dotimes (_ (1+ (random 7)))
            (let ((a (- (random 19) 9))
                  (b (- (random 29) 14)))
              (mstrick-max-affine f a b)
              (pl-max-affine pl a b)
              (check-against f pl))))))))

(test multi-slope-trick/random-convex-hull-with-point
  (let ((*random-state* (sb-ext:seed-random-state 8))
        (*test-dribble* nil))
    (finishes
      (dotimes (_ 1000)
        (multiple-value-bind (f pl) (random-start)
          (dotimes (_ (1+ (random 7)))
            (let ((x (+ (pl-dom-min pl)
                        (- (random (+ (- (pl-dom-max pl) (pl-dom-min pl)) 9)) 4)))
                  (y (- (random 33) 16)))
              (mstrick-convex-hull-with-point f x y)
              (pl-convex-hull-with-point pl x y)
              (check-against f pl))))))))

(test multi-slope-trick/random-mixed-ops
  (let ((*random-state* (sb-ext:seed-random-state 9))
        (*test-dribble* nil))
    (finishes
      (dotimes (_ 300)
        (multiple-value-bind (f pl) (random-start)
          (dotimes (_ 25)
            (case (random 12)
              ((0 1 2)
               (let ((a (- (random 19) 9))
                     (b (- (random 29) 14)))
                 (mstrick-max-affine f a b)
                 (pl-max-affine pl a b)))
              ((3 4 5)
               (let ((x (+ (pl-dom-min pl)
                           (- (random (+ (- (pl-dom-max pl) (pl-dom-min pl)) 7))
                              3)))
                     (y (- (random 33) 16)))
                 (mstrick-convex-hull-with-point f x y)
                 (pl-convex-hull-with-point pl x y)))
              ((6)
               (let ((slope (- (random 13) 6))
                     (width (- (random 7) 3)))
                 (mstrick-insert-segment f slope width)
                 (pl-insert-segment pl slope width)))
              ((7)
               (let* ((kink (+ (pl-dom-min pl)
                               (- (random (+ (- (pl-dom-max pl) (pl-dom-min pl)) 5))
                                  2)))
                      (a (- (random 11) 5))
                      (b (- (random 11) 5)))
                 (mstrick-add-kink f kink (min a b) (max a b))
                 (pl-add-kink pl kink (min a b) (max a b))))
              ((8)
               (let ((delta (- (random 11) 5)))
                 (mstrick-translate f delta)
                 (pl-translate pl delta)))
              ((9)
               (let ((c (+ (pl-dom-min pl)
                           (random (1+ (- (pl-dom-max pl) (pl-dom-min pl)))))))
                 (mstrick-restrict-dom-min f c)
                 (pl-restrict-dom-min pl c)))
              ((10)
               (let ((c (+ (pl-dom-min pl)
                           (random (1+ (- (pl-dom-max pl) (pl-dom-min pl)))))))
                 (mstrick-restrict-dom-max f c)
                 (pl-restrict-dom-max pl c)))
              ((11)
               ;; Binary op with a small fresh operand.
               (when (< (- (pl-dom-max pl) (pl-dom-min pl)) 25)
                 (multiple-value-bind (g pl-g) (random-start)
                   (if (zerop (random 2))
                       (progn (mstrick-inf-conv f g)
                              (pl-inf-conv pl pl-g))
                       (let ((lo (max (pl-dom-min pl) (pl-dom-min pl-g)))
                             (hi (min (pl-dom-max pl) (pl-dom-max pl-g))))
                         (when (<= lo hi)
                           (mstrick-pointwise-add f g)
                           (pl-pointwise-add pl pl-g))))))))
            ;; Keep the window bounded so the dense oracle stays cheap.
            (when (> (- (pl-dom-max pl) (pl-dom-min pl)) 60)
              (let ((c (+ (pl-dom-min pl) 40)))
                (mstrick-restrict-dom-max f c)
                (pl-restrict-dom-max pl c)))
            (check-against f pl)))))))

(test multi-slope-trick/max-affine-order-independence
  (let ((*random-state* (sb-ext:seed-random-state 10))
        (*test-dribble* nil))
    (finishes
      (dotimes (_ 500)
        (let* ((x0 (- (random 9) 4))
               (y0 (- (random 13) 6))
               (base (loop repeat (random 4)
                           collect (cons (- (random 13) 6) (1+ (random 4)))))
               (lines (loop repeat (1+ (random 5))
                            collect (cons (- (random 17) 8) (- (random 25) 12)))))
          (flet ((build (lines)
                   (let ((f (make-mstrick x0 y0)))
                     (loop for (s . w) in base
                           do (mstrick-insert-segment f s w))
                     (loop for (a . b) in lines
                           do (mstrick-max-affine f a b))
                     f)))
            (let ((f1 (build lines))
                  (f2 (build (coerce (shuffle! (coerce lines 'vector)) 'list))))
              (unless (and (= (mstrick-dom-min f1) (mstrick-dom-min f2))
                           (= (mstrick-dom-max f1) (mstrick-dom-max f2)))
                (error "max-affine order dependence in the domain"))
              (loop for z from (mstrick-dom-min f1) to (mstrick-dom-max f1)
                    unless (= (mstrick-value f1 z) (mstrick-value f2 z))
                      do (error "max-affine order dependence at ~A" z)))))))))

(test multi-slope-trick/convex-hull-confluence
  ;; Sequential absorption equals the single-shot multi-point transform.
  (let ((*random-state* (sb-ext:seed-random-state 11))
        (*test-dribble* nil))
    (finishes
      (dotimes (_ 500)
        (multiple-value-bind (f pl) (random-start)
          (let ((points (loop repeat (1+ (random 5))
                              collect (cons (+ (pl-dom-min pl)
                                               (- (random
                                                   (+ (- (pl-dom-max pl)
                                                         (pl-dom-min pl))
                                                      9))
                                                  4))
                                            (- (random 33) 16)))))
            (loop for (x . y) in points
                  do (mstrick-convex-hull-with-point f x y))
            (pl-convex-hull-with-points pl points)
            (check-against f pl)))))))

(test multi-slope-trick/discrete-bound-preservation
  (let ((*random-state* (sb-ext:seed-random-state 12))
        (*test-dribble* nil))
    (finishes
      (dotimes (_ 500)
        ;; Ground truth V: a random convex integer sequence on [d0, d1].
        (let* ((d0 (- (random 11) 5))
               (n (+ 2 (random 10)))
               (slope (- (random 5) 4))
               (values (list (- (random 13) 6))))
          (dotimes (_ n)
            (push (+ (first values) slope) values)
            (incf slope (random 3)))
          (let* ((v (coerce (nreverse values) 'vector))
                 (d1 (+ d0 n))
                 (v-at (lambda (z) (aref v (- z d0)))))
            ;; Lower side: cuts below V never push the function above V.
            (let* ((vmin (reduce #'min v))
                   (lower (make-mstrick d0 (- vmin 5))))
              (mstrick-insert-segment lower 0 (- d1 d0))
              (dotimes (_ (1+ (random 5)))
                (let* ((z (+ d0 (random (1+ (- d1 d0)))))
                       (a (if (= z d1)
                              (- (funcall v-at z) (funcall v-at (- z 1)))
                              (- (funcall v-at (+ z 1)) (funcall v-at z))))
                       (b (- (funcall v-at z) (* a z) (random 4))))
                  (mstrick-max-affine lower a b)
                  (loop for zz from d0 to d1
                        unless (<= (mstrick-value lower zz) (funcall v-at zz))
                          do (error "lower bound violated at ~A" zz)))))
            ;; Upper side: absorbing points above V keeps the function above V.
            (let* ((xc (+ d0 (random (1+ (- d1 d0)))))
                   (upper (make-mstrick xc (+ (funcall v-at xc) (random 4)))))
              (dotimes (_ (1+ (random 7)))
                (let ((x (+ d0 (random (1+ (- d1 d0)))))
                      (y-extra (random 5)))
                  (mstrick-convex-hull-with-point
                   upper x (+ (funcall v-at x) y-extra))
                  (loop for zz from (mstrick-dom-min upper)
                          to (mstrick-dom-max upper)
                        unless (>= (mstrick-value upper zz) (funcall v-at zz))
                          do (error "upper bound violated at ~A" zz)))))))))))
