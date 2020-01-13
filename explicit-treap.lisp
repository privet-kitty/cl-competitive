;;;
;;; Treap with explicit key
;;; Virtually it works like std::map, std::multiset, or java.util.TreeMap.
;;;

;; Tips to use this structure as a multiset: Just define OP as (defun op (x y)
;; (+ x y)) and insert each element by (treap-ensure-key <treap> <key> 1
;; :if-exists #'1+) instead of TREAP-INSERT.

;; TODO: abolish COUNT slot
;; TODO: introduce abstraction by macro

(declaim (inline op))
(defun op (x y)
  "Is the operator comprising a monoid"
  (min x y))

(defconstant +op-identity+ most-positive-fixnum
  "identity element w.r.t. OP")

(declaim (inline updater-op))
(defun updater-op (a b)
  "Is the operator to compute and update LAZY value."
  (+ a b))

(defconstant +updater-identity+ 0
  "identity element w.r.t. UPDATER-OP")

(declaim (inline modifier-op))
(defun modifier-op (a b size)
  "Is the operator to update ACCUMULATOR based on LAZY value."
  (declare (ignore size))
  (+ a b))

(defstruct (treap (:constructor %make-treap (key priority value &key left right (accumulator value) lazy (count 1)))
                  (:copier nil)
                  (:conc-name %treap-))
  (key 0 :type fixnum)
  (value +op-identity+ :type fixnum)
  (accumulator +op-identity+ :type fixnum)
  (lazy +updater-identity+ :type fixnum)
  (priority 0 :type (integer 0 #.most-positive-fixnum))
  (count 0 :type (integer 0 #.most-positive-fixnum)) ; deprecated
  (left nil :type (or null treap))
  (right nil :type (or null treap)))

(declaim (inline treap-count))
(defun treap-count (treap)
  "Returns the size of the (nullable) TREAP."
  (declare ((or null treap) treap))
  (if (null treap)
      0
      (%treap-count treap)))

(declaim (inline treap-key))
(defun treap-key (treap)
  "Returns the key of the (nullable) TREAP."
  (and treap (%treap-key treap)))

(declaim (inline treap-accumulator))
(defun treap-accumulator (treap)
  (declare ((or null treap) treap))
  (if (null treap)
      +op-identity+
      (%treap-accumulator treap)))

(declaim (inline update-count))
(defun update-count (treap)
  (declare (treap treap))
  (setf (%treap-count treap)
        (+ 1
           (treap-count (%treap-left treap))
           (treap-count (%treap-right treap)))))

(declaim (inline update-accumulator))
(defun update-accumulator (treap)
  (declare (treap treap))
  (setf (%treap-accumulator treap)
        (if (%treap-left treap)
            (if (%treap-right treap)
                (let ((mid-res (op (%treap-accumulator (%treap-left treap))
                                   (%treap-value treap))))
                  (declare (dynamic-extent mid-res))
                  (op mid-res (%treap-accumulator (%treap-right treap))))
                (op (%treap-accumulator (%treap-left treap))
                    (%treap-value treap)))
            (if (%treap-right treap)
                (op (%treap-value treap)
                    (%treap-accumulator (%treap-right treap)))
                (%treap-value treap)))))

(declaim (inline force-up))
(defun force-up (treap)
  "Propagates up the information from children."
  (declare (treap treap))
  (update-count treap)
  (update-accumulator treap))

(declaim (inline force-down))
(defun force-down (treap)
  "Propagates down the information to children."
  (declare (treap treap))
  (unless (eql +updater-identity+ (%treap-lazy treap))
    (when (%treap-left treap)
      (setf (%treap-lazy (%treap-left treap))
            (updater-op (%treap-lazy (%treap-left treap))
                        (%treap-lazy treap)))
      (setf (%treap-accumulator (%treap-left treap))
            (modifier-op (%treap-accumulator (%treap-left treap))
                         (%treap-lazy treap)
                         (%treap-count (%treap-left treap)))))
    (when (%treap-right treap)
      (setf (%treap-lazy (%treap-right treap))
            (updater-op (%treap-lazy (%treap-right treap))
                        (%treap-lazy treap)))
      (setf (%treap-accumulator (%treap-right treap))
            (modifier-op (%treap-accumulator (%treap-right treap))
                         (%treap-lazy treap)
                         (%treap-count (%treap-right treap)))))
    (setf (%treap-value treap)
          (modifier-op (%treap-value treap)
                       (%treap-lazy treap)
                       1))
    (setf (%treap-lazy treap) +updater-identity+)))

(declaim (ftype (function * (values (or null treap) (or null treap) &optional)) treap-split))
(defun treap-split (treap key &key (order #'<))
  "Destructively splits the TREAP with reference to KEY and returns two treaps,
the smaller sub-treap (< KEY) and the larger one (>= KEY)."
  (declare (function order)
           ((or null treap) treap))
  (if (null treap)
      (values nil nil)
      (progn
        (force-down treap)
        (if (funcall order (%treap-key treap) key)
            (multiple-value-bind (left right)
                (treap-split (%treap-right treap) key :order order)
              (setf (%treap-right treap) left)
              (force-up treap)
              (values treap right))
            (multiple-value-bind (left right)
                (treap-split (%treap-left treap) key :order order)
              (setf (%treap-left treap) right)
              (force-up treap)
              (values left treap))))))

(declaim (inline treap-insert))
(defun treap-insert (treap key value &key (order #'<))
  "Destructively inserts KEY into TREAP and returns the resultant treap. You
cannot rely on the side effect. Use the returned value.

The behavior is undefined when duplicate keys are inserted."
  (declare ((or null treap) treap)
           (function order))
  (labels ((recur (node treap)
             (declare (treap node))
             (unless treap (return-from recur node))
             (force-down treap)
             (if (> (%treap-priority node) (%treap-priority treap))
                 (progn
                   (setf (values (%treap-left node) (%treap-right node))
                         (treap-split treap (%treap-key node) :order order))
                   (force-up node)
                   node)
                 (progn
                   (if (funcall order (%treap-key node) (%treap-key treap))
                       (setf (%treap-left treap)
                             (recur node (%treap-left treap)))
                       (setf (%treap-right treap)
                             (recur node (%treap-right treap))))
                   (force-up treap)
                   treap))))
    (recur (%make-treap key (random most-positive-fixnum) value) treap)))

(declaim (inline treap-ensure-key))
(defun treap-ensure-key (treap key value &key (order #'<) if-exists)
  "IF-EXISTS := nil | function

Ensures that TREAP contains KEY and assigns VALUE to it if IF-EXISTS is
false. If IF-EXISTS is function and TREAP already contains KEY, TREAP-ENSURE-KEY
updates the value by the function instead of overwriting it with VALUE."
  (declare (function order)
           ((or null treap) treap))
  (labels ((find-and-update (treap)
             ;; Updates the value slot and returns T if KEY exists
             (unless treap (return-from find-and-update nil))
             (force-down treap)
             (cond ((funcall order key (%treap-key treap))
                    (when (find-and-update (%treap-left treap))
                      (force-up treap)
                      t))
                   ((funcall order (%treap-key treap) key)
                    (when (find-and-update (%treap-right treap))
                      (force-up treap)
                      t))
                   (t (setf (%treap-value treap)
                            (if if-exists
                                (funcall if-exists (%treap-value treap))
                                value))
                      (force-up treap)
                      t))))
    (if (find-and-update treap)
        treap
        (treap-insert treap key value :order order))))

(defun treap-merge (left right)
  "Destructively concatenates two treaps. Assumes that all keys of LEFT are
smaller (or larger, depending on the order) than those of RIGHT.

Note that this `merge' is different from CL:MERGE and rather close to
CL:CONCATENATE."
  (declare (optimize (speed 3))
           ((or null treap) left right))
  (cond ((null left) (when right (force-down right) (force-up right)) right)
        ((null right) (when left (force-down left) (force-up left)) left)
        (t (force-down left)
           (force-down right)
         (if (> (%treap-priority left) (%treap-priority right))
             (progn
               (setf (%treap-right left)
                     (treap-merge (%treap-right left) right))
               (force-up left)
               left)
             (progn
               (setf (%treap-left right)
                     (treap-merge left (%treap-left right)))
               (force-up right)
               right)))))

(defun treap-delete (treap key &key (order #'<))
  "Destructively deletes the KEY in TREAP and returns the resultant
treap. Returns the unmodified TREAP If KEY doesn't exist. You cannot rely on the
side effect. Use the returned value.

 (Note that this function deletes at most one node even if duplicated keys
exist.)"
  (declare ((or null treap) treap)
           (function order))
  (when treap
    (force-down treap)
    (cond ((funcall order key (%treap-key treap))
           (setf (%treap-left treap)
                 (treap-delete (%treap-left treap) key :order order))
           (force-up treap)
           treap)
          ((funcall order (%treap-key treap) key)
           (setf (%treap-right treap)
                 (treap-delete (%treap-right treap) key :order order))
           (force-up treap)
           treap)
          (t
           (treap-merge (%treap-left treap) (%treap-right treap))))))

(declaim (inline treap-map))
(defun treap-map (function treap)
  "Successively applies FUNCTION to TREAP[0], ..., TREAP[SIZE-1]. FUNCTION must
take two arguments: KEY and VALUE."
  (labels ((recur (treap)
             (when treap
               (force-down treap)
               (recur (%treap-left treap))
               (funcall function (%treap-key treap) (%treap-value treap))
               (recur (%treap-right treap))
               (force-up treap))))
    (recur treap)))

(defmethod print-object ((object treap) stream)
  (print-unreadable-object (object stream :type t)
    (let ((init t))
      (treap-map (lambda (key value)
                   (if init
                       (setf init nil)
                       (write-char #\  stream))
                   (format stream "<~A . ~A>" key value))
                 object))))

(defmacro do-treap ((key-var value-var treap &optional result) &body body)
  "Successively binds the key and value of INODE[0], ..., INODE[SIZE-1] to
KEY-VAR and VALUE-VAR and executes BODY."
  `(block nil
     (treap-map (lambda (,key-var ,value-var) ,@body) ,treap)
     ,result))

;; This function takes O(nlog(n)) time. It is just for debugging.
(defun treap (order &rest key-and-values)
  "Takes cons cells in the form of (<key> . <value>)."
  (loop with res = nil
        for (key . value) in key-and-values
        do (setf res (treap-insert res key value :order order))
        finally (return res)))

;; Reference: https://cp-algorithms.com/data_structures/treap.html
;; TODO: take a sorted list as the argument
(declaim (inline make-treap))
(defun make-treap (sorted-vector)
  "Makes a treap using each key of the given SORTED-VECTOR in O(n) time. Note
that this function doesn't check if the SORTED-VECTOR is actually sorted
w.r.t. your intended order. The values are filled with the identity element."
  (declare (vector sorted-vector))
  (labels ((heapify (top)
             (when top
               (let ((prioritized-node top))
                 (when (and (%treap-left top)
                            (> (%treap-priority (%treap-left top))
                               (%treap-priority prioritized-node)))
                   (setq prioritized-node (%treap-left top)))
                 (when (and (%treap-right top)
                            (> (%treap-priority (%treap-right top))
                               (%treap-priority prioritized-node)))
                   (setq prioritized-node (%treap-right top)))
                 (unless (eql prioritized-node top)
                   (rotatef (%treap-priority prioritized-node)
                            (%treap-priority top))
                   (heapify prioritized-node)))))
           (build (l r)
             (declare ((integer 0 #.most-positive-fixnum) l r))
             (if (= l r)
                 nil
                 (let* ((mid (ash (+ l r) -1))
                        (node (%make-treap (aref sorted-vector mid)
                                           (random most-positive-fixnum)
                                           +op-identity+)))
                   (setf (%treap-left node) (build l mid))
                   (setf (%treap-right node) (build (+ mid 1) r))
                   (heapify node)
                   (update-count node)
                   node))))
    (build 0 (length sorted-vector))))

(define-condition invalid-treap-index-error (type-error)
  ((treap :initarg :treap :reader invalid-treap-index-error-treap)
   (index :initarg :index :reader invalid-treap-index-error-index))
  (:report
   (lambda (condition stream)
     (format stream "Invalid index ~W for treap ~S."
             (invalid-treap-index-error-index condition)
             (invalid-treap-index-error-treap condition)))))

;; DEPRECATED
(defun treap-ref (treap index)
  "Returns the key and value corresponding to the INDEX."
  (declare (optimize (speed 3))
           ((or null treap) treap)
           ((integer 0 #.most-positive-fixnum) index))
  (when (>= index (treap-count treap))
    (error 'invalid-treap-index-error :treap treap :index index))
  (labels ((%ref (treap index)
             (declare (optimize (safety 0))
                      (treap treap)
                      ((integer 0 #.most-positive-fixnum) index))
             (force-down treap)
             (prog1
                 (let ((left-count (treap-count (%treap-left treap))))
                   (cond ((< index left-count)
                          (%ref (%treap-left treap) index))
                         ((> index left-count)
                          (%ref (%treap-right treap) (- index left-count 1)))
                         (t (values (%treap-key treap) (%treap-value treap)))))
               (force-up treap))))
    (%ref treap index)))

;; FIXME: might be problematic when two priorities collide.
(declaim (inline treap-query))
(defun treap-query (treap &key left right (order #'<))
  "Queries the sum of the half-open interval specified by the keys: [LEFT,
RIGHT). If LEFT [RIGHT] is not given, it is assumed to be -inf [+inf]."
  (if (null left)
      (if (null right)
          (treap-accumulator treap)
          (multiple-value-bind (treap-0-r treap-r-n)
              (treap-split treap right :order order)
            (prog1 (treap-accumulator treap-0-r)
              (treap-merge treap-0-r treap-r-n))))
      (if (null right)
          (multiple-value-bind (treap-0-l treap-l-n)
              (treap-split treap left :order order)
            (prog1 (treap-accumulator treap-l-n)
              (treap-merge treap-0-l treap-l-n)))
          (progn
            (assert (not (funcall order right left)))
            (multiple-value-bind (treap-0-l treap-l-n)
                (treap-split treap left :order order)
              (multiple-value-bind (treap-l-r treap-r-n)
                  (treap-split treap-l-n right :order order)
                (prog1 (treap-accumulator treap-l-r)
                  (treap-merge treap-0-l (treap-merge treap-l-r treap-r-n)))))))))

(declaim (inline treap-update))
(defun treap-update (treap x left right &key (order #'<))
  "Updates TREAP[KEY] := (OP TREAP[KEY] X) for all KEY in [l, r)"
  (assert (not (funcall order left right)))
  (multiple-value-bind (treap-0-l treap-l-n)
      (treap-split treap left :order order)
    (multiple-value-bind (treap-l-r treap-r-n)
        (treap-split treap-l-n right :order order)
      (when treap-l-r
        (setf (%treap-lazy treap-l-r)
              (updater-op (%treap-lazy treap-l-r) x)))
      (treap-merge treap-0-l (treap-merge treap-l-r treap-r-n)))))

;;;
;;; Bisection search for key
;;;

;; NOTE: These functions intentionally doesn't return the assigned value. That
;; is for efficiency, because thereby they doesn't need to execute lazy
;; propagation.

(defun treap-find (treap key &key (order #'<))
  "Finds the key that satisfies (AND (NOT (FUNCALL ORDER KEY (%TREAP-KEY
<sub-treap>))) (NOT (FUNCALL ORDER (%TREAP-KEY <sub-treap>) KEY))) and returns
KEY if it exists, otherwise returns NIL."
  (declare (optimize (speed 3))
           (function order)
           ((or null treap) treap))
  (cond ((null treap) nil)
        ((funcall order key (%treap-key treap))
         (treap-find (%treap-left treap) key :order order))
        ((funcall order (%treap-key treap) key)
         (treap-find (%treap-right treap) key :order order))
        (t key)))

(declaim (inline treap-bisect-left))
(defun treap-bisect-left (treap key &key (order #'<))
  "Returns the smallest key equal to or larger than KEY. Returns NIL if KEY is
larger than any keys in TREAP."
  (declare ((or null treap) treap)
           (function order))
  (labels ((recur (treap)
             (unless treap (return-from recur nil))
             (if (funcall order (%treap-key treap) key)
                 (recur (%treap-right treap))
                 (or (recur (%treap-left treap))
                     treap))))
    (treap-key (recur treap))))

(declaim (inline treap-bisect-left))
(defun treap-bisect-right (treap key &key (order #'<))
  "Returns the smallest key larger than KEY. Returns NIL if KEY is equal to or
larger than any keys in TREAP."
  (declare ((or null treap) treap)
           (function order))
  (labels ((recur (treap)
             (unless treap (return-from recur nil))
             (if (funcall order key (%treap-key treap))
                 (or (recur (%treap-left treap))
                     treap)
                 (recur (%treap-right treap)))))
    (treap-key (recur treap))))

(declaim (inline treap-bisect-left-1))
(defun treap-bisect-left-1 (treap key &key (order #'<))
  "Returns the largest key smaller than KEY. Returns NIL if KEY is equal to or
smaller than any keys in TREAP."
  (declare ((or null treap) treap)
           (function order))
  (labels ((recur (treap)
             (unless treap (return-from recur nil))
             (if (funcall order (%treap-key treap) key)
                 (or (recur (%treap-right treap))
                     treap)
                 (recur (%treap-left treap)))))
    (treap-key (recur treap))))

(declaim (inline treap-bisect-right-1))
(defun treap-bisect-right-1 (treap key &key (order #'<))
  "Returns the largest key equal to or smaller than KEY. Returns NIL if KEY is
smaller than any keys in TREAP."
  (declare ((or null treap) treap)
           (function order))
  (labels ((recur (treap)
             (unless treap (return-from recur nil))
             (if (funcall order key (%treap-key treap))
                 (recur (%treap-left treap))
                 (or (recur (%treap-right treap))
                     treap))))
    (treap-key (recur treap))))

;; not tested
(defun treap-range-bisect (treap value &key (order #'<))
  "Returns the smallest existing key that satisfies TREAP[<1st key>]+ TREAP[<2nd key>] + ... + TREAP[key] >= VALUE (if ORDER is #'<).

Note:
- This function handles a **closed** interval. 
- This function returns NIL instead if TREAP[<1st key>]+ ... + TREAP[<last
key>] < VALUE.
- The prefix sums of TTREAP, (TREAP[<1st key>], TREAP[<1st key>]+TREAP[<2nd
key>], ...) must be monotone w.r.t. ORDER.
- ORDER must be a strict order"
  (labels
      ((recur (treap prev-sum)
         (unless treap
           (return-from recur))
         (force-down treap)
         (let ((sum prev-sum))
           (prog1
               (cond ((not (funcall order
                                    (setq sum (op sum (treap-accumulator (%treap-left treap))))
                                    value))
                      (if (%treap-left treap)
                          (recur (%treap-left treap) prev-sum)
                          (%treap-key treap)))
                     ((not (funcall order
                                    (setq sum (op sum (%treap-value treap)))
                                    value))
                      (%treap-key treap))
                     (t
                      (recur (%treap-right treap) sum)))
             (force-up treap)))))
    (recur treap +op-identity+)))

(defun treap-range-bisect-from-end (treap value &key (order #'<))
  (labels
      ((recur (treap prev-sum)
         (unless treap
           (return-from recur))
         (force-down treap)
         (let ((sum prev-sum))
           (prog1
               (cond ((not (funcall order
                                    (setq sum (op (treap-accumulator (%treap-right treap)) sum))
                                    value))
                      (if (%treap-right treap)
                          (recur (%treap-right treap) prev-sum)
                          (%treap-key treap)))
                     ((not (funcall order
                                    (setq sum (op (%treap-value treap) sum))
                                    value))
                      (%treap-key treap))
                     (t
                      (recur (%treap-left treap) sum)))
             (force-up treap)))))
    (recur treap +op-identity+)))
