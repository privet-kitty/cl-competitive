;;;
;;; Treap with explicit key
;;; Virtually it works like std::map, std::multiset, or java.util.TreeMap.
;;;


;; Tips to use this structure as multiset: Just define OP as (defun op (x y) (+
;; x y)) and insert each element by (treap-ensure-key <treap> <key> 1 :if-exists
;; #'1+) instead of TREAP-INSERT.

(declaim (inline op))
(defun op (x y)
  "Is the operator comprising a monoid"
  (min x y))

(defconstant +op-identity+ 0
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

;; Treap with explicit key
(defstruct (treap (:constructor %make-treap (key priority value &key left right (accumulator value) lazy (count 1)))
                  (:copier nil)
                  (:conc-name %treap-))
  (key 0 :type fixnum)
  (value +op-identity+ :type fixnum)
  (accumulator +op-identity+ :type fixnum)
  (lazy +updater-identity+ :type fixnum)
  (priority 0 :type (integer 0 #.most-positive-fixnum))
  (count 0 :type (integer 0 #.most-positive-fixnum))
  (left nil :type (or null treap))
  (right nil :type (or null treap)))

(declaim (inline treap-count))
(defun treap-count (treap)
  "Returns the size of the (nullable) TREAP."
  (declare ((or null treap) treap))
  (if (null treap)
      0
      (%treap-count treap)))

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

(declaim (inline force-self))
(defun force-self (treap)
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

(defun treap-find (treap key &key (order #'<))
  "Finds the key that satisfies (and (not (funcall order key (%treap-key
sub-treap))) (not (funcall order (%treap-key sub-treap) key))) and returns KEY
and the assigned value. Returns NIL if KEY is not contained."
  (declare (function order)
           ((or null treap) treap))
  (cond ((null treap) (values nil nil))
        ((funcall order key (%treap-key treap))
         (treap-find (%treap-left treap) key :order order))
        ((funcall order (%treap-key treap) key)
         (treap-find (%treap-right treap) key :order order))
        (t (values key (%treap-value treap)))))

(defun treap-bisect-right-1 (treap key &key (order #'<))
  "Returns the largest key equal to or smaller than KEY and the assigned
value. Returns NIL if KEY is smaller than any keys in TREAP."
  (declare ((or null treap) treap)
           (function order))
  (labels ((recur (treap)
             (unless treap (return-from recur nil))
             (force-down treap)
             (if (funcall order key (%treap-key treap))
                 (recur (%treap-left treap))
                 (or (recur (%treap-right treap))
                     treap))))
    (let ((result (recur treap)))
      (if result
          (values (%treap-key result) (%treap-value result))
          (values nil nil)))))

(defun treap-bisect-left (treap key &key (order #'<))
  "Returns the smallest key equal to or larger than KEY and the assigned
value. Returns NIL if KEY is larger than any keys in TREAP."
  (declare ((or null treap) treap)
           (function order))
  (labels ((recur (treap)
             (unless treap (return-from recur nil))
             (force-down treap)
             (if (funcall order (%treap-key treap) key)
                 (recur (%treap-right treap))
                 (or (recur (%treap-left treap))
                     treap))))
    (let ((result (recur treap)))
      (if result
          (values (%treap-key result) (%treap-value result))
          (values nil nil)))))

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
              (force-self treap)
              (values treap right))
            (multiple-value-bind (left right)
                (treap-split (%treap-left treap) key :order order)
              (setf (%treap-left treap) right)
              (force-self treap)
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
                   (force-self node)
                   node)
                 (progn
                   (if (funcall order (%treap-key node) (%treap-key treap))
                       (setf (%treap-left treap)
                             (recur node (%treap-left treap)))
                       (setf (%treap-right treap)
                             (recur node (%treap-right treap))))
                   (force-self treap)
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
                      (force-self treap)
                      t))
                   ((funcall order (%treap-key treap) key)
                    (when (find-and-update (%treap-right treap))
                      (force-self treap)
                      t))
                   (t (setf (%treap-value treap)
                            (if if-exists
                                (funcall if-exists (%treap-value treap))
                                value))
                      (force-self treap)
                      t))))
    (if (find-and-update treap)
        treap
        (treap-insert treap key value :order order))))

(defun treap-merge (left right)
  "Destructively merges two treaps. Assumes that all keys of LEFT are smaller
 (or larger, depending on the order) than those of RIGHT."
  (declare (optimize (speed 3))
           ((or null treap) left right))
  (cond ((null left) (when right (force-down right) (force-self right)) right)
        ((null right) (when left (force-down left) (force-self left)) left)
        (t (force-down left)
           (force-down right)
         (if (> (%treap-priority left) (%treap-priority right))
             (progn
               (setf (%treap-right left)
                     (treap-merge (%treap-right left) right))
               (force-self left)
               left)
             (progn
               (setf (%treap-left right)
                     (treap-merge left (%treap-left right)))
               (force-self right)
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
           (force-self treap)
           treap)
          ((funcall order (%treap-key treap) key)
           (setf (%treap-right treap)
                 (treap-delete (%treap-right treap) key :order order))
           (force-self treap)
           treap)
          (t
           (treap-merge (%treap-left treap) (%treap-right treap))))))

(defun treap-map (function treap)
  "Successively applies FUNCTION to TREAP[0], ..., TREAP[SIZE-1]. FUNCTION must
take two arguments: KEY and VALUE."
  (declare (function function))
  (when treap
    (force-down treap)
    (treap-map function (%treap-left treap))
    (funcall function (%treap-key treap) (%treap-value treap))
    (treap-map function (%treap-right treap))
    (force-self treap)))

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

;; Reference: https://cp-algorithms.com/data_structures/treap.html
;; TODO: take a sorted list as the argument
(declaim (inline make-treap))
(defun make-treap (sorted-vector)
  "Makes a treap using each key of the given SORTED-VECTOR in O(n). Note that
this function doesn't check if the SORTED-VECTOR is actually sorted w.r.t. your
intended order. The values are filled with the identity element."
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
               (force-self treap))))
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
