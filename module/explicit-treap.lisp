;;;
;;; Treap with explicit key
;;; Virtually it works like std::map, std::multiset, or java.util.TreeMap.
;;;

;; TODO: abstraction

(defpackage :cp/explicit-treap
  (:use :cl)
  (:export #:treap #:treap-p #:treap-key #:treap-accumulator
           #:treap-split #:treap-insert #:treap-merge #:treap-delete
           #:treap-ensure-key #:treap-unite #:treap-map #:do-treap
           #:make-treap #:treap-fold #:treap-update #:treap-ref
           #:treap-first #:treap-last #:treap-find
           #:treap-bisect-left #:treap-bisect-right #:treap-bisect-left-1 #:treap-bisect-right-1
           #:treap-fold-bisect #:treap-fold-bisect-from-end))
(in-package :cp/explicit-treap)

;; Tips to use this structure as a multiset: Just define OP as (defun op (x y)
;; (+ x y)) and insert each element by
;;
;; (treap-ensure-key <treap> <key> 1 :if-exists #'1+)
;;
;; instead of TREAP-INSERT.

;; TODO & NOTE: insufficient tests
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

;; FIXME: Should the left and right end of the target interval be included?
(declaim (inline modifier-op))
(defun modifier-op (a b)
  "Is the operator to update ACCUMULATOR based on LAZY value."
  (+ a b))

(defstruct (treap (:constructor %make-treap (key priority value &key left right (accumulator value) lazy))
                  (:copier nil)
                  (:conc-name %treap-))
  (key 0 :type fixnum)
  (value +op-identity+ :type fixnum)
  (accumulator +op-identity+ :type fixnum)
  (lazy +updater-identity+ :type fixnum)
  (priority 0 :type (integer 0 #.most-positive-fixnum))
  (left nil :type (or null treap))
  (right nil :type (or null treap)))

(declaim (inline treap-key))
(defun treap-key (treap)
  "Returns the key of (nullable) TREAP."
  (and treap (%treap-key treap)))

(declaim (inline treap-accumulator))
(defun treap-accumulator (treap)
  (declare ((or null treap) treap))
  (if (null treap)
      +op-identity+
      (%treap-accumulator treap)))

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
                         (%treap-lazy treap))))
    (when (%treap-right treap)
      (setf (%treap-lazy (%treap-right treap))
            (updater-op (%treap-lazy (%treap-right treap))
                        (%treap-lazy treap)))
      (setf (%treap-accumulator (%treap-right treap))
            (modifier-op (%treap-accumulator (%treap-right treap))
                         (%treap-lazy treap))))
    (setf (%treap-value treap)
          (modifier-op (%treap-value treap)
                       (%treap-lazy treap)))
    (setf (%treap-lazy treap) +updater-identity+)))

(declaim (ftype (function * (values (or null treap) (or null treap) &optional)) treap-split))
(defun treap-split (treap key &key (order #'<))
  "Destructively splits TREAP with reference to KEY and returns two treaps,
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
  "Destructively deletes KEY in TREAP and returns the resultant treap. Returns
unmodified TREAP If KEY doesn't exist. You cannot rely on the side effect. Use
the returned value.

 (Note that this function deletes at most one node even if duplicate keys
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

(declaim (inline treap-unite))
(defun treap-unite (treap1 treap2 &key (order #'<))
  "Merges two treaps with keeping the order."
  (labels
      ((recur (l r)
         (cond ((null l) (when r (force-down r) (force-up r)) r)
               ((null r) (when l (force-down l) (force-up l)) l)
               (t (force-down l)
                  (when (< (%treap-priority l) (%treap-priority r))
                    (rotatef l r))
                  (multiple-value-bind (lchild rchild)
                      (treap-split r (%treap-key l) :order order)
                    (setf (%treap-left l) (recur (%treap-left l) lchild)
                          (%treap-right l) (recur (%treap-right l) rchild))
                    (force-up l)
                    l)))))
    (recur treap1 treap2)))

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
  "Successively binds the keys and the values of TREAP to KEY-VAR and VALUE-VAR
in ascending order, and executes BODY."
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
(defun make-treap (size key-function &optional value-function)
  "Makes a treap in O(n) time using each key returned by (KEY-FUNCTION
<index>). Note that this function doesn't check if the keys are really sorted
w.r.t. your intended order. The values are filled with VALUE-FUNCTION in the
same way. If it is not given, the identity element is used."
  (declare ((integer 0 #.most-positive-fixnum) size)
           (function key-function)
           ((or null function) value-function))
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
                        (node (%make-treap (funcall key-function mid)
                                           (random most-positive-fixnum)
                                           (if value-function
                                               (funcall value-function mid)
                                               +op-identity+))))
                   (setf (%treap-left node) (build l mid))
                   (setf (%treap-right node) (build (+ mid 1) r))
                   (heapify node)
                   (force-up node)
                   node))))
    (build 0 size)))

(defun treap-fold (treap &key left right (order #'<))
  "Queries the sum of the half-open interval specified by the keys: [LEFT,
RIGHT). If LEFT [RIGHT] is not given, it is assumed to be -inf [+inf]."
  (declare (function order))
  (labels ((recur (treap l r)
             (unless treap
               (return-from recur +op-identity+))
             (force-down treap)
             (prog1
                 (if (and (null l) (null r))
                     (%treap-accumulator treap)
                     (let ((key (%treap-key treap)))
                       (if (or (null l) (not (funcall order key l))) ; L <= KEY
                           (if (or (null r) (funcall order key r)) ; KEY < R
                               (op (op (recur (%treap-left treap) l nil)
                                       (%treap-value treap))
                                   (recur (%treap-right treap) nil r))
                               (recur (%treap-left treap) l r))
                           (recur (%treap-right treap) l r))))
               (force-up treap))))
    (recur treap left right)))

(declaim (inline treap-update))
(defun treap-update (treap x &key left right (order #'<))
  "Updates TREAP by TREAP[KEY] := (OP TREAP[KEY] X) for all KEY in [l, r). L
and/or R can be NIL, then it is regarded as the (negative or positive)
infinity."
  (assert (not (and left right (funcall order right left))))
  (labels ((recur (treap l r)
             (when treap
               (if (and (null l) (null r))
                   (progn
                     (setf (%treap-lazy treap)
                           (updater-op (%treap-lazy treap) x))
                     (force-down treap))
                   (let ((key (%treap-key treap)))
                     (force-down treap)
                     (if (or (null l) (not (funcall order key l))) ; L <= KEY
                         (if (or (null r) (funcall order key r)) ; KEY < R
                             (progn
                               (recur (%treap-left treap) l nil)
                               (setf (%treap-value treap)
                                     (modifier-op (%treap-value treap) x))
                               (recur (%treap-right treap) nil r))
                             (recur (%treap-left treap) l r))
                         (recur (%treap-right treap) l r))))
               (force-up treap))))
    (recur treap left right)
    treap))

(declaim (inline treap-ref))
(defun treap-ref (treap key &key (order #'<))
  (declare ((or null treap) treap))
  (labels ((recur (treap)
             (when treap
               (force-down treap)
               (prog1 (cond ((funcall order key (%treap-key treap))
                             (recur (%treap-left treap)))
                            ((funcall order (%treap-key treap) key)
                             (recur (%treap-right treap)))
                            (t (%treap-value treap)))
                 (force-up treap)))))
    (recur treap)))

(declaim (inline (setf treap-ref)))
(defun (setf treap-ref) (new-value treap key &key (order #'<))
  (declare ((or null treap) treap))
  (labels ((recur (treap)
             (when treap
               (force-down treap)
               (prog1 (cond ((funcall order key (%treap-key treap))
                             (recur (%treap-left treap)))
                            ((funcall order (%treap-key treap) key)
                             (recur (%treap-right treap)))
                            (t (setf (%treap-value treap) new-value)))
                 (force-up treap)))))
    (recur treap)))

(defun treap-first (treap)
  (declare (optimize (speed 3))
           (treap treap))
  (if (%treap-left treap)
      (treap-first (%treap-left treap))
      (%treap-key treap)))

(defun treap-last (treap)
  (declare (optimize (speed 3))
           (treap treap))
  (if (%treap-right treap)
      (treap-last (%treap-right treap))
      (%treap-key treap)))

;;;
;;; Binary search by key
;;;

;; NOTE: These functions intentionally don't return the assigned value. That is
;; for efficiency, because thereby they don't need to execute lazy propagation.

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
             (cond ((null treap) nil)
                   ((funcall order (%treap-key treap) key)
                    (recur (%treap-right treap)))
                   (t (or (recur (%treap-left treap))
                          treap)))))
    (treap-key (recur treap))))

(declaim (inline treap-bisect-left))
(defun treap-bisect-right (treap key &key (order #'<))
  "Returns the smallest key larger than KEY. Returns NIL if KEY is equal to or
larger than any keys in TREAP."
  (declare ((or null treap) treap)
           (function order))
  (labels ((recur (treap)
             (cond ((null treap) nil)
                   ((funcall order key (%treap-key treap))
                    (or (recur (%treap-left treap))
                        treap))
                   (t (recur (%treap-right treap))))))
    (treap-key (recur treap))))

(declaim (inline treap-bisect-left-1))
(defun treap-bisect-left-1 (treap key &key (order #'<))
  "Returns the largest key smaller than KEY. Returns NIL if KEY is equal to or
smaller than any keys in TREAP."
  (declare ((or null treap) treap)
           (function order))
  (labels ((recur (treap)
             (cond ((null treap) nil)
                   ((funcall order (%treap-key treap) key)
                    (or (recur (%treap-right treap))
                        treap))
                   (t (recur (%treap-left treap))))))
    (treap-key (recur treap))))

(declaim (inline treap-bisect-right-1))
(defun treap-bisect-right-1 (treap key &key (order #'<))
  "Returns the largest key equal to or smaller than KEY. Returns NIL if KEY is
smaller than any keys in TREAP."
  (declare ((or null treap) treap)
           (function order))
  (labels ((recur (treap)
             (cond ((null treap) nil)
                   ((funcall order key (%treap-key treap))
                    (recur (%treap-left treap)))
                   (t (or (recur (%treap-right treap))
                          treap)))))
    (treap-key (recur treap))))

;; not tested
(defun treap-fold-bisect (treap value &key (order #'<))
  "Returns the smallest existing key that satisfies TREAP[<1st key>]+ TREAP[<2nd key>] + ... + TREAP[key] >= VALUE (if ORDER is #'<).

Note:
- This function deals with a **closed** interval. 
- This function returns NIL instead if TREAP[<1st key>]+ ... + TREAP[<last
key>] < VALUE.
- The prefix sums of TREAP (TREAP[<1st key>], TREAP[<1st key>] + TREAP[<2nd
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

(defun treap-fold-bisect-from-end (treap value &key (order #'<))
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
