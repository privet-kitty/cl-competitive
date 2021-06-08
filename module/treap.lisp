(defpackage :cp/treap
  (:use :cl)
  (:export #:treap #:treap-count #:treap-find #:treap-bisect-left
           #:treap-split #:treap-insert #:treap-push #:treap-pop #:treap-delete #:treap-merge
           #:treap-map #:invalid-treap-index-error #:treap-first #:treap-last))
(in-package :cp/treap)

;; No unit tests. You should use ref-able-treap instead.

(defstruct (treap (:constructor %make-treap (key priority &optional left right))
                  (:copier nil)
                  (:predicate nil)
                  (:conc-name %treap-))
  key
  (priority 0 :type (integer 0 #.most-positive-fixnum))
  (left nil :type (or null treap))
  (right nil :type (or null treap)))

(declaim (inline treap-key))
(defun treap-key (treap)
  (and treap (%treap-key treap)))

(declaim (inline treap-find))
(defun treap-find (key treap &key (order #'<))
  "Returns KEY if TREAP contains it; otherwise it returns NIL.

An element in TREAP is considered to be equal to KEY iff (and (not (funcall
order key <element>)) (not (funcall order <element> key))) is true."
  (declare ((or null treap) treap))
  (labels ((recur (treap)
             (cond ((null treap) nil)
                   ((funcall order key (%treap-key treap))
                    (recur (%treap-left treap)))
                   ((funcall order (%treap-key treap) key)
                    (recur (%treap-right treap)))
                   (t key))))
    (recur treap)))

(declaim (inline treap-split))
(defun treap-split (key treap &key (order #'<))
  "Destructively splits TREAP with reference to KEY and returns two treaps,
the smaller sub-treap (< KEY) and the larger one (>= KEY)."
  (declare ((or null treap) treap))
  (labels ((recur (treap)
             (cond ((null treap)
                    (values nil nil))
                   ((funcall order (%treap-key treap) key)
                    (multiple-value-bind (left right)
                        (recur (%treap-right treap))
                      (setf (%treap-right treap) left)
                      (values treap right)))
                   (t
                    (multiple-value-bind (left right)
                        (recur (%treap-left treap))
                      (setf (%treap-left treap) right)
                      (values left treap))))))
    (recur treap)))

(declaim (inline treap-insert))
(defun treap-insert (key treap &key (order #'<))
  "Destructively inserts KEY into TREAP and returns the resultant treap. You
cannot rely on the side effect. Use the returned value.

The behavior is undefined when duplicate keys are inserted."
  (declare ((or null treap) treap))
  (labels ((recur (new-node treap)
             (declare (treap new-node))
             (cond ((null treap) new-node)
                   ((> (%treap-priority new-node) (%treap-priority treap))
                    (setf (values (%treap-left new-node) (%treap-right new-node))
                          (treap-split (%treap-key new-node) treap :order order))
                    new-node)
                   (t
                    (if (funcall order (%treap-key new-node) (%treap-key treap))
                        (setf (%treap-left treap)
                              (recur new-node (%treap-left treap)))
                        (setf (%treap-right treap)
                              (recur new-node (%treap-right treap))))
                    treap))))
    (recur (%make-treap key (random most-positive-fixnum)) treap)))

(declaim (inline treap-map))
(defun treap-map (function treap)
  "Successively applies FUNCTION to each key of TREAP in the given
order. FUNCTION must take one argument."
  (declare (function function))
  (labels ((recur (treap)
             (when treap
               (recur (%treap-left treap))
               (funcall function (%treap-key treap))
               (recur (%treap-right treap)))))
    (recur treap)))

(defmethod print-object ((object treap) stream)
  (print-unreadable-object (object stream :type t)
    (let ((init t))
      (treap-map (lambda (key)
                   (if init
                       (setf init nil)
                       (write-char #\  stream))
                   (write key :stream stream))
                 object))))

(declaim (ftype (function * (values (or null treap) &optional)) treap-merge))
(defun treap-merge (left right)
  "Destructively concatenates two treaps. Assumes that all keys of LEFT are
smaller (or larger, depending on the order) than those of RIGHT."
  (declare (optimize (speed 3))
           ((or null treap) left right))
  (cond ((null left) right)
        ((null right) left)
        ((> (%treap-priority left) (%treap-priority right))
         (setf (%treap-right left)
               (treap-merge (%treap-right left) right))
         left)
        (t
         (setf (%treap-left right)
               (treap-merge left (%treap-left right)))
         right)))

(declaim (inline treap-delete))
(defun treap-delete (key treap &key (order #'<))
  "Destructively deletes the KEY in TREAP and returns the resultant treap. You
cannot rely on the side effect. Use the returned value."
  (declare ((or null treap) treap))
  (labels ((recur (treap)
             (cond ((null treap) nil)
                   ((funcall order key (%treap-key treap))
                    (setf (%treap-left treap) (recur (%treap-left treap)))
                    treap)
                   ((funcall order (%treap-key treap) key)
                    (setf (%treap-right treap) (recur (%treap-right treap)))
                    treap)
                   (t
                    (treap-merge (%treap-left treap) (%treap-right treap))))))
    (recur treap)))

(defmacro treap-push (key treap &optional (order '#'<))
  "Pushes a KEY to TREAP."
  `(setf ,treap (treap-insert ,key ,treap :order ,order)))

(defmacro treap-pop (key treap &optional (order '#'<))
  "Deletes a KEY from TREAP."
  `(setf ,treap (treap-delete ,key ,treap :order ,order)))

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

(declaim (ftype (function * (values (integer 0 #.most-positive-fixnum) &optional))
                treap-count))
(defun treap-count (treap)
  "Counts the number of elements in TREAP in O(n) time."
  (declare (optimize (speed 3))
           ((or null treap) treap))
  (labels ((recur (treap)
             (declare (optimize (safety 0)))
             (if (null treap)
                 0
                 (+ 1
                    (treap-count (%treap-left treap))
                    (treap-count (%treap-right treap))))))
    (recur treap)))

(declaim (inline treap-bisect-left))
(defun treap-bisect-left (treap key &key (order #'<))
  "Returns the smallest key equal to or larger than KEY. Returns NIL if KEY is
larger than any keys in TREAP."
  (declare ((or null treap) treap))
  (labels ((recur (treap)
             (cond ((null treap) nil)
                   ((funcall order (%treap-key treap) key)
                    (recur (%treap-right treap)))
                   (t (or (recur (%treap-left treap))
                          treap)))))
    (treap-key (recur treap))))
