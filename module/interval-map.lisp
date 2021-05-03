(defpackage :cp/interval-map
  (:use :cl)
  (:export #:interval-map #:imap-get #:imap-insert #:imap-concat #:imap-split #:imap-map
           #:make-imap))
(in-package :cp/interval-map)

;; NOTE: not tested

(deftype element-type () t)

(defstruct (interval-map (:constructor %make-interval-map
                             (lkey rkey value
                              &optional lnode rnode
                              &aux (priority (random #.(+ 1 most-positive-fixnum)))))
                         (:conc-name %imap-)
                         (:copier %copy-imap))
  "This structure maintains an ordered map of half-open intervals and a value
assigned to it. The underlying data structure is Treap. Every fundamental
operation takes expected O(log(n)) time.

NOTE:
- For every destructive operation, you cannot rely on a side effect but have to
use a returned value. (i.e. same as destructive operations to cons.)
- Unlike cp/interval-set, each interval doesn't merge with adjacent intervals
automatically. i.e. [3, 10) and [10, 20) are not combined into [3, 20)."
  (lkey nil :type fixnum)
  (rkey nil :type fixnum)
  (value nil :type element-type)
  (priority nil :type fixnum)
  (lnode nil :type (or null interval-map))
  (rnode nil :type (or null interval-map)))

(declaim (inline imap-map))
(defun imap-map (function imap)
  "Applies function to each interval [L, R) in IMAP in ascending order. FUNCTION
must take three arguments: L, R, and the assigned value."
  (labels ((recur (node)
             (when node
               (recur (%imap-lnode node))
               (funcall function (%imap-lkey node) (%imap-rkey node) (%imap-value node))
               (recur (%imap-rnode node)))))
    (recur imap)))

(defmethod print-object ((object interval-map) stream)
  (print-unreadable-object (object stream :type t)
    (let ((init t))
      (imap-map (lambda (l r value)
                  (if init
                      (setq init nil)
                      (write-char #\  stream))
                  (format stream "<[~A ~A): ~A>" l r value))
                object))))

(declaim (inline copy-imap))
(defun copy-imap (imap)
  (let ((res (%copy-imap imap)))
    (setf (%imap-priority res) (random #.(+ 1 most-positive-fixnum)))
    res))

(defun %heapify (node)
  "Makes NODE max-heap w.r.t. priorities by swapping the priorities in the whole
treap."
  (declare (optimize (speed 3))
           ((or null interval-map) node))
  (when node
    (let ((high-priority-node node))
      (when (and (%imap-lnode node)
                 (> (%imap-priority (%imap-lnode node))
                    (%imap-priority high-priority-node)))
        (setq high-priority-node (%imap-lnode node)))
      (when (and (%imap-rnode node)
                 (> (%imap-priority (%imap-rnode node))
                    (%imap-priority high-priority-node)))
        (setq high-priority-node (%imap-rnode node)))
      (unless (eql high-priority-node node)
        (rotatef (%imap-priority high-priority-node)
                 (%imap-priority node))
        (%heapify high-priority-node)))))

(declaim (inline make-imap))
(defun make-imap (start end &key initial-element initial-contents)
  (declare ((or null vector) initial-contents))
  (labels ((build (l r)
             (declare (fixnum l r))
             (if (= l r)
                 nil
                 (let* ((mid (ash (+ l r) -1))
                        (node (%make-interval-map mid (+ mid 1)
                                                  (if initial-contents
                                                      (aref initial-contents mid)
                                                      initial-element))))
                   (setf (%imap-lnode node) (build l mid)
                         (%imap-rnode node) (build (+ mid 1) r))
                   (%heapify node)
                   node))))
    (build start end)))

(declaim (inline %imap-leftmost-node))
(defun %imap-leftmost-node (imap)
  (labels ((recur (imap)
             (if (%imap-lnode imap)
                 (recur (%imap-lnode imap))
                 imap)))
    (recur imap)))

(declaim (inline %imap-rightmost-node))
(defun %imap-rightmost-node (imap)
  (labels ((recur (imap)
             (if (%imap-rnode imap)
                 (recur (%imap-rnode imap))
                 imap)))
    (recur imap)))

(declaim (ftype (function * (values (or null interval-map) &optional)) imap-concat))
(defun imap-concat (left right)
  (declare (optimize (speed 3)))
  "Destructively concatenates two treaps. Note that this function doesn't check
if the resultant map is valid or not."
  (cond ((null left) right)
        ((null right) left)
        ((> (%imap-priority left) (%imap-priority right))
         (setf (%imap-rnode left)
               (imap-concat (%imap-rnode left) right))
         left)
        (t
         (setf (%imap-lnode right)
               (imap-concat left (%imap-lnode right)))
         right)))

(declaim (ftype (function * (values (or null interval-map) (or null interval-map) &optional)
                          )
                %imap-lkey-split %imap-rkey-split))
(defun %imap-lkey-split (imap key)
  (declare (optimize (speed 3))
           (fixnum key))
  (labels ((recur (node)
             (cond ((null node) (values nil nil))
                   ((< (%imap-lkey node) key)
                    (multiple-value-bind (lnode rnode)
                        (recur (%imap-rnode node))
                      (setf (%imap-rnode node) lnode)
                      (values node rnode)))
                   (t
                    (multiple-value-bind (lnode rnode)
                        (recur (%imap-lnode node))
                      (setf (%imap-lnode node) rnode)
                      (values lnode node))))))
    (recur imap)))

(defun %imap-rkey-split (imap key)
  (declare (optimize (speed 3))
           (fixnum key))
  (labels ((recur (node)
             (cond ((null node) (values nil nil))
                   ((< key (%imap-rkey node))
                    (multiple-value-bind (lnode rnode)
                        (recur (%imap-lnode node))
                      (setf (%imap-lnode node) rnode)
                      (values lnode node)))
                   (t
                    (multiple-value-bind (lnode rnode)
                        (recur (%imap-rnode node))
                      (setf (%imap-rnode node) lnode)
                      (values node rnode))))))
    (recur imap)))

(declaim (inline %cut))
(defun %cut (imap)
  (setf (%imap-lnode imap) nil
        (%imap-rnode imap) nil))

(defun imap-insert (imap lkey rkey value)
  "Inserts an interval [LKEY, RKEY) to IMAP and assigns VALUE to it."
  (declare (optimize (speed 3))
           (fixnum lkey rkey))
  (assert (<= lkey rkey))
  (if (= lkey rkey)
      imap
      (multiple-value-bind (left tmp) (%imap-rkey-split imap lkey)
        (multiple-value-bind (mid right) (%imap-lkey-split tmp rkey)
          (let ((node (%make-interval-map lkey rkey value)))
            (when mid
              (let ((lmost-node (%imap-leftmost-node mid))
                    (rmost-node (%imap-rightmost-node mid)))
                ;; recycle lmost-node and rmost-node if possible
                (when (eql lmost-node rmost-node)
                  (setq rmost-node (copy-imap rmost-node)))
                (when (< (%imap-lkey lmost-node) lkey)
                  (%cut lmost-node)
                  (setf (%imap-rkey lmost-node) lkey
                        (%imap-lnode node) lmost-node))
                (when (< rkey (%imap-rkey rmost-node))
                  (%cut rmost-node)
                  (setf (%imap-lkey rmost-node) rkey
                        (%imap-rnode node) rmost-node)))
              (%heapify node))
            (imap-concat (imap-concat left node) right))))))

(declaim (ftype (function * (values (or null interval-map) (or null interval-map) &optional)
                          )
                imap-split))
(defun imap-split (imap key)
  "Splits IMAP to two interval-maps with reference to KEY."
  (declare (optimize (speed 3))
           (fixnum key))
  (multiple-value-bind (left tmp) (%imap-rkey-split imap key)
    (multiple-value-bind (mid right) (%imap-lkey-split tmp key)
      (if mid
          (let ((lmid mid)
                (rmid (copy-imap mid)))
            (setf (%imap-rkey lmid) key
                  (%imap-lkey rmid) key)
            (values (imap-concat left lmid)
                    (imap-concat rmid left)))
          (values left right)))))

(declaim (ftype (function * (values (or null element-type)
                                    (or null fixnum)
                                    (or null fixnum)
                                    &optional))
                imap-get))
(defun imap-get (imap key)
  "Returns threee values: the value assigned to KEY, left end of the interval
that contains KEY, and the right end of the same interval. Returns (VALUES NIL
NIL NIL) instead if IMAP doesn't contain KEY."
  (declare (optimize (speed 3))
           (fixnum key))
  (labels ((recur (node)
             (cond ((null node) (values nil nil nil))
                   ((< key (%imap-lkey node))
                    (recur (%imap-lnode node)))
                   ((< key (%imap-rkey node))
                    (values (%imap-value node) (%imap-lkey node) (%imap-rkey node)))
                   (t (recur (%imap-rnode node))))))
    (recur imap)))
