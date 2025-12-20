(defpackage :cp/multi-slope-trick-old
  (:use :cl)
  (:export #:make-multi-slope-trick #:mstrick-add #:mstrick-add-both
           #:mstrick-delete #:mstrick-min #:mstrick-argmin #:mstrick-shift
           #:mstrick-left-cum #:mstrick-right-cum
           #:mstrick-subdiff)
  (:documentation "Provides slope trick with multiset.

Note:
This module is obsolete. Use CP/MULTI-SLOPE-TRICK instead."))
(in-package :cp/multi-slope-trick-old)

(defstruct (mset (:constructor %make-mset
                     (key priority count &key left right (size count)))
                 (:conc-name %mset-))
  (key nil :type fixnum)
  (lazy 0 :type fixnum)
  (count nil :type (integer 0 #.most-positive-fixnum))
  (size nil :type (integer 0 #.most-positive-fixnum))
  (priority nil :type (integer 0 #.most-positive-fixnum))
  (left nil :type (or null mset))
  (right nil :type (or null mset)))

(declaim (inline mset-key))
(defun mset-key (mset)
  "Returns the root key of (nullable) MSET."
  (and mset (%mset-key mset)))

(declaim (inline mset-size))
(defun mset-size (mset)
  "Returns the total number of elements in MSET."
  (declare ((or null mset) mset))
  (if (null mset)
      0
      (%mset-size mset)))

(declaim (inline update-size))
(defun update-size (mset)
  (declare (mset mset))
  (setf (%mset-size mset)
        (if (%mset-left mset)
            (if (%mset-right mset)
                (let ((tmp (+ (%mset-size (%mset-left mset))
                              (%mset-count mset))))
                  (declare ((integer 0 #.most-positive-fixnum) tmp))
                  (+ tmp (%mset-size (%mset-right mset))))
                (+ (%mset-size (%mset-left mset))
                   (%mset-count mset)))
            (if (%mset-right mset)
                (+ (%mset-count mset)
                   (%mset-size (%mset-right mset)))
                (%mset-count mset)))))

(declaim (inline force-down))
(defun force-down (mset)
  (declare (mset mset))
  (let ((lazy (%mset-lazy mset)))
    (unless (zerop lazy)
      (setf (%mset-lazy mset) 0)
      (incf (%mset-key mset) lazy)
      (let ((left (%mset-left mset))
            (right (%mset-right mset)))
        (when left
          (incf (%mset-lazy left) lazy))
        (when right
          (incf (%mset-lazy right) lazy))))))

(declaim (ftype (function * (values (or null mset) (or null mset) &optional))
                mset-split))
(defun mset-split (mset key)
  "Destructively splits MSET with reference to KEY and returns two multisets,
the smaller sub-multiset (< KEY) and the larger one (>= KEY)."
  (declare (optimize (speed 3))
           ((or null mset) mset)
           (fixnum key))
  (labels ((recur (mset)
             (unless mset
               (return-from recur (values nil nil)))
             (force-down mset)
             (if (< (%mset-key mset) key)
                 (multiple-value-bind (left right) (recur (%mset-right mset))
                   (setf (%mset-right mset) left)
                   (update-size mset)
                   (values mset right))
                 (multiple-value-bind (left right) (recur (%mset-left mset))
                   (setf (%mset-left mset) right)
                   (update-size mset)
                   (values left mset)))))
    (recur mset)))

(declaim (ftype (function * (values (or null mset) (or null mset) &optional))
                mset-indexed-split))
(defun mset-indexed-split (mset index)
  "Destructively splits MSET at INDEX and returns two multisets, the smaller
sub-multiset and the larger one."
  (declare (optimize (speed 3))
           ((or null mset) mset)
           ((integer 0 #.most-positive-fixnum) index))
  (assert (<= index (mset-size mset)))
  (labels
      ((recur (mset index)
         (declare ((integer 0 #.most-positive-fixnum) index))
         (when (null mset)
           (return-from recur (values nil nil)))
         (force-down mset)
         (let* ((start (mset-size (%mset-left mset)))
                (end (+ start (%mset-count mset))))
           (declare ((integer 0 #.most-positive-fixnum) start end))
           (cond ((<= end index)
                  (multiple-value-bind (left right)
                      (recur (%mset-right mset) (- index end))
                    (setf (%mset-right mset) left)
                    (update-size mset)
                    (values mset right)))
                 ((<= index start)
                  (multiple-value-bind (left right)
                      (recur (%mset-left mset) index)
                    (setf (%mset-left mset) right)
                    (update-size mset)
                    (values left mset)))
                 (t
                  (let ((count (%mset-count mset))
                        (lnode (copy-mset mset))
                        (rnode mset))
                    (setf (%mset-count lnode) (- index start)
                          (%mset-count rnode) (- count (%mset-count lnode))
                          (%mset-right lnode) nil
                          (%mset-left rnode) nil)
                    (update-size lnode)
                    (update-size rnode)
                    (values lnode rnode)))))))
    (recur mset index)))

(declaim (ftype (function * (values (or null mset) &optional))
                %mset-concat))
(defun %mset-concat (left right)
  "Destructively concatenates two multisets. Assumes that all keys of LEFT are
**strictly** smaller than those of RIGHT."
  (declare (optimize (speed 3))
           ((or null mset) left right))
  (cond ((null left) right)
        ((null right) left)
        ((> (%mset-priority left) (%mset-priority right))
         (force-down left)
         (force-down right)
         (setf (%mset-right left)
               (%mset-concat (%mset-right left) right))
         (update-size left)
         left)
        (t
         (force-down left)
         (force-down right)
         (setf (%mset-left right)
               (%mset-concat left (%mset-left right)))
         (update-size right)
         right)))

(declaim (ftype (function * (values (or null mset) &optional))
                mset-concat))
(defun mset-concat (left right)
  "Destructively concatenates two multisets. Assumes that all keys of LEFT are
equal to or smaller than those of RIGHT.

This function includes %MSET-CONCAT, but it is not as fast."
  (declare (optimize (speed 3))
           ((or null mset) left right))
  (unless left
    (return-from mset-concat right))
  (unless right
    (return-from mset-concat left))
  (block preprocess
    (labels
        ((lrecur (mset)
           (force-down mset)
           (if (%mset-right mset)
               (lrecur (%mset-right mset))
               (let ((lend mset))
                 (labels
                     ((rrecur (mset)
                        (force-down mset)
                        (cond ((%mset-left mset)
                               (setf (%mset-left mset) (rrecur (%mset-left mset)))
                               (update-size mset)
                               mset)
                              ((= (%mset-key mset) (%mset-key lend))
                               (incf (%mset-count lend) (%mset-count mset))
                               (%mset-right mset))
                              (t (return-from preprocess)))))
                   (setq right (rrecur right)))))
           (update-size mset)
           mset))
      (setq left (lrecur left))))
  (%mset-concat left right))

(defun mset-insert (mset key count)
  "Destructively inserts KEY into MSET and returns the resultant multiset. You
cannot rely on the side effect. Use the returned value."
  (declare (optimize (speed 3))
           ((or null mset) mset)
           (fixnum key)
           ((integer 0 #.most-positive-fixnum) count))
  (when (zerop count)
    (return-from mset-insert mset))
  (labels ((recur (new-priority mset found)
             (declare ((integer 0 #.most-positive-fixnum) new-priority))
             (when mset
               (force-down mset))
             (let* ((new-found (or found
                                   (null mset)
                                   (> new-priority (%mset-priority mset))))
                    (res (cond ((null mset) nil)
                               ((< key (%mset-key mset))
                                (recur new-priority (%mset-left mset) new-found))
                               ((< (%mset-key mset) key)
                                (recur new-priority (%mset-right mset) new-found))
                               (t
                                (incf (%mset-count mset) count)
                                t))))
               (cond ((eql res t)
                      (update-size mset)
                      t)
                     ((mset-p res)
                      (if (< key (%mset-key mset))
                          (setf (%mset-left mset) res)
                          (setf (%mset-right mset) res))
                      (update-size mset)
                      mset)
                     ((not (eq found new-found))
                      (multiple-value-bind (left right) (mset-split mset key)
                        (let ((res (%make-mset key new-priority count
                                               :left left :right right)))
                          (update-size res)
                          res)))
                     (t nil)))))
    (let ((res (recur (random (+ 1 most-positive-fixnum)) mset nil)))
      (if (eql t res)
          mset
          res))))

(define-condition mset-empty-error (error)
  ((mset :initarg :mset :reader mset-empty-error-mset))
  (:report
   (lambda (condition stream)
     (format stream "Attempted to draw excessive number of elements from multiset ~W."
             (mset-empty-error-mset condition)))))

(declaim (ftype (function * (values (or null mset) &optional)) mset-delete))
(defun mset-delete (mset key count)
  "Destructively deletes KEY in MSET and returns the resultant multiset. You
cannot rely on the side effect. Use the returned multiset."
  (declare ((or null mset) mset)
           ((integer 0 #.most-positive-fixnum) count))
  (labels
      ((%error () (error 'mset-empty-error :mset mset))
       (recur (mset)
         (unless mset (%error))
         (force-down mset)
         (cond ((< key (%mset-key mset))
                (setf (%mset-left mset) (recur (%mset-left mset)))
                (update-size mset)
                mset)
               ((< (%mset-key mset) key)
                (setf (%mset-right mset) (recur (%mset-right mset)))
                (update-size mset)
                mset)
               (t
                (let ((current (%mset-count mset)))
                  (cond ((< current count) (%error))
                        ((> current count)
                         (decf (%mset-count mset) count)
                         (update-size mset)
                         mset)
                        (t
                         (%mset-concat (%mset-left mset)
                                       (%mset-right mset)))))))))
    (recur mset)))

(declaim (inline mset-map-run-length))
(defun mset-map-run-length (function mset)
  "Successively applies FUNCTION to each element of MSET in the underlying
order. FUNCTION must take two arguments: KEY and COUNT."
  (labels ((recur (mset)
             (when mset
               (force-down mset)
               (recur (%mset-left mset))
               (funcall function (%mset-key mset) (%mset-count mset))
               (recur (%mset-right mset)))))
    (recur mset)))

(defmethod print-object ((object mset) stream)
  (print-unreadable-object (object stream :type t)
    (let ((init t))
      (mset-map-run-length
       (lambda (key count)
         (if init
             (setq init nil)
             (write-char #\  stream))
         (format stream "<~A . ~A>" key count))
       object))))

(declaim (ftype (function * (values mset &optional)) mset-first))
(defun mset-first (mset)
  "Returns the leftmost node of MSET."
  (declare (optimize (speed 3))
           (mset mset))
  (force-down mset)
  (if (%mset-left mset)
      (mset-first (%mset-left mset))
      mset))

(declaim (ftype (function * (values mset &optional)) mset-last))
(defun mset-last (mset)
  "Returns the rightmost node of MSET."
  (declare (optimize (speed 3))
           (mset mset))
  (force-down mset)
  (if (%mset-right mset)
      (mset-last (%mset-right mset))
      mset))

(defun mset-shift (mset delta)
  "Adds DELTA to all keys in MSET."
  (declare (optimize (speed 3))
           (fixnum delta))
  (when mset
    (incf (%mset-lazy mset) delta)
    (force-down mset))
  mset)

(defstruct (multi-slope-trick (:constructor make-multi-slope-trick ())
                              (:conc-name %mstrick-)
                              (:copier nil)
                              (:predicate nil))
  "Manages convex piecewise linear function. The primitive function the constructor
gives is a constant function. Note that this structure doesn't store a constant
term, i.e., it only gives you a slope."
  (ltree nil :type (or null mset))
  (rtree nil :type (or null mset)))

(declaim (ftype (function * (values (or null fixnum) (or null fixnum) &optional))
                mstrick-argmin))
(defun mstrick-argmin (mstrick)
  "Returns two values: the left end and the right end of the closed interval on
which f takes the minimum. Positive and negative infinities are represented by
NIL."
  (let ((ltree (%mstrick-ltree mstrick))
        (rtree (%mstrick-rtree mstrick)))
    (values (when ltree (%mset-key (mset-last ltree)))
            (when rtree (%mset-key (mset-first rtree))))))

(defun mstrick-add (mstrick a weight)
  "Adds x |-> max(0, weight*(x-a)) to f."
  (declare (optimize (speed 3))
           (fixnum a weight))
  (symbol-macrolet ((ltree (%mstrick-ltree mstrick))
                    (rtree (%mstrick-rtree mstrick)))
    (cond ((> weight 0)
           (if (or (null ltree)
                   (<= (%mset-key (mset-last ltree)) a))
               (setq rtree (mset-insert rtree a weight))
               (progn
                 (setq ltree (mset-insert ltree a weight))
                 (multiple-value-bind (l r)
                     (mset-indexed-split ltree (- (mset-size ltree) weight))
                   (setq ltree l
                         rtree (mset-concat r rtree)))
                 ())))
          ((< weight 0)
           (setq weight (- weight))
           (if (or (null rtree)
                   (<= a (%mset-key (mset-first rtree))))
               (setq ltree (mset-insert ltree a weight))
               (progn
                 (setq rtree (mset-insert rtree a weight))
                 (multiple-value-bind (l r) (mset-indexed-split rtree weight)
                   (setq ltree (mset-concat ltree l)
                         rtree r))))))
    mstrick))

(defun mstrick-delete (mstrick a weight)
  "Subtracts x |-> max(0, weight*(x-a)) from f.

This is intended to be a rollback operation against MSTRICK-ADD. The behaviour
is undefined if this operation breaks convexity."
  (declare (optimize (speed 3))
           (fixnum a weight))
  (symbol-macrolet ((ltree (%mstrick-ltree mstrick))
                    (rtree (%mstrick-rtree mstrick)))
    (cond ((> weight 0)
           (handler-bind ((mset-empty-error
                            (lambda (c) (declare (ignorable c))
                              (multiple-value-bind (l r)
                                  (mset-indexed-split rtree weight)
                                (setq ltree (mset-concat ltree l)
                                      rtree r))
                              (setq ltree (mset-delete ltree a weight))
                              (return-from mstrick-delete mstrick))))
             (setq rtree (mset-delete rtree a weight))))
          ((< weight 0)
           (setq weight (- weight))
           (handler-bind ((mset-empty-error
                            (lambda (c) (declare (ignorable c))
                              (multiple-value-bind (l r)
                                  (mset-indexed-split ltree (- (mset-size ltree) weight))
                                (setq ltree l
                                      rtree (mset-concat r rtree)))
                              (setq rtree (mset-delete rtree a weight))
                              (return-from mstrick-delete mstrick))))
             (setq ltree (mset-delete ltree a weight)))))
    mstrick))

(defun test-mstrick ()
  (let ((ms (make-multi-slope-trick))
        (*random-state* (sb-ext:seed-random-state 0)))
    (mstrick-add ms -9 8)
    (print ms)
    (mstrick-add ms -8 5)
    (mstrick-add ms -8 -5)
    (print ms)
    (mstrick-delete ms -8 -5)))

(defun mstrick-add-both (mstrick a weight)
  "Adds x |-> weight*abs(x-a) to f."
  (declare (fixnum a)
           ((integer 0 #.most-positive-fixnum) weight))
  (mstrick-add mstrick a weight)
  (mstrick-add mstrick a (- weight)))

(defun mstrick-left-cum (mstrick)
  "Replaces f to g such that g(x) := min_{t <= x} f(t)."
  (setf (%mstrick-rtree mstrick) nil)
  mstrick)

(defun mstrick-right-cum (mstrick)
  "Replaces f to g such that g(x) := min_{x <= t} f(t)"
  (setf (%mstrick-ltree mstrick) nil)
  mstrick)

(defun mstrick-shift (mstrick ldelta &optional rdelta)
  "Replaces f to g such that g(x) := min_{x-r <= t <= x-l}f(x)."
  (declare (optimize (speed 3))
           (fixnum ldelta)
           ((or null fixnum) rdelta))
  (let ((rdelta (or rdelta ldelta)))
    (assert (<= ldelta rdelta))
    (mset-shift (%mstrick-ltree mstrick) ldelta)
    (mset-shift (%mstrick-rtree mstrick) rdelta)
    mstrick))

(declaim (ftype (function * (values fixnum fixnum &optional)) mstrick-subdiff))
(defun mstrick-subdiff (mstrick x)
  "Returns the subdifferential of f at x.

This function returns the left and right end of the closed interval of
subdifferential."
  (declare (optimize (speed 3))
           (fixnum x))
  (let* ((ltree (%mstrick-ltree mstrick))
         (rtree (%mstrick-rtree mstrick))
         (left-node (when ltree (mset-last ltree)))
         (right-node (when rtree (mset-first rtree))))
    (cond
      ((and left-node right-node
            (= x (%mset-key left-node) (%mset-key right-node)))
       (values (- (%mset-count left-node)) (%mset-count right-node)))
      ((and right-node (<= (%mset-key right-node) x))
       (labels
           ((recur (mset offset)
              (declare ((integer 0 #.most-positive-fixnum) offset))
              (force-down mset)
              (cond ((< x (%mset-key mset))
                     (if (%mset-left mset)
                         (recur (%mset-left mset) offset)
                         (values offset offset)))
                    ((< (%mset-key mset) x)
                     (if (%mset-right mset)
                         (recur (%mset-right mset)
                                (+ offset
                                   (%mset-count mset)
                                   (mset-size (%mset-left mset))))
                         (values (+ offset (%mset-size mset))
                                 (+ offset (%mset-size mset)))))
                    (t (let ((left-size (+ offset (mset-size (%mset-left mset)))))
                         (declare ((integer 0 #.most-positive-fixnum) left-size))
                         (values left-size (+ left-size (%mset-count mset))))))))
         (recur rtree 0)))
      ((and left-node (<= x (%mset-key left-node)))
       (labels
           ((recur (mset offset)
              (declare ((integer 0 #.most-positive-fixnum) offset))
              (force-down mset)
              (cond ((< (%mset-key mset) x)
                     (if (%mset-right mset)
                         (recur (%mset-right mset) offset)
                         (values offset offset)))
                    ((< x (%mset-key mset))
                     (if (%mset-left mset)
                         (recur (%mset-left mset)
                                (+ offset
                                   (%mset-count mset)
                                   (mset-size (%mset-right mset))))
                         (values (+ offset (%mset-size mset))
                                 (+ offset (%mset-size mset)))))
                    (t (let ((right-size (+ offset (mset-size (%mset-right mset)))))
                         (declare ((integer 0 #.most-positive-fixnum) right-size))
                         (values right-size (+ right-size (%mset-count mset))))))))
         (multiple-value-bind (l r) (recur ltree 0)
           (values (- r) (- l)))))
      (t (values 0 0)))))
