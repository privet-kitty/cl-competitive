(defpackage :cp/multi-slope-trick
  (:use :cl)
  (:export #:make-multi-slope-trick #:mstrick-add #:mstrick-add-abs #:mstrick-add-linear
           #:mstrick-delete #:mstrick-min #:mstrick-argmin #:mstrick-shift
           #:mstrick-left-cum #:mstrick-left-cum-rollback
           #:mstrick-right-cum #:mstrick-right-cum-rollback
           #:mstrick-arg-subdiff #:mstrick-subdiff #:mstrick-value
           #:+negative-inf+ #:+positive-inf+)
  (:documentation "Provides slope trick with multiset."))
(in-package :cp/multi-slope-trick)

(defstruct (mset (:constructor %make-mset
                     (key priority count
                      &key left right (size count) (rkey key)))
                 (:conc-name %mset-))
  "
Slope trick interpretation of each slot:
- KEY: X coordinate of the left end of an interval
- RKEY: rightmost KEY of the subtree
- LAZY: amount of the right shift of the subtree
- COUNT: increment of the slope compared to the previous interval
- SIZE: slope
- AGG: Vertical increment from leftmost KEY to the rightmost KEY of the subtree,
i.e. sum of <difference of adjacent keys> * <size>.
"
  (key nil :type fixnum)
  (rkey nil :type fixnum)
  (lazy 0 :type fixnum)
  (count nil :type (integer 0 #.most-positive-fixnum))
  (size nil :type (integer 0 #.most-positive-fixnum))
  (agg 0 :type fixnum)
  (priority nil :type (integer 0 #.most-positive-fixnum))
  (left nil :type (or null mset))
  (right nil :type (or null mset)))

(declaim (inline mset-key))
(defun mset-key (mset)
  "Returns the root key of (nullable) MSET."
  (and mset (%mset-key mset)))

(declaim (inline mset-rkey))
(declaim (ftype (function * (values (or fixnum null) &optional)) mset-rkey))
(defun mset-rkey (mset)
  "Returns the rightmost key of (nullable) MSET."
  (and mset (+ (%mset-lazy mset) (%mset-rkey mset))))

(declaim (inline mset-size))
(defun mset-size (mset)
  "Returns the total number of elements in MSET."
  (declare ((or null mset) mset))
  (if (null mset)
      0
      (%mset-size mset)))

(declaim (inline mset-agg))
(defun mset-agg (mset)
  "Returns the aggregated value of MSET."
  (declare ((or null mset) mset))
  (if (null mset)
      0
      (%mset-agg mset)))

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

(declaim (inline force-up))
(defun force-up (mset)
  (declare (mset mset))
  (let ((left (%mset-left mset))
        (right (%mset-right mset)))
    (when right
      (setf (%mset-rkey mset)
            (+ (%mset-lazy right) (%mset-rkey right))))
    (setf (%mset-size mset)
          (the+ (integer 0 #.most-positive-fixnum)
                (mset-size left)
                (%mset-count mset)
                (mset-size right))
          (%mset-agg mset)
          (the+ fixnum
                (mset-agg left)
                (if left
                    (* (%mset-size left)
                       (the fixnum (- (%mset-rkey mset)
                                      (+ (%mset-lazy left) (%mset-rkey left)))))
                    0)
                (* (%mset-count mset)
                   (the fixnum (- (%mset-rkey mset) (%mset-key mset))))
                (mset-agg right)))))

(declaim (inline force-down))
(defun force-down (mset)
  (declare (mset mset))
  (let ((lazy (%mset-lazy mset)))
    (unless (zerop lazy)
      (setf (%mset-lazy mset) 0)
      (incf (%mset-key mset) lazy)
      (incf (%mset-rkey mset) lazy)
      (when (%mset-left mset)
        (incf (%mset-lazy (%mset-left mset)) lazy))
      (when (%mset-right mset)
        (incf (%mset-lazy (%mset-right mset)) lazy)))))

(defun mset-key-agg (mset key)
  "Returns the aggregated value at KEY of MSET."
  (declare (optimize (speed 3))
           ((or null mset) mset)
           (fixnum key))
  (labels
      ((recur (mset sum)
         (unless mset
           (return-from recur sum))
         (force-down mset)
         (let ((left (%mset-left mset)))
           (if (< key (%mset-key mset))
               (recur left sum)
               (recur (%mset-right mset)
                      (the+ fixnum
                            sum
                            (mset-agg left)
                            (if left
                                (* (%mset-size left)
                                   (the fixnum
                                        (- key
                                           (+ (%mset-lazy left) (%mset-rkey left)))))
                                0)
                            (* (%mset-count mset)
                               (the fixnum (- key (%mset-key mset))))))))))
    (recur mset 0)))

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
                   (force-up mset)
                   (values mset right))
                 (multiple-value-bind (left right) (recur (%mset-left mset))
                   (setf (%mset-left mset) right)
                   (force-up mset)
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
                    (force-up mset)
                    (values mset right)))
                 ((<= index start)
                  (multiple-value-bind (left right)
                      (recur (%mset-left mset) index)
                    (setf (%mset-left mset) right)
                    (force-up mset)
                    (values left mset)))
                 (t
                  (let ((count (%mset-count mset))
                        (lnode (copy-mset mset))
                        (rnode mset))
                    (setf (%mset-count lnode) (- index start)
                          (%mset-count rnode) (- count (%mset-count lnode))
                          (%mset-right lnode) nil
                          (%mset-left rnode) nil)
                    (force-up lnode)
                    (force-up rnode)
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
         (force-up left)
         left)
        (t
         (force-down left)
         (force-down right)
         (setf (%mset-left right)
               (%mset-concat left (%mset-left right)))
         (force-up right)
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
                               (force-up mset)
                               mset)
                              ((= (%mset-key mset) (%mset-key lend))
                               (incf (%mset-count lend) (%mset-count mset))
                               (%mset-right mset))
                              (t (return-from preprocess)))))
                   (declare (dynamic-extent #'rrecur))
                   (setq right (rrecur right)))))
           (force-up mset)
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
                      (force-up mset)
                      t)
                     ((mset-p res)
                      (if (< key (%mset-key mset))
                          (setf (%mset-left mset) res)
                          (setf (%mset-right mset) res))
                      (force-up mset)
                      mset)
                     ((not (eq found new-found))
                      (multiple-value-bind (left right) (mset-split mset key)
                        (let ((res (%make-mset key new-priority count
                                               :left left :right right)))
                          (force-up res)
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
  (declare (optimize (speed 3))
           (fixnum key)
           ((or null mset) mset)
           ((integer 0 #.most-positive-fixnum) count))
  (labels
      ((%error () (error 'mset-empty-error :mset mset))
       (recur (mset)
         (unless mset (%error))
         (force-down mset)
         (cond ((< key (%mset-key mset))
                (setf (%mset-left mset) (recur (%mset-left mset)))
                (force-up mset)
                mset)
               ((< (%mset-key mset) key)
                (setf (%mset-right mset) (recur (%mset-right mset)))
                (force-up mset)
                mset)
               (t
                (let ((current (%mset-count mset)))
                  (cond ((< current count) (%error))
                        ((> current count)
                         (decf (%mset-count mset) count)
                         (force-up mset)
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

(defun mset-ref (mset index)
  "Returns the INDEX-th element of MSET."
  (declare (optimize (speed 3))
           (mset mset)
           ((integer 0 #.most-positive-fixnum) index))
  (assert (< index (%mset-size mset)))
  (labels ((recur (mset parent-sum)
             (force-down mset)
             (let ((sum parent-sum))
               (declare ((integer 0 #.most-positive-fixnum) sum))
               (cond ((< index (incf sum (mset-size (%mset-left mset))))
                      (recur (%mset-left mset) parent-sum))
                     ((< index (incf sum (%mset-count mset)))
                      (%mset-key mset))
                     (t (recur (%mset-right mset) sum))))))
    (recur mset 0)))

(defun mset-ref-1 (mset index)
  "Returns the element right before the INDEX-th one of MSET.

This is equivalent to (INDEX-1)-th element, but I implement it separately for
the future expansion of this data structure to fractional count."
  (declare (optimize (speed 3))
           (mset mset)
           ((integer 1 #.most-positive-fixnum) index))
  (assert (<= index (%mset-size mset)))
  (labels ((recur (mset parent-sum)
             (force-down mset)
             (let ((sum parent-sum))
               (declare ((integer 0 #.most-positive-fixnum) sum))
               (cond ((<= index (incf sum (mset-size (%mset-left mset))))
                      (recur (%mset-left mset) parent-sum))
                     ((<= index (incf sum (%mset-count mset)))
                      (%mset-key mset))
                     (t (recur (%mset-right mset) sum))))))
    (recur mset 0)))

(declaim (ftype (function * (values (integer 0 #.most-positive-fixnum) &optional))
                mset-position-left))
(defun mset-position-left (mset key)
  "Returns the leftmost index at which KEY can be inserted with keeping the
order."
  (declare (optimize (speed 3))
           ((or null mset) mset)
           (fixnum key))
  (labels ((recur (mset)
             (when mset
               (force-down mset))
             (cond ((null mset) 0)
                   ((< (%mset-key mset) key)
                    (the (integer 0 #.most-positive-fixnum)
                         (+ (mset-size (%mset-left mset))
                            (%mset-count mset)
                            (recur (%mset-right mset)))))
                   (t
                    (recur (%mset-left mset))))))
    (recur mset)))

(declaim (ftype (function * (values (integer 0 #.most-positive-fixnum) &optional))
                mset-position-right))
(defun mset-position-right (mset key)
  "Returns the rightmost index at which KEY can be inserted with keeping the
order."
  (declare (optimize (speed 3))
           ((or null mset) mset)
           (fixnum key))
  (labels ((recur (mset)
             (when mset
               (force-down mset))
             (cond ((null mset) 0)
                   ((< key (%mset-key mset))
                    (recur (%mset-left mset)))
                   (t
                    (the (integer 0 #.most-positive-fixnum)
                         (+ (mset-size (%mset-left mset))
                            (%mset-count mset)
                            (recur (%mset-right mset))))))))
    (recur mset)))

(defun mset-shift (mset delta)
  "Adds DELTA to all keys in MSET."
  (declare (optimize (speed 3))
           (fixnum delta))
  (when mset
    (incf (%mset-lazy mset) delta)
    (force-down mset))
  mset)


(declaim (ftype (function * (values fixnum &optional)) mset-first))
(defun mset-first (mset)
  "Returns the leftmost key of MSET."
  (declare (optimize (speed 3))
           (mset mset))
  (force-down mset)
  (if (%mset-left mset)
      (mset-first (%mset-left mset))
      (%mset-key mset)))

(declaim (ftype (function * (values mset &optional)) mset-first-node))
(defun mset-first-node (mset)
  "Returns the leftmost node of MSET."
  (declare (optimize (speed 3))
           (mset mset))
  (force-down mset)
  (if (%mset-left mset)
      (mset-first-node (%mset-left mset))
      mset))

(defun mset-bisect-right (mset key)
"Returns the smallest key larger than KEY. Returns NIL if KEY is equal to or
larger than any keys in MSET."
(declare (optimize (speed 3))
         (fixnum key)
         ((or null mset) mset))
(labels ((recur (mset)
           (unless mset
             (return-from recur nil))
           (force-down mset)
           (if (< key (%mset-key mset))
               (or (recur (%mset-left mset))
                   mset)
               (recur (%mset-right mset)))))
  (mset-key (recur mset))))

(defconstant +negative-inf+ most-negative-fixnum)
(defconstant +positive-inf+ most-positive-fixnum)

(defstruct (multi-slope-trick
            (:constructor make-multi-slope-trick (&optional base-slope intercept))
            (:conc-name %mstrick-)
            (:copier nil)
            (:predicate nil))
  "Manages convex piecewise linear function. The primitive function the constructor
gives is a constant function. Note that this structure doesn't store a constant
term, i.e., it only gives you a slope."
  (base-slope 0 :type fixnum)
  (intercept 0 :type fixnum) ; value at leftmost breakpoint (value at 0 if linear)
  (mset nil :type (or null mset)))

(declaim (ftype (function * (values fixnum &optional)) mstrick-value))
(defun mstrick-value (mstrick x)
  "Returns the function value at X."
  (declare (optimize (speed 3))
           (fixnum x))
  (let ((mset (%mstrick-mset mstrick)))
    (the+ fixnum
          (%mstrick-intercept mstrick)
          (* (%mstrick-base-slope mstrick)
             (the fixnum (- x (if mset (mset-first mset) 0))))
          (mset-key-agg mset x))))

(declaim (ftype (function * (values fixnum fixnum &optional))
                mstrick-arg-subdiff))
(defun mstrick-arg-subdiff (mstrick diff)
  "Returns two values: the left and right end of the closed interval on which the
subdifferential of f contains DIFF. If it doesn't exist, it returns [-inf, -inf]
or [+inf, +inf], depending on if DIFF is below or above every slope of f."
  (declare (optimize (speed 3))
           (fixnum diff))
  (let* ((base-slope (%mstrick-base-slope mstrick))
         (mset (%mstrick-mset mstrick))
         (end-slope (+ base-slope (mset-size mset))))
    (cond ((< diff base-slope) (values +negative-inf+ +negative-inf+))
          ((< end-slope diff) (values +positive-inf+ +positive-inf+))
          (t (values (if (= diff base-slope)
                         +negative-inf+
                         (mset-ref-1 mset (the fixnum (- diff base-slope))))
                     (if (= diff end-slope)
                         +positive-inf+
                         (mset-ref mset (the fixnum (- diff base-slope)))))))))

(defun mstrick-add (mstrick a weight)
  "Adds x |-> max(0, weight*(x-a)) to f."
  (declare (optimize (speed 3))
           (fixnum a weight))
  (when (zerop weight)
    (return-from mstrick-add mstrick))
  (symbol-macrolet ((base-slope (%mstrick-base-slope mstrick))
                    (intercept (%mstrick-intercept mstrick))
                    (mset (%mstrick-mset mstrick)))
    (let* ((base-x-del (if mset (mset-first mset) 0))
           (base-x-add (if (null mset)
                           a
                           (min a base-x-del))))
      (declare (fixnum base-x-del base-x-add))
      (setq intercept
            (+ (if (= base-x-del base-x-add)
                   intercept
                   (mstrick-value mstrick base-x-add))
               (the* fixnum (max 0 (* weight (- base-x-add a)))))))
    (cond ((> weight 0)
           (setq mset (mset-insert mset a weight)))
          ((< weight 0)
           (incf base-slope weight)
           (setq mset (mset-insert mset a (- weight))))))
  mstrick)

(defun mstrick-delete (mstrick a weight)
  "Subtracts x |-> max(0, weight*(x-a)) from f.

This is intended to be a rollback operation against MSTRICK-ADD. The behavior is
undefined if this operation breaks convexity."
  (declare (optimize (speed 3))
           (fixnum a weight))
  (when (zerop weight)
    (return-from mstrick-delete mstrick))
  (symbol-macrolet ((base-slope (%mstrick-base-slope mstrick))
                    (intercept (%mstrick-intercept mstrick))
                    (mset (%mstrick-mset mstrick)))
    (let* ((post-base-x
             (if mset
                 (let ((first-node (mset-first-node mset)))
                   (if (and (= a (%mset-key first-node))
                            (= (abs weight) (%mset-count first-node)))
                       (or (mset-bisect-right mset a) 0)
                       (%mset-key first-node)))
                 0))
           (new-intercept (- (mstrick-value mstrick post-base-x)
                             (the* fixnum (max 0 (* weight (- post-base-x a)))))))
      (declare (fixnum post-base-x new-intercept))
      (cond ((> weight 0)
             (setq mset (mset-delete mset a weight)))
            ((< weight 0)
             (setq mset (mset-delete mset a (- weight)))
             (decf base-slope weight)))
      (setq intercept new-intercept))
    mstrick))

(defun mstrick-add-abs (mstrick a weight)
  "Adds x |-> weight*abs(x-a) to f."
  (declare (fixnum a)
           ((integer 0 #.most-positive-fixnum) weight))
  (mstrick-add mstrick a weight)
  (mstrick-add mstrick a (- weight)))

(defun mstrick-add-linear (mstrick slope)
  "Adds a linear function x |-> slope*x to f."
  (declare (fixnum slope)
           (optimize (speed 3)))
  (incf (%mstrick-base-slope mstrick) slope)
  (when (%mstrick-mset mstrick)
    (incf (%mstrick-intercept mstrick)
          (the fixnum (* slope (mset-first (%mstrick-mset mstrick)))))))

(declaim (ftype (function * (values fixnum fixnum &optional)) mstrick-subdiff))
(defun mstrick-subdiff (mstrick x)
  "Returns the subdifferential of f at x.

This function returns the left and right end of the closed interval of
subdifferential."
  (declare (optimize (speed 3))
           (fixnum x))
  (let* ((mset (%mstrick-mset mstrick))
         (base-slope (%mstrick-base-slope mstrick)))
    (if (null mset)
        (values base-slope base-slope)
        (let ((res-l (mset-position-left mset x))
              (res-r (mset-position-right mset x)))
          (values (the fixnum (+ base-slope res-l))
                  (the fixnum (+ base-slope res-r)))))))

(define-condition mstrick-breaking-convexity-error (error)
  ((mstrick :initarg :mstrick :reader mstrick-breaking-convexity-error-mstrick))
  (:report
   (lambda (condition stream)
     (format stream "Attempted to breaking convexity of ~W"
             (mstrick-breaking-convexity-error-mstrick condition)))))

(defun mstrick-left-cum (mstrick c)
  "Clips the slope of f to (-infinity, C].

This function raises an error if C is less than the minimum slope of f. This
function returns the rest part, which is used to rollback this operation."
  (declare (optimize (speed 3))
           (fixnum c))
  (symbol-macrolet ((base-slope (%mstrick-base-slope mstrick))
                    (intercept (%mstrick-intercept mstrick))
                    (mset (%mstrick-mset mstrick)))
    (let ((rest-part (make-multi-slope-trick base-slope intercept))
          (base-x (when mset (mset-first mset))))
      (cond ((< c base-slope)
             (error 'mstrick-breaking-convexity-error :mstrick mstrick))
            ((< c (+ base-slope (mset-size mset)))
             (multiple-value-bind (l r)
                 (mset-indexed-split mset (the fixnum (- c base-slope)))
               (setf mset l
                     (%mstrick-mset rest-part) r))))
      (when (and base-x (null mset))
        (decf intercept (the fixnum (* base-x base-slope))))
      (setq base-slope (min base-slope c))
      rest-part)))

(defun mstrick-left-cum-rollback (mstrick rest-mstrick)
  "Rollbacks a LEFT-CUM operation.

Note that this function breaks REST-MSTRICK."
  (declare (optimize (speed 3)))
  (setf (%mstrick-mset mstrick)
        (mset-concat (%mstrick-mset mstrick) (%mstrick-mset rest-mstrick))
        (%mstrick-base-slope mstrick) (%mstrick-base-slope rest-mstrick)
        (%mstrick-intercept mstrick) (%mstrick-intercept rest-mstrick))
  mstrick)

(defun mstrick-right-cum (mstrick c)
  "Clips the slope of f to [C, +infinity).

This function raises an error if C is greater than the maximum slope of f. This
function returns the rest part, which is used to rollback this operation."
  (declare (optimize (speed 3))
           (fixnum c))
  (symbol-macrolet ((base-slope (%mstrick-base-slope mstrick))
                    (intercept (%mstrick-intercept mstrick))
                    (mset (%mstrick-mset mstrick)))
    (let ((rest-part (make-multi-slope-trick base-slope intercept)))
      (multiple-value-bind (left-c right-c)
          (mstrick-arg-subdiff mstrick c)
        (cond ((= right-c +negative-inf+))
              ((= right-c +positive-inf+)
               (if (= left-c +positive-inf+)
                   (error 'mstrick-breaking-convexity-error :mstrick mstrick)
                   (let* ((end-x (if (/= +negative-inf+ left-c)
                                     left-c
                                     0))
                          (new-intercept (- (mstrick-value mstrick end-x)
                                           (the fixnum (* end-x c)))))
                     (setf (%mstrick-mset rest-part) mset)
                     (setq mset nil
                           base-slope c
                           intercept new-intercept))))
              (t
               (let ((new-intercept (mstrick-value mstrick right-c)))
                 (multiple-value-bind (l r)
                     (mset-indexed-split mset (the fixnum (- c base-slope)))
                   (setf (%mstrick-mset rest-part) l)
                   (setq mset r
                         base-slope c
                         intercept new-intercept))))))
      rest-part)))

(defun mstrick-right-cum-rollback (mstrick rest-mstrick)
  "Rollbacks a RIGHT-CUM operation.

Note that this function breaks REST-MSTRICK."
  (declare (optimize (speed 3)))
  (setf (%mstrick-mset mstrick)
        (mset-concat (%mstrick-mset rest-mstrick) (%mstrick-mset mstrick))
        (%mstrick-base-slope mstrick) (%mstrick-base-slope rest-mstrick)
        (%mstrick-intercept mstrick) (%mstrick-intercept rest-mstrick))
  mstrick)


(defun mstrick-shift (mstrick ldelta &optional (rdelta ldelta))
  "Replaces f to g such that g(x) := min_{x-rdelta <= t <= x-ldelta} f(t).
Shifts left breakpoints (slope < 0) by ldelta, right breakpoints (slope > 0) by rdelta."
  (declare (optimize (speed 3))
           (fixnum ldelta rdelta))
  (assert (<= ldelta rdelta))
  (symbol-macrolet ((base-slope (%mstrick-base-slope mstrick))
                    (intercept (%mstrick-intercept mstrick))
                    (mset (%mstrick-mset mstrick)))
    (if mset
        (cond
          ((<= 0 base-slope)
           (setq mset (mset-shift mset rdelta)))
          ((<= (+ base-slope (mset-size mset)) 0)
           (setq mset (mset-shift mset ldelta)))
          (t
           (multiple-value-bind (left right)
               (mset-indexed-split mset (- base-slope))
             (setq mset (mset-concat (mset-shift left ldelta)
                                     (mset-shift right rdelta))))))
        (if (< 0 base-slope)
            (decf intercept (the fixnum (* base-slope rdelta)))
            (decf intercept (the fixnum (* base-slope ldelta))))))
  mstrick)

(defun test-hand ()
  (let ((*random-state* (sb-ext:seed-random-state 2)))
    (let ((ms (make-multi-slope-trick -5)))
      ;; (print ms)
      (mstrick-add ms 4 -1)
      ;; (print ms)
      (mstrick-add ms 3 1)
      (print ms)
      (mstrick-shift ms -3 -3)
      (print ms)
      (mstrick-value ms 10))))
