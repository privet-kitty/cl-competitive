(defpackage :cp/multi-slope-trick-old
  (:use :cl)
  (:export #:make-multi-slope-trick #:mstrick-add #:mstrick-add-abs #:mstrick-add-linear
           #:mstrick-delete #:mstrick-min #:mstrick-argmin #:mstrick-shift
           #:mstrick-left-cum #:mstrick-left-cum-rollback
           #:mstrick-right-cum #:mstrick-right-cum-rollback
           #:mstrick-arg-subdiff #:mstrick-subdiff)
  (:documentation "Provides slope trick with multiset."))
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

(defconstant +NEGATIVE-INF+ most-negative-fixnum)
(defconstant +POSITIVE-INF+ most-positive-fixnum)

(defstruct (multi-slope-trick (:constructor make-multi-slope-trick (&optional base-slope))
                              (:conc-name %mstrick-)
                              (:copier nil)
                              (:predicate nil))
  "Manages convex piecewise linear function. The primitive function the constructor
gives is a constant function. Note that this structure doesn't store a constant
term, i.e., it only gives you a slope."
  (base-slope 0 :type fixnum)
  (mset nil :type (or null mset)))

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
  (symbol-macrolet ((base-slope (%mstrick-base-slope mstrick))
                    (mset (%mstrick-mset mstrick)))
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
  (symbol-macrolet ((base-slope (%mstrick-base-slope mstrick))
                    (mset (%mstrick-mset mstrick)))
    (cond ((> weight 0)
           (setq mset (mset-delete mset a weight)))
          ((< weight 0)
           (setq mset (mset-delete mset a (- weight)))
           (decf base-slope weight)))
    mstrick))

(defun mstrick-add-abs (mstrick a weight)
  "Adds x |-> weight*abs(x-a) to f."
  (declare (fixnum a)
           ((integer 0 #.most-positive-fixnum) weight))
  (mstrick-add mstrick a weight)
  (mstrick-add mstrick a (- weight)))

(defun mstrick-add-linear (mstrick slope)
  "Adds a linear function x |-> slope*x to f."
  (declare (fixnum slope))
  (incf (%mstrick-base-slope mstrick) slope))

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

(defun mstrick-left-cum (mstrick c)
  "Clips the slope of f to (-infinity, C].

This function raises an error if C is less than the minimum slope of f. This
function returns the rest part, which is used to rollback this operation."
  (declare (optimize (speed 3))
           (fixnum c))
  (symbol-macrolet ((base-slope (%mstrick-base-slope mstrick))
                    (mset (%mstrick-mset mstrick)))
    (let ((rest-part (make-multi-slope-trick base-slope)))
      (cond ((<= c base-slope)
             (setf (%mstrick-mset rest-part) mset
                   mset nil))
            ((< c (+ base-slope (mset-size mset)))
             (multiple-value-bind (l r) (mset-indexed-split mset (- c base-slope))
               (setf mset l
                     (%mstrick-mset rest-part) r))))
      (setq base-slope (min base-slope c))
      rest-part)))

(defun mstrick-left-cum-rollback (mstrick rest-mstrick)
  "Rollbacks a LEFT-CUM operation.

Note that this function breaks REST-MSTRICK."
  (declare (optimize (speed 3)))
  (setf (%mstrick-mset mstrick) (mset-concat (%mstrick-mset mstrick) (%mstrick-mset rest-mstrick))
        (%mstrick-base-slope mstrick) (%mstrick-base-slope rest-mstrick))
  mstrick)

(defun mstrick-right-cum (mstrick c)
  "Clips the slope of f to [C, +infinity).

This function raises an error if C is greater than the maximum slope of f. This
function returns the rest part, which is used to rollback this operation."
  (declare (optimize (speed 3))
           (fixnum c))
  (symbol-macrolet ((base-slope (%mstrick-base-slope mstrick))
                    (mset (%mstrick-mset mstrick)))
    (let ((rest-part (make-multi-slope-trick base-slope)))
      (cond ((<= (+ base-slope (mset-size mset)) c)
             (setf (%mstrick-mset rest-part) mset
                   mset nil))
            ((< base-slope c)
             (multiple-value-bind (l r) (mset-indexed-split mset (- c base-slope))
               (setf mset r
                     (%mstrick-mset rest-part) l))))
      (setq base-slope (max base-slope c))
      rest-part)))

(defun mstrick-right-cum-rollback (mstrick rest-mstrick)
  "Rollbacks a RIGHT-CUM operation.

Note that this function breaks REST-MSTRICK."
  (declare (optimize (speed 3)))
  (setf (%mstrick-mset mstrick) (mset-concat (%mstrick-mset rest-mstrick) (%mstrick-mset mstrick))
        (%mstrick-base-slope mstrick) (%mstrick-base-slope rest-mstrick))
  mstrick)

(defun mstrick-shift (mstrick ldelta &optional (rdelta ldelta))
  "Replaces f to g such that g(x) := min_{x-rdelta <= t <= x-ldelta} f(t).
Shifts left breakpoints (slope < 0) by ldelta, right breakpoints (slope > 0) by rdelta."
  (declare (optimize (speed 3))
           (fixnum ldelta rdelta))
  (assert (<= ldelta rdelta))
  (symbol-macrolet ((base-slope (%mstrick-base-slope mstrick))
                    (mset (%mstrick-mset mstrick)))
    (when mset
      (cond
        ((<= 0 base-slope)
         (setq mset (mset-shift mset rdelta)))
        ((<= (+ base-slope (mset-size mset)) 0)
         (setq mset (mset-shift mset ldelta)))
        (t
         (multiple-value-bind (left right)
             (mset-indexed-split mset (- base-slope))
           (setq mset (mset-concat (mset-shift left ldelta)
                                   (mset-shift right rdelta))))))))
  mstrick)

