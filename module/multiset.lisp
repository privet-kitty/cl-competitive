(defpackage :cp/multiset
  (:use :cl)
  (:export #:mset #:mset-empty-error #:mset-empty-error-mset
           #:%mset-concat #:mset-concat #:mset-split #:mset-indexed-split
           #:mset-insert #:mset-delete
           #:mset-push #:mset-pop #:mset-map #:mset-map-run-length
           #:mset-find #:mset-count #:mset-first #:mset-last #:mset-size
           #:mset-bisect-left #:mset-bisect-right #:mset-bisect-left-1 #:mset-bisect-right-1
           #:mset-position-left #:mset-position-right #:mset-ref)
  (:documentation "Provides multiset implementation."))
(in-package :cp/multiset)

(define-condition mset-empty-error (error)
  ((mset :initarg :mset :reader mset-empty-error-mset))
  (:report
   (lambda (condition stream)
     (format stream "Attempted to draw excessive number of elements from multiset ~W."
             (mset-empty-error-mset condition)))))

(defstruct (mset (:constructor %make-mset
                     (key priority count &key left right (size count)))
                 (:conc-name %mset-))
  key
  (count 0 :type (integer 0 #.most-positive-fixnum))
  (size 0 :type (integer 0 #.most-positive-fixnum))
  (priority 0 :type (mod #.most-positive-fixnum))
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

(declaim (ftype (function * (values (or null mset) (or null mset) &optional))
                mset-split)
         (inline mset-split))
(defun mset-split (mset key &key (order #'<))
  "Destructively splits MSET with reference to KEY and returns two multisets,
the smaller sub-multiset (< KEY) and the larger one (>= KEY)."
  (declare ((or null mset) mset))
  (labels ((recur (mset)
             (cond ((null mset) (values nil nil))
                   ((funcall order (%mset-key mset) key)
                    (multiple-value-bind (left right) (recur (%mset-right mset))
                      (setf (%mset-right mset) left)
                      (update-size mset)
                      (values mset right)))
                   (t
                    (multiple-value-bind (left right) (recur (%mset-left mset))
                      (setf (%mset-left mset) right)
                      (update-size mset)
                      (values left mset))))))
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
         (if (null mset)
             (values nil nil)
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
                        (values lnode rnode))))))))
    (recur mset index)))

(declaim (ftype (function * (values (or null mset) &optional))
                %mset-concat))
(defun %mset-concat (left right)
  "Destructively concatenates two multisets. Assumes that all keys of LEFT are
**strictly** smaller (or larger, depending on the order) than those of RIGHT."
  (declare (optimize (speed 3))
           ((or null mset) left right))
  (cond ((null left) right)
        ((null right) left)
        ((> (%mset-priority left) (%mset-priority right))
         (setf (%mset-right left)
               (%mset-concat (%mset-right left) right))
         (update-size left)
         left)
        (t
         (setf (%mset-left right)
               (%mset-concat left (%mset-left right)))
         (update-size right)
         right)))

(declaim (ftype (function * (values (or null mset) &optional))
                mset-concat)
         (inline mset-concat))
(defun mset-concat (left right &key (order #'<))
  "Destructively concatenates two multisets. Assumes that all keys of LEFT are
equal to or smaller (or larger, depending on the order) than those of RIGHT.

This function includes %MSET-CONCAT, but it is not as fast."
  (declare ((or null mset) left right))
  (block outer
    (labels
        ((lrecur (mset)
           (if (%mset-right mset)
               (lrecur (%mset-right mset))
               (let ((lend mset))
                 (labels
                     ((rrecur (mset)
                        (cond ((%mset-left mset)
                               (setf (%mset-left mset) (rrecur (%mset-left mset)))
                               (update-size mset)
                               mset)
                              ((not (or (funcall order (%mset-key mset) (%mset-key lend))
                                        (funcall order (%mset-key lend) (%mset-key mset))))
                               (incf (%mset-count lend) (%mset-count mset))
                               (%mset-right mset))
                              (t (return-from outer)))))
                   (setq right (rrecur right)))))
           (update-size mset)
           mset))
      (when (and left right)
        (setq left (lrecur left)))))
  (%mset-concat left right))

(declaim (inline mset-insert))
(defun mset-insert (mset key &key (count 1) (order #'<))
  "Destructively inserts KEY into MSET and returns the resultant multiset. You
cannot rely on the side effect. Use the returned value."
  (declare ((or null mset) mset)
           ((integer 0 #.most-positive-fixnum) count))
  (when (zerop count)
    (return-from mset-insert mset))
  (labels ((recur (new-priority mset found)
             (declare ((mod #.most-positive-fixnum) new-priority))
             (let* ((new-found (or found
                                   (null mset)
                                   (> new-priority (%mset-priority mset))))
                    (res (cond ((null mset) nil)
                               ((funcall order key (%mset-key mset))
                                (recur new-priority (%mset-left mset) new-found))
                               ((funcall order (%mset-key mset) key)
                                (recur new-priority (%mset-right mset) new-found))
                               (t
                                (incf (%mset-count mset) count)
                                t))))
               (cond ((eql res t)
                      (update-size mset)
                      t)
                     ((mset-p res)
                      (if (funcall order key (%mset-key mset))
                          (setf (%mset-left mset) res)
                          (setf (%mset-right mset) res))
                      (update-size mset)
                      mset)
                     ((not (eq found new-found))
                      (multiple-value-bind (left right) (mset-split mset key :order order)
                        (let ((res (%make-mset key new-priority count
                                               :left left :right right)))
                          (update-size res)
                          res)))
                     (t nil)))))
    (let ((res (recur (random (+ 1 most-positive-fixnum)) mset nil)))
      (if (eql t res)
          mset
          res))))

(declaim (ftype (function * (values (or null mset) &optional))
                mset-delete)
         (inline mset-delete))
(defun mset-delete (mset key &key (count 1) (order #'<) (error-p t))
  "Destructively deletes KEY in MSET and returns the resultant multiset. You
cannot rely on the side effect. Use the returned multiset.

If ERROR-P is true, this function throws an MSET-EMPTY-ERROR when excessive
number of KEYs are attempted to be deleted.

If COUNT is T, this function deletes all KEYs contained in MSET."
  (declare ((or null mset) mset)
           ((or (eql t) (integer 0 #.most-positive-fixnum)) count))
  (labels
      ((%error () (error 'mset-empty-error :mset mset))
       (recur (mset)
         (cond ((null mset)
                (when error-p (%error)))
               ((funcall order key (%mset-key mset))
                (setf (%mset-left mset) (recur (%mset-left mset)))
                (update-size mset)
                mset)
               ((funcall order (%mset-key mset) key)
                (setf (%mset-right mset) (recur (%mset-right mset)))
                (update-size mset)
                mset)
               (t
                (let ((current (%mset-count mset)))
                  (cond ((and error-p (< current count))
                         (%error))
                        ((> current count)
                         (decf (%mset-count mset) count)
                         (update-size mset)
                         mset)
                        (t
                         (%mset-concat (%mset-left mset)
                                       (%mset-right mset)))))))))
    (recur mset)))

(defmacro mset-push (key mset &optional (order '#'<) &environment env)
  "Pushes a KEY to MSET."
  (multiple-value-bind (temps vals stores setter getter)
      (get-setf-expansion mset env)
    `(let* (,@(mapcar #'list temps vals)
            (,(car stores) (mset-insert ,getter ,key :order ,order))
            ,@(cdr stores))
       ,setter)))

(defmacro mset-pop (key mset &optional (order '#'<) &environment env)
  "Deletes a KEY from MSET."
  (multiple-value-bind (temps vals stores setter getter)
      (get-setf-expansion mset env)
    `(let* (,@(mapcar #'list temps vals)
            (,(car stores) (mset-delete ,getter ,key :order ,order))
            ,@(cdr stores))
       ,setter)))

(declaim (inline mset-ref))
(defun mset-count (mset key &key (order #'<))
  "Returns the number of KEYs in MSET."
  (declare ((or null mset) mset))
  (labels ((recur (mset)
             (cond ((null mset) 0)
                   ((funcall order key (%mset-key mset))
                    (recur (%mset-left mset)))
                   ((funcall order (%mset-key mset) key)
                    (recur (%mset-right mset)))
                   (t (%mset-count mset)))))
    (recur mset)))

(declaim (inline mset-map-run-length))
(defun mset-map-run-length (function mset)
  "Successively applies FUNCTION to each element of MSET in the underlying
order. FUNCTION must take two arguments: KEY and COUNT:"
  (labels ((recur (mset)
             (when mset
               (recur (%mset-left mset))
               (funcall function (%mset-key mset) (%mset-count mset))
               (recur (%mset-right mset)))))
    (recur mset)))

(declaim (inline mset-map))
(defun mset-map (function mset)
  "Successively applies FUNCTION to each element of MSET in the underlying
order. This function only passes a key to FUNCTION and calls it as many times as
the number of the key in MSET."
  (labels ((recur (mset)
             (when mset
               (recur (%mset-left mset))
               (dotimes (_ (%mset-count mset))
                 (funcall function (%mset-key mset)))
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

(declaim (inline mset-first))
(defun mset-first (mset)
  "Returns the leftmost key of MSET."
  (declare (mset mset))
  (loop while (%mset-left mset)
        do (setq mset (%mset-left mset)))
  (%mset-key mset))

(declaim (inline mset-last))
(defun mset-last (mset)
  "Returns the rightmost key of MSET."
  (declare (mset mset))
  (loop while (%mset-right mset)
        do (setq mset (%mset-right mset)))
  (%mset-key mset))

(declaim (inline mset-find))
(defun mset-find (mset key &key (order #'<))
  "Finds and returns KEY if it exists, otherwise returns NIL. Equality is here
equivalent to 'neither larger nor smaller'."
  (declare ((or null mset) mset))
  (labels ((recur (mset)
             (cond ((null mset) nil)
                   ((funcall order key (%mset-key mset))
                    (recur (%mset-left mset)))
                   ((funcall order (%mset-key mset) key)
                    (recur (%mset-right mset)))
                   (t key))))
    (recur mset)))

(declaim (ftype (function * (values (integer 0 #.most-positive-fixnum) &optional))
                mset-position-left)
         (inline mset-position-left))
(defun mset-position-left (mset key &key (order #'<))
  "Returns the leftmost index at which KEY can be inserted with keeping the
order."
  (declare ((or null mset) mset))
  (labels ((recur (mset)
             (cond ((null mset) 0)
                   ((funcall order (%mset-key mset) key)
                    (the (integer 0 #.most-positive-fixnum)
                         (+ (mset-size (%mset-left mset))
                            (%mset-count mset)
                            (recur (%mset-right mset)))))
                   (t
                    (recur (%mset-left mset))))))
    (recur mset)))

(declaim (ftype (function * (values (integer 0 #.most-positive-fixnum) &optional))
                mset-position-right)
         (inline mset-position-right))
(defun mset-position-right (mset key &key (order #'<))
  "Returns the rightmost index at which KEY can be inserted with keeping the
order."
  (declare ((or null mset) mset))
  (labels ((recur (mset)
             (cond ((null mset) 0)
                   ((funcall order key (%mset-key mset))
                    (recur (%mset-left mset)))
                   (t
                    (the (integer 0 #.most-positive-fixnum)
                         (+ (mset-size (%mset-left mset))
                            (%mset-count mset)
                            (recur (%mset-right mset))))))))
    (recur mset)))

(defun mset-ref (mset index)
  "Returns the INDEX-th element of MSET."
  (declare (optimize (speed 3))
           (mset mset)
           ((integer 0 #.most-positive-fixnum) index))
  (assert (< index (%mset-size mset)))
  (labels ((recur (mset parent-sum)
             (let ((sum parent-sum))
               (declare ((integer 0 #.most-positive-fixnum) sum))
               (cond ((< index (incf sum (mset-size (%mset-left mset))))
                      (recur (%mset-left mset) parent-sum))
                     ((< index (incf sum (%mset-count mset)))
                      (%mset-key mset))
                     (t (recur (%mset-right mset) sum))))))
    (recur mset 0)))

;;;
;;; Binary search by key
;;;

(declaim (inline mset-bisect-left))
(defun mset-bisect-left (mset key &key (order #'<))
  "Returns the smallest key equal to or larger than KEY. Returns NIL if KEY is
larger than any keys in MSET."
  (declare ((or null mset) mset)
           (function order))
  (labels ((recur (mset)
             (cond ((null mset) nil)
                   ((funcall order (%mset-key mset) key)
                    (recur (%mset-right mset)))
                   (t (or (recur (%mset-left mset))
                          mset)))))
    (mset-key (recur mset))))

(declaim (inline mset-bisect-right))
(defun mset-bisect-right (mset key &key (order #'<))
  "Returns the smallest key larger than KEY. Returns NIL if KEY is equal to or
larger than any keys in MSET."
  (declare ((or null mset) mset)
           (function order))
  (labels ((recur (mset)
             (cond ((null mset) nil)
                   ((funcall order key (%mset-key mset))
                    (or (recur (%mset-left mset))
                        mset))
                   (t (recur (%mset-right mset))))))
    (mset-key (recur mset))))

(declaim (inline mset-bisect-left-1))
(defun mset-bisect-left-1 (mset key &key (order #'<))
  "Returns the largest key smaller than KEY. Returns NIL if KEY is equal to or
smaller than any keys in MSET."
  (declare ((or null mset) mset)
           (function order))
  (labels ((recur (mset)
             (cond ((null mset) nil)
                   ((funcall order (%mset-key mset) key)
                    (or (recur (%mset-right mset))
                        mset))
                   (t (recur (%mset-left mset))))))
    (mset-key (recur mset))))

(declaim (inline mset-bisect-right-1))
(defun mset-bisect-right-1 (mset key &key (order #'<))
  "Returns the largest key equal to or smaller than KEY. Returns NIL if KEY is
smaller than any keys in MSET."
  (declare ((or null mset) mset)
           (function order))
  (labels ((recur (mset)
             (cond ((null mset) nil)
                   ((funcall order key (%mset-key mset))
                    (recur (%mset-left mset)))
                   (t (or (recur (%mset-right mset))
                          mset)))))
    (mset-key (recur mset))))
