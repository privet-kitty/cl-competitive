;; UNDER CONSTRUCTION
(defpackage :cp/multiset
  (:use :cl)
  (:export #:mset #:mset-empty-error #:mset-empty-error-mset
           #:mset-concat #:mset-split #:mset-insert #:mset-delete
           #:mset-push #:mset-pop #:mset-map #:mset-map-run-length
           #:mset-find #:mset-count #:mset-first #:mset-last
           #:mset-size #:mset-position-left #:mset-position-right
           #:mset-bisect-left #:mset-bisect-right #:mset-bisect-left-1 #:mset-bisect-right-1)
  (:documentation "Provides multiset implementation with access by index."))
(in-package :cp/multiset)

(define-condition mset-empty-error (error)
  ((mset :initarg :mset :reader mset-empty-error-mset))
  (:report
   (lambda (condition stream)
     (format stream "Attempted to draw excessive number of elements from multiset ~W."
             (mset-empty-error-mset condition)))))

(defstruct (mset (:constructor %make-mset
                     (key priority count &key left right (size count)))
                 (:copier nil)
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

(declaim (ftype (function * (values (or null mset) &optional))
                mset-concat))
(defun mset-concat (left right)
  "Destructively concatenates two multisets. Assumes that all keys of LEFT are
smaller (or larger, depending on the order) than those of RIGHT."
  (declare (optimize (speed 3))
           ((or null mset) left right))
  (cond ((null left) right)
        ((null right) left)
        ((> (%mset-priority left) (%mset-priority right))
         (setf (%mset-right left)
               (mset-concat (%mset-right left) right))
         (update-size left)
         left)
        
        (t
         (setf (%mset-left right)
               (mset-concat left (%mset-left right)))
         (update-size right)
         right)))

(declaim (inline mset-insert))
(defun mset-insert (mset key &key (count 1) (order #'<))
  "Destructively inserts KEY into MSET and returns the resultant multiset. You
cannot rely on the side effect. Use the returned value."
  (declare ((or null mset) mset)
           ((integer 0 #.most-positive-fixnum) count))
  (when (zerop count)
    (return-from mset-insert mset))
  (labels ((find-and-update (mset)
             "Updates COUNT slot and returns true if KEY exists."
             (cond ((null mset) nil)
                   ((funcall order key (%mset-key mset))
                    (when (find-and-update (%mset-left mset))
                      (update-size mset)
                      t))
                   ((funcall order (%mset-key mset) key)
                    (when (find-and-update (%mset-right mset))
                      (update-size mset)
                      t))
                   (t
                    (incf (%mset-count mset) count)
                    (update-size mset)
                    t)))
           (insert (node mset)
             (cond ((null mset) node)
                   ((> (%mset-priority node) (%mset-priority mset))
                    (setf (values (%mset-left node) (%mset-right node))
                          (mset-split mset (%mset-key node) :order order))
                    (update-size node)
                    node)
                   (t
                    (if (funcall order (%mset-key node) (%mset-key mset))
                        (setf (%mset-left mset)
                              (insert node (%mset-left mset)))
                        (setf (%mset-right mset)
                              (insert node (%mset-right mset))))
                    (update-size mset)
                    mset))))
    (if (find-and-update mset)
        mset
        (insert (%make-mset key (random most-positive-fixnum) count) mset))))

(declaim (ftype (function * (values (or null mset) &optional))
                mset-delete)
         (inline mset-delete))
(defun mset-delete (mset key &key (count 1) (order #'<) (empty-error-p t))
  "Destructively deletes KEY in MSET and returns the resultant multiset. You
cannot rely on the side effect. Use the returned multiset.

If EMPTY-ERROR-P is true, this function throws an MSET-EMPTY-ERROR when
excessive number of KEYs are attempted to be deleted."
  (declare ((or null mset) mset)
           ((integer 0 #.most-positive-fixnum) count))
  (let (found)
    (labels
        ((%error () (error 'mset-empty-error :mset mset))
         (recur (mset)
           (cond ((null mset) nil)
                 ((funcall order key (%mset-key mset))
                  (setf (%mset-left mset) (recur (%mset-left mset)))
                  (update-size mset)
                  mset)
                 ((funcall order (%mset-key mset) key)
                  (setf (%mset-right mset) (recur (%mset-right mset)))
                  (update-size mset)
                  mset)
                 (t
                  (setq found t)
                  (let ((current (%mset-count mset)))
                    (cond ((and empty-error-p (< current count))
                           (%error))
                          ((> current count)
                           (decf (%mset-count mset) count)
                           (update-size mset)
                           mset)
                          (t
                           (mset-concat (%mset-left mset)
                                        (%mset-right mset)))))))))
      (prog1 (recur mset)
        (when (and empty-error-p (not found))
          (%error))))))

(defmacro mset-push (key mset &optional (order '#'<))
  "Pushes a KEY to MSET."
  `(setf ,mset (mset-insert ,mset ,key :order ,order)))

(defmacro mset-pop (key mset &optional (order '#'<))
  "Deletes a KEY from MSET."
  `(setf ,mset (mset-delete ,mset ,key :order ,order)))

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

(defun mset-first (mset)
  "Returns the leftmost key of MSET."
  (declare (optimize (speed 3))
           (mset mset))
  (if (%mset-left mset)
      (mset-first (%mset-left mset))
      (%mset-key mset)))

(defun mset-last (mset)
  "Returns the rightmost key of MSET."
  (declare (optimize (speed 3))
           (mset mset))
  (if (%mset-right mset)
      (mset-last (%mset-right mset))
      (%mset-key mset)))

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

;; under construction

;; (declaim (inline mset-unite))
;; (defun mset-unite (mset1 mset2 &key (order #'<))
;;   "Merges two multisets with keeping the order."
;;   (labels
;;       ((recur (l r)
;;          (cond ((null l) r)
;;                ((null r) l)
;;                (t (when (< (%mset-priority l) (%mset-priority r))
;;                     (rotatef l r))
;;                   (multiple-value-bind (lchild rchild)
;;                       (mset-split r (%mset-key l) :order order)
;;                     (setf (%mset-left l) (recur (%mset-left l) lchild)
;;                           (%mset-right l) (recur (%mset-right l) rchild))
;;                     (update-size l)
;;                     l)))))
;;     (recur mset1 mset2)))

;; (defun mset-fold-bisect (mset count &key (order #'<))
;;   "Returns the smallest existing key that satisfies MSET[<1st key>]+ MSET[<2nd
;; key>] + ... + MSET[key] >= COUNT (if ORDER is #'<).

;; - This function deals with a **closed** interval. 
;; - This function returns NIL instead if MSET[<1st key>]+ ... + MSET[<last
;; key>] < COUNT.
;; - The prefix sums of MSET (MSET[<1st key>], MSET[<1st key>] + MSET[<2nd
;; key>], ...) must be monotone w.r.t. ORDER.
;; - ORDER must be a strict order"
;;   (labels
;;       ((recur (mset prev-sum)
;;          (unless mset
;;            (return-from recur))
;;          (let ((sum prev-sum))
;;            (cond ((not (funcall order
;;                                 (setq sum (+ sum (mset-size (%mset-left mset))))
;;                                 count))
;;                   (if (%mset-left mset)
;;                       (recur (%mset-left mset) prev-sum)
;;                       (%mset-key mset)))
;;                  ((not (funcall order
;;                                 (setq sum (+ sum (%mset-count mset)))
;;                                 count))
;;                   (%mset-key mset))
;;                  (t (recur (%mset-right mset) sum))))))
;;     (recur mset 0)))

;; (defun mset-fold-bisect-from-end (mset count &key (order #'<))
;;   "Returns the largest existing key that satisfies MSET[<key>] + ... +
;; MSET[<2nd last key>] + MSET[last key] >= COUNT (if ORDER is #'<).

;; - This function deals with a **closed** interval. 
;; - This function returns NIL instead if MSET[<1st key>]+ ... + MSET[<last
;; key>] < COUNT.
;; - The suffix sums of MSET (MSET[<last key>], MSET[<2nd last key>] +
;; MSET[<last key>], ...) must be monotone w.r.t. ORDER.
;; - ORDER must be a strict order"
;;   (labels
;;       ((recur (mset prev-sum)
;;          (unless mset
;;            (return-from recur))
;;          (let ((sum prev-sum))
;;            (cond ((not (funcall order
;;                                 (setq sum (+ (mset-size (%mset-right mset)) sum))
;;                                 count))
;;                   (if (%mset-right mset)
;;                       (recur (%mset-right mset) prev-sum)
;;                       (%mset-key mset)))
;;                  ((not (funcall order
;;                                 (setq sum (+ (%mset-count mset) sum))
;;                                 count))
;;                   (%mset-key mset))
;;                  (t (recur (%mset-left mset) sum))))))
;;     (recur mset 0)))
