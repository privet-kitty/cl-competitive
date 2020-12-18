(defpackage :cp/multiset
  (:use :cl)
  (:export #:multiset #:multiset-empty-error #:multiset-empty-error-multiset
           #:multiset-concat #:multiset-split #:multiset-insert #:multiset-delete
           #:multiset-push #:multiset-pop #:multiset-map #:multiset-map-run-length
           #:multiset-find #:multiset-count #:multiset-first #:multiset-last
           #:multiset-size)
  (:documentation "Provides multiset implementation with access by index."))
(in-package :cp/multiset)

(define-condition multiset-empty-error (error)
  ((multiset :initarg :multiset :reader multiset-empty-error-multiset))
  (:report
   (lambda (condition stream)
     (format stream "Attempted to draw excessive number of elements from multiset ~W."
             (multiset-empty-error-multiset condition)))))

(defstruct (multiset (:constructor %make-multiset
                         (key priority count &key left right (size count)))
                     (:copier nil)
                     (:conc-name %multiset-))
  key
  (count 0 :type (integer 0 #.most-positive-fixnum))
  (size 0 :type (integer 0 #.most-positive-fixnum))
  (priority 0 :type (mod #.most-positive-fixnum))
  (left nil :type (or null multiset))
  (right nil :type (or null multiset)))

(declaim (inline multiset-key))
(defun multiset-key (multiset)
  "Returns the root key of (nullable) MULTISET."
  (and multiset (%multiset-key multiset)))

(declaim (inline multiset-size))
(defun multiset-size (multiset)
  "Returns the total number of elements in MULTISET."
  (declare ((or null multiset) multiset))
  (if (null multiset)
      0
      (%multiset-size multiset)))

(declaim (inline update-size))
(defun update-size (multiset)
  (declare (multiset multiset))
  (setf (%multiset-size multiset)
        (if (%multiset-left multiset)
            (if (%multiset-right multiset)
                (let ((tmp (+ (%multiset-size (%multiset-left multiset))
                              (%multiset-count multiset))))
                  (declare ((integer 0 #.most-positive-fixnum) tmp))
                  (+ tmp (%multiset-size (%multiset-right multiset))))
                (+ (%multiset-size (%multiset-left multiset))
                   (%multiset-count multiset)))
            (if (%multiset-right multiset)
                (+ (%multiset-count multiset)
                   (%multiset-size (%multiset-right multiset)))
                (%multiset-count multiset)))))

(declaim (ftype (function * (values (or null multiset) (or null multiset) &optional))
                multiset-split))
(defun multiset-split (multiset key &key (order #'<))
  "Destructively splits MULTISET with reference to KEY and returns two multisets,
the smaller sub-multiset (< KEY) and the larger one (>= KEY)."
  (declare (function order)
           ((or null multiset) multiset))
  (cond ((null multiset) (values nil nil))
        ((funcall order (%multiset-key multiset) key)
         (multiple-value-bind (left right)
             (multiset-split (%multiset-right multiset) key :order order)
           (setf (%multiset-right multiset) left)
           (update-size multiset)
           (values multiset right)))
        (t
         (multiple-value-bind (left right)
             (multiset-split (%multiset-left multiset) key :order order)
           (setf (%multiset-left multiset) right)
           (update-size multiset)
           (values left multiset)))))

(declaim (ftype (function * (values (or null multiset) &optional))
                multiset-concat))
(defun multiset-concat (left right)
  "Destructively concatenates two multisets. Assumes that all keys of LEFT are
smaller (or larger, depending on the order) than those of RIGHT."
  (declare (optimize (speed 3))
           ((or null multiset) left right))
  (cond ((null left) right)
        ((null right) left)
        ((> (%multiset-priority left) (%multiset-priority right))
         (setf (%multiset-right left)
               (multiset-concat (%multiset-right left) right))
         (update-size left)
         left)
        
        (t
         (setf (%multiset-left right)
               (multiset-concat left (%multiset-left right)))
         (update-size right)
         right)))

(declaim (inline multiset-insert))
(defun multiset-insert (multiset key &key (count 1) (order #'<))
  "Destructively inserts KEY into MULTISET and returns the resultant multiset. You
cannot rely on the side effect. Use the returned value."
  (declare ((or null multiset) multiset)
           (function order)
           ((integer 0 #.most-positive-fixnum) count))
  (when (zerop count)
    (return-from multiset-insert multiset))
  (labels ((find-and-update (multiset)
             "Updates COUNT slot and returns true if KEY exists."
             (cond ((null multiset) nil)
                   ((funcall order key (%multiset-key multiset))
                    (when (find-and-update (%multiset-left multiset))
                      (update-size multiset)
                      t))
                   ((funcall order (%multiset-key multiset) key)
                    (when (find-and-update (%multiset-right multiset))
                      (update-size multiset)
                      t))
                   (t
                    (incf (%multiset-count multiset) count)
                    (update-size multiset)
                    t)))
           (insert (node multiset)
             (cond ((null multiset) node)
                   ((> (%multiset-priority node) (%multiset-priority multiset))
                    (setf (values (%multiset-left node) (%multiset-right node))
                          (multiset-split multiset (%multiset-key node) :order order))
                    (update-size node)
                    node)
                   (t
                    (if (funcall order (%multiset-key node) (%multiset-key multiset))
                        (setf (%multiset-left multiset)
                              (insert node (%multiset-left multiset)))
                        (setf (%multiset-right multiset)
                              (insert node (%multiset-right multiset))))
                    (update-size multiset)
                    multiset))))
    (if (find-and-update multiset)
        multiset
        (insert (%make-multiset key (random most-positive-fixnum) count) multiset))))

(declaim (ftype (function * (values (or null multiset) &optional))
                multiset-delete))
(defun multiset-delete (multiset key &key (count 1) (order #'<) (empty-error-p t))
  "Destructively deletes KEY in MULTISET and returns the resultant multiset. You
cannot rely on the side effect. Use the returned multiset.

If EMPTY-ERROR-P is true, this function throws an MULTISET-EMPTY-ERROR when
excessive number of KEYs are attempted to be deleted."
  (declare ((or null multiset) multiset)
           (function order)
           ((integer 0 #.most-positive-fixnum) count))
  (let (found)
    (labels
        ((%error () (error 'multiset-empty-error :multiset multiset))
         (recur (multiset)
           (cond ((null multiset) nil)
                 ((funcall order key (%multiset-key multiset))
                  (setf (%multiset-left multiset)
                        (recur (%multiset-left multiset)))
                  (update-size multiset)
                  multiset)
                 ((funcall order (%multiset-key multiset) key)
                  (setf (%multiset-right multiset)
                        (recur (%multiset-right multiset)))
                  (update-size multiset)
                  multiset)
                 (t
                  (setq found t)
                  (let ((current (%multiset-count multiset)))
                    (cond ((and empty-error-p (< current count))
                           (%error))
                          ((> current count)
                           (decf (%multiset-count multiset) count)
                           (update-size multiset)
                           multiset)
                          (t
                           (multiset-concat (%multiset-left multiset)
                                            (%multiset-right multiset)))))))))
      (prog1 (recur multiset)
        (when (and empty-error-p (not found))
          (%error))))))

(defmacro multiset-push (key multiset order)
  "Pushes a KEY to MULTISET."
  `(setf ,multiset (multiset-insert ,multiset ,key :order ,order)))

(defmacro multiset-pop (key multiset order)
  "Deletes a KEY from MULTISET."
  `(setf ,multiset (multiset-delete ,multiset ,key :order ,order)))

(declaim (inline multiset-ref))
(defun multiset-count (multiset key &key (order #'<))
  "Returns the number of KEYs in MULTISET."
  (declare ((or null multiset) multiset))
  (labels ((recur (multiset)
             (cond ((null multiset) 0)
                   ((funcall order key (%multiset-key multiset))
                    (recur (%multiset-left multiset)))
                   ((funcall order (%multiset-key multiset) key)
                    (recur (%multiset-right multiset)))
                   (t (%multiset-count multiset)))))
    (recur multiset)))

(declaim (inline multiset-map-run-length))
(defun multiset-map-run-length (function multiset)
  "Successively applies FUNCTION to each element of MULTISET in the underlying
order. FUNCTION must take two arguments: KEY and COUNT:"
  (labels ((recur (multiset)
             (when multiset
               (recur (%multiset-left multiset))
               (funcall function (%multiset-key multiset) (%multiset-count multiset))
               (recur (%multiset-right multiset)))))
    (recur multiset)))

(declaim (inline multiset-map))
(defun multiset-map (function multiset)
  "Successively applies FUNCTION to each element of MULTISET in the underlying
order. This function only passes a key to FUNCTION and calls it as many times as
the number of the key in MULTISET."
  (labels ((recur (multiset)
             (when multiset
               (recur (%multiset-left multiset))
               (dotimes (_ (%multiset-count multiset))
                 (funcall function (%multiset-key multiset)))
               (recur (%multiset-right multiset)))))
    (recur multiset)))

(defmethod print-object ((object multiset) stream)
  (print-unreadable-object (object stream :type t)
    (let ((init t))
      (multiset-map-run-length
       (lambda (key count)
         (if init
             (setq init nil)
             (write-char #\  stream))
         (format stream "<~A . ~A>" key count))
       object))))

(defun multiset-first (multiset)
  "Returns the leftmost key of MULTISET."
  (declare (optimize (speed 3))
           (multiset multiset))
  (if (%multiset-left multiset)
      (multiset-first (%multiset-left multiset))
      (%multiset-key multiset)))

(defun multiset-last (multiset)
  "Returns the rightmost key of MULTISET."
  (declare (optimize (speed 3))
           (multiset multiset))
  (if (%multiset-right multiset)
      (multiset-last (%multiset-right multiset))
      (%multiset-key multiset)))

(defun multiset-find (multiset key &key (order #'<))
  "Finds and returns KEY if it exists, otherwise returns NIL. Equality is here
equivalent to 'neither larger nor smaller'."
  (declare (optimize (speed 3))
           (function order)
           ((or null multiset) multiset))
  (cond ((null multiset) nil)
        ((funcall order key (%multiset-key multiset))
         (multiset-find (%multiset-left multiset) key :order order))
        ((funcall order (%multiset-key multiset) key)
         (multiset-find (%multiset-right multiset) key :order order))
        (t key)))

;; under construction

;; (declaim (inline multiset-unite))
;; (defun multiset-unite (multiset1 multiset2 &key (order #'<))
;;   "Merges two multisets with keeping the order."
;;   (labels
;;       ((recur (l r)
;;          (cond ((null l) (when r (update-size r)) r)
;;                ((null r) (when l (update-size l)) l)
;;                (t (when (< (%multiset-priority l) (%multiset-priority r))
;;                     (rotatef l r))
;;                   (multiple-value-bind (lchild rchild)
;;                       (multiset-split r (%multiset-key l) :order order)
;;                     (setf (%multiset-left l) (recur (%multiset-left l) lchild)
;;                           (%multiset-right l) (recur (%multiset-right l) rchild))
;;                     (update-size l)
;;                     l)))))
;;     (recur multiset1 multiset2)))

;; (defun multiset-fold (multiset &key left right (order #'<))
;;   "Returns the sum (w.r.t. OP) of the half-open interval specified by the keys: [LEFT,
;; RIGHT). If LEFT [RIGHT] is not given, it is assumed to be -inf [+inf]."
;;   (declare (function order))
;;   (labels ((recur (multiset l r)
;;              (unless multiset
;;                (return-from recur 0))
;;              (prog1
;;                  (if (and (null l) (null r))
;;                      (%multiset-size multiset)
;;                      (let ((key (%multiset-key multiset)))
;;                        (if (or (null l) (not (funcall order key l))) ; L <= KEY
;;                            (if (or (null r) (funcall order key r)) ; KEY < R
;;                                (+ (+ (recur (%multiset-left multiset) l nil)
;;                                      (%multiset-count multiset))
;;                                   (recur (%multiset-right multiset) nil r))
;;                                (recur (%multiset-left multiset) l r))
;;                            (recur (%multiset-right multiset) l r))))
;;                (update-size multiset))))
;;     (recur multiset left right)))

;;;
;;; Binary search by key
;;;

;; (declaim (inline multiset-bisect-left))
;; (defun multiset-bisect-left (multiset key &key (order #'<))
;;   "Returns the smallest key equal to or larger than KEY. Returns NIL if KEY is
;; larger than any keys in MULTISET."
;;   (declare ((or null multiset) multiset)
;;            (function order))
;;   (labels ((recur (multiset)
;;              (cond ((null multiset) nil)
;;                    ((funcall order (%multiset-key multiset) key)
;;                     (recur (%multiset-right multiset)))
;;                    (t (or (recur (%multiset-left multiset))
;;                           multiset)))))
;;     (multiset-key (recur multiset))))

;; (declaim (inline multiset-bisect-left))
;; (defun multiset-bisect-right (multiset key &key (order #'<))
;;   "Returns the smallest key larger than KEY. Returns NIL if KEY is equal to or
;; larger than any keys in MULTISET."
;;   (declare ((or null multiset) multiset)
;;            (function order))
;;   (labels ((recur (multiset)
;;              (cond ((null multiset) nil)
;;                    ((funcall order key (%multiset-key multiset))
;;                     (or (recur (%multiset-left multiset))
;;                         multiset))
;;                    (t (recur (%multiset-right multiset))))))
;;     (multiset-key (recur multiset))))

;; (declaim (inline multiset-bisect-left-1))
;; (defun multiset-bisect-left-1 (multiset key &key (order #'<))
;;   "Returns the largest key smaller than KEY. Returns NIL if KEY is equal to or
;; smaller than any keys in MULTISET."
;;   (declare ((or null multiset) multiset)
;;            (function order))
;;   (labels ((recur (multiset)
;;              (cond ((null multiset) nil)
;;                    ((funcall order (%multiset-key multiset) key)
;;                     (or (recur (%multiset-right multiset))
;;                         multiset))
;;                    (t (recur (%multiset-left multiset))))))
;;     (multiset-key (recur multiset))))

;; (declaim (inline multiset-bisect-right-1))
;; (defun multiset-bisect-right-1 (multiset key &key (order #'<))
;;   "Returns the largest key equal to or smaller than KEY. Returns NIL if KEY is
;; smaller than any keys in MULTISET."
;;   (declare ((or null multiset) multiset)
;;            (function order))
;;   (labels ((recur (multiset)
;;              (cond ((null multiset) nil)
;;                    ((funcall order key (%multiset-key multiset))
;;                     (recur (%multiset-left multiset)))
;;                    (t (or (recur (%multiset-right multiset))
;;                           multiset)))))
;;     (multiset-key (recur multiset))))

;; ;; not tested
;; (defun multiset-fold-bisect (multiset count &key (order #'<))
;;   "Returns the smallest existing key that satisfies MULTISET[<1st key>]+ MULTISET[<2nd
;; key>] + ... + MULTISET[key] >= COUNT (if ORDER is #'<).

;; - This function deals with a **closed** interval. 
;; - This function returns NIL instead if MULTISET[<1st key>]+ ... + MULTISET[<last
;; key>] < COUNT.
;; - The prefix sums of MULTISET (MULTISET[<1st key>], MULTISET[<1st key>] + MULTISET[<2nd
;; key>], ...) must be monotone w.r.t. ORDER.
;; - ORDER must be a strict order"
;;   (labels
;;       ((recur (multiset prev-sum)
;;          (unless multiset
;;            (return-from recur))
;;          (let ((sum prev-sum))
;;            (cond ((not (funcall order
;;                                 (setq sum (+ sum (multiset-size (%multiset-left multiset))))
;;                                 count))
;;                   (if (%multiset-left multiset)
;;                       (recur (%multiset-left multiset) prev-sum)
;;                       (%multiset-key multiset)))
;;                  ((not (funcall order
;;                                 (setq sum (+ sum (%multiset-count multiset)))
;;                                 count))
;;                   (%multiset-key multiset))
;;                  (t (recur (%multiset-right multiset) sum))))))
;;     (recur multiset 0)))

;; (defun multiset-fold-bisect-from-end (multiset count &key (order #'<))
;;   "Returns the largest existing key that satisfies MULTISET[<key>] + ... +
;; MULTISET[<2nd last key>] + MULTISET[last key] >= COUNT (if ORDER is #'<).

;; - This function deals with a **closed** interval. 
;; - This function returns NIL instead if MULTISET[<1st key>]+ ... + MULTISET[<last
;; key>] < COUNT.
;; - The suffix sums of MULTISET (MULTISET[<last key>], MULTISET[<2nd last key>] +
;; MULTISET[<last key>], ...) must be monotone w.r.t. ORDER.
;; - ORDER must be a strict order"
;;   (labels
;;       ((recur (multiset prev-sum)
;;          (unless multiset
;;            (return-from recur))
;;          (let ((sum prev-sum))
;;            (cond ((not (funcall order
;;                                 (setq sum (+ (multiset-size (%multiset-right multiset)) sum))
;;                                 count))
;;                   (if (%multiset-right multiset)
;;                       (recur (%multiset-right multiset) prev-sum)
;;                       (%multiset-key multiset)))
;;                  ((not (funcall order
;;                                 (setq sum (+ (%multiset-count multiset) sum))
;;                                 count))
;;                   (%multiset-key multiset))
;;                  (t (recur (%multiset-left multiset) sum))))))
;;     (recur multiset 0)))
