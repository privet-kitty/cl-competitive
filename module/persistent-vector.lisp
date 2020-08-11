;;;
;;; Fully persistent vector
;;; (implemented with perfect m-ary tree)
;;;

(defpackage :cp/persistent-vector
  (:use :cl)
  (:export #:persistent-vector #:pv-assoc #:pv-ref))
(in-package :cp/persistent-vector)

;; TODO:
;; - more sane handling of unbounded place
;; - handy function for initialization (currently `unbound value' is zero)
;; - iteration
;; - abstraction

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +persistent-vector-log+ 16)
  (defconstant +persistent-vector-default-value+ 0))

(declaim (inline %make-persistent-vector))
(defstruct (persistent-vector (:constructor %make-persistent-vector ())
                              (:conc-name %pv-))
  (value +persistent-vector-default-value+ :type fixnum)
  (children nil :type (or null (simple-vector #.+persistent-vector-log+))))

(defun pv-assoc (pvector index value)
  "Returns a new persistent vector whose value at INDEX is modified to
VALUE. PVECTOR can be null."
  (declare ((or null persistent-vector) pvector)
           ((integer 0 #.most-positive-fixnum) index))
  (labels ((recur (pvector index)
             (declare ((or (eql #.+persistent-vector-default-value+) persistent-vector) pvector)
                      ((integer 0 #.most-positive-fixnum) index))
             (let ((res (%make-persistent-vector)))
               (if (eql +persistent-vector-default-value+ pvector)
                   (setf (%pv-children res)
                         (make-array +persistent-vector-log+
                                     :initial-element +persistent-vector-default-value+))
                   (setf (%pv-children res) (copy-seq (%pv-children pvector))
                         (%pv-value res) (%pv-value pvector)))
               (if (zerop index)
                   (setf (%pv-value res) value)
                   (setf (aref (%pv-children res) (mod index +persistent-vector-log+))
                         (recur (aref (%pv-children res) (mod index +persistent-vector-log+))
                                (floor index +persistent-vector-log+))))
               res)))
    (recur (or pvector +persistent-vector-default-value+) index)))

(defun pv-ref (pvector index)
  "Returns a value at INDEX.

NOTE: currently unbounded (or out-of-bound) value is zero."
  (declare ((or null persistent-vector) pvector))
  (labels ((recur (pvector index)
             (declare ((or (eql #.+persistent-vector-default-value+) persistent-vector) pvector)
                      ((integer 0 #.most-positive-fixnum) index))
             (cond ((eql +persistent-vector-default-value+ pvector)
                    +persistent-vector-default-value+)
                   ((zerop index) (%pv-value pvector))
                   (t (recur (aref (%pv-children pvector)
                                   (mod index +persistent-vector-log+))
                             (floor index +persistent-vector-log+))))))
    (recur (or pvector +persistent-vector-default-value+) index)))

(defmethod print-object ((object persistent-vector) stream)
  (print-unreadable-object (object stream :type t)
    (let ((res (make-array 1 :initial-element +persistent-vector-default-value+))
          (max-index 0))
      (declare ((integer 0 #.most-positive-fixnum) max-index))
      (labels
          ((set! (index value)
             (declare ((integer 0 #.most-positive-fixnum) index))
             (when (>= index (length res))
               (setq res (adjust-array res (* 2 index)
                                       :initial-element +persistent-vector-default-value+)))
             (setf (aref res index) value
                   max-index (max max-index index)))
           (recur (pvector depth index)
             (declare ((integer 0 #.most-positive-fixnum) depth index))
             (unless (zerop index)
               (set! index (%pv-value pvector)))
             (let ((base (expt +persistent-vector-log+ depth))
                   (children (%pv-children pvector)))
               (declare ((integer 0 #.most-positive-fixnum) base))
               (dotimes (i +persistent-vector-log+)
                 (unless (eql +persistent-vector-default-value+ (aref children i))
                   (let ((next-index (+ (* base i) index)))
                     (recur (aref children i) (+ depth 1) next-index)))))))
        (set! 0 (%pv-value object))
        (recur object 0 0))
      (write (adjust-array res (+ max-index 1)
                           :initial-element +persistent-vector-default-value+)
             :stream stream))))
