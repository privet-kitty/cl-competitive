(defpackage :cp/persistent-vector
  (:use :cl)
  (:export #:persistent-vector #:pv-assoc #:pv-ref)
  (:documentation "Provides fully persistent vector implemented with perfect
m-ary tree."))
(in-package :cp/persistent-vector)

;; TODO:
;; - more sane handling of unbound place
;; - handy function for initialization (currently `unbound value' is zero)
;; - iteration
;; - abstraction

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +log+ 16)
  (defconstant +default-value+ 0))

(defun test (vector)
  (declare (optimize (speed 3) (safety 0))
           ((simple-vector 16) vector))
  (let ((res (make-array 16 :element-type t)))
    (macrolet ((unrolled-copy (n)
                 `(progn
                    ,@(loop for i below n
                            collect `(setf (aref res ,i) (aref vector ,i))))))
      (unrolled-copy 16)
      res)))

(declaim (inline %make-persistent-vector))
(defstruct (persistent-vector (:constructor %make-persistent-vector ())
                              (:conc-name %pv-)
                              (:copier nil)
                              (:predicate nil))
  (value +default-value+ :type fixnum)
  (children nil :type (or null (simple-vector #.+log+))))

(defun pv-assoc (pvector index value)
  "Returns a new persistent vector whose value at INDEX is modified to
VALUE. PVECTOR can be null."
  (declare (optimize (speed 3))
           ((or null persistent-vector) pvector)
           ((integer 0 #.most-positive-fixnum) index))
  (labels ((recur (pvector index)
             (declare ((or (eql #.+default-value+) persistent-vector) pvector)
                      ((integer 0 #.most-positive-fixnum) index))
             (let ((res (%make-persistent-vector)))
               (if (eql +default-value+ pvector)
                   (setf (%pv-children res)
                         (make-array +log+
                                     :initial-element +default-value+))
                   (setf (%pv-children res) (copy-seq (%pv-children pvector))
                         (%pv-value res) (%pv-value pvector)))
               (if (zerop index)
                   (setf (%pv-value res) value)
                   (setf (aref (%pv-children res) (mod index +log+))
                         (recur (aref (%pv-children res) (mod index +log+))
                                (floor index +log+))))
               res)))
    (recur (or pvector +default-value+) index)))

(defun pv-ref (pvector index)
  "Returns a value at INDEX.

NOTE: currently unbound (or out-of-bound) value is zero."
  (declare (optimize (speed 3))
           ((or null persistent-vector) pvector))
  (labels ((recur (pvector index)
             (declare ((or (eql #.+default-value+) persistent-vector) pvector)
                      ((integer 0 #.most-positive-fixnum) index))
             (cond ((eql +default-value+ pvector)
                    +default-value+)
                   ((zerop index) (%pv-value pvector))
                   (t (recur (aref (%pv-children pvector)
                                   (mod index +log+))
                             (floor index +log+))))))
    (recur (or pvector +default-value+) index)))

(defmethod print-object ((object persistent-vector) stream)
  (print-unreadable-object (object stream :type t)
    (let ((res (make-array 1 :initial-element +default-value+))
          (max-index 0))
      (declare ((integer 0 #.most-positive-fixnum) max-index))
      (labels ((set! (index value)
                 (declare ((integer 0 #.most-positive-fixnum) index))
                 (when (>= index (length res))
                   (setq res (adjust-array res (* 2 index)
                                           :initial-element +default-value+)))
                 (setf (aref res index) value
                       max-index (max max-index index)))
               (recur (pvector depth index)
                 (declare ((integer 0 #.most-positive-fixnum) depth index))
                 (unless (zerop index)
                   (set! index (%pv-value pvector)))
                 (let ((base (expt +log+ depth))
                       (children (%pv-children pvector)))
                   (declare ((integer 0 #.most-positive-fixnum) base))
                   (dotimes (i +log+)
                     (unless (eql +default-value+ (aref children i))
                       (let ((next-index (+ (* base i) index)))
                         (recur (aref children i) (+ depth 1) next-index)))))))
        (set! 0 (%pv-value object))
        (recur object 0 0))
      (write (adjust-array res (+ max-index 1)
                           :initial-element +default-value+)
             :stream stream))))
