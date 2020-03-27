;;;
;;; Persistent vector (unfinished)
;;;

;; NOTE: not tested

;; TODO:
;; - more sane handling of unbounded index
;; - handy function for initialization (currently `unbounded value' is zero)
;; - iteration; map
;; - abstraction
;; - printer

(defconstant +persistent-vector-log+ 16)

(declaim (inline %make-persistent-vector))
(defstruct (persistent-vector (:constructor %make-persistent-vector ())
                              (:conc-name %pv-))
  (value 0 :type fixnum)
  (children nil :type (or null (simple-vector #.+persistent-vector-log+))))

(defun pv-assoc (pvector index value)
  "Returns a new persistent vector whose value at INDEX is modified to
VALUE. PVECTOR can be null."
  (declare ((or null persistent-vector) pvector)
           ((integer 0 #.most-positive-fixnum) index))
  (labels ((recur (pvector index)
             (declare ((or (integer 0 0) persistent-vector) pvector)
                      ((integer 0 #.most-positive-fixnum) index))
             (let ((res (%make-persistent-vector)))
               (if (eql 0 pvector)
                   (setf (%pv-children res)
                         (make-array +persistent-vector-log+ :initial-element 0))
                   (setf (%pv-children res) (copy-seq (%pv-children pvector))
                         (%pv-value res) (%pv-value pvector)))
               (if (zerop index)
                   (setf (%pv-value res) value)
                   (setf (aref (%pv-children res) (mod index +persistent-vector-log+))
                         (recur (aref (%pv-children res) (mod index +persistent-vector-log+))
                                (floor index +persistent-vector-log+))))
               res)))
    (recur (or pvector 0) index)))

(defun pv-ref (pvector index)
  "Returns a value at INDEX.

NOTE: currently unbounded (or out-of-bound) value is zero."
  (declare ((or null persistent-vector) pvector))
  (labels ((recur (pvector index)
             (declare ((or (integer 0 0) persistent-vector) pvector)
                      ((integer 0 #.most-positive-fixnum) index))
             (cond ((eql 0 pvector) 0)
                   ((zerop index) (%pv-value pvector))
                   (t (recur (aref (%pv-children pvector) (mod index +persistent-vector-log+))
                             (floor index +persistent-vector-log+))))))
    (recur (or pvector 0) index)))
