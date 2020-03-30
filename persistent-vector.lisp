;;;
;;; Persistent vector (unfinished)
;;;

;; NOTE: not tested

;; TODO:
;; - more sane handling of unbounded index
;; - handy function for initialization (currently `unbounded value' is zero)
;; - iteration; map
;; - abstraction

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
                   (t (recur (aref (%pv-children pvector)
                                   (mod index +persistent-vector-log+))
                             (floor index +persistent-vector-log+))))))
    (recur (or pvector 0) index)))

(defmethod print-object ((object persistent-vector) stream)
  (print-unreadable-object (object stream :type t)
    (let ((res (make-array 1))
          (max-index 0))
      (declare ((integer 0 #.most-positive-fixnum) max-index))
      (labels
          ((set! (index value)
             (declare ((integer 0 #.most-positive-fixnum) index))
             (when (>= index (length res))
               (setq res (adjust-array res (* 2 index))))
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
                 (unless (eq 0 (aref children i))
                   (let ((next-index (+ (* base i) index)))
                     (recur (aref children i) (+ depth 1) next-index)))))))
        (set! 0 (%pv-value object))
        (recur object 0 0))
      (write (adjust-array res (+ max-index 1)) :stream stream))))
