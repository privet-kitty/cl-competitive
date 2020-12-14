(defpackage :cp/bit-basher
  (:use :cl)
  (:export #:bit-not! #:bit-fill! #:bit-count #:bit-lshift #:bit-rshift #:bit-shift)
  (:documentation "Provides several operations on bit vector that are not
included in the standard."))
(in-package :cp/bit-basher)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (assert (= sb-vm:n-word-bits 64)))

;; KLUDGE: a variant of DPB that handles a 64-bit word efficiently
(defmacro u64-dpb (new spec int)
  (destructuring-bind (byte s p) spec
    (assert (eql 'byte byte))
    (let ((size (gensym)) (posn (gensym)) (mask (gensym)))
      `(let* ((,size ,s)
              (,posn ,p)
              (,mask (ldb (byte ,size 0) -1)))
         (logior (the (unsigned-byte 64) (ash (logand ,new ,mask) ,posn))
                 (the (unsigned-byte 64) (logand ,int (lognot (ash ,mask ,posn)))))))))

(defconstant +most-positive-word+ (ldb (byte 64 0) -1))

(defun bit-not! (sb-vector &optional (start 0) end)
  "Destructively flips the bits in the range [START, END)."
  (declare (optimize (speed 3))
           (simple-bit-vector sb-vector)
           ((mod #.array-total-size-limit) start)
           ((or null (mod #.array-total-size-limit)) end))
  (setq end (or end (length sb-vector)))
  (assert (<= start end (length sb-vector)))
  (multiple-value-bind (start/64 start%64) (floor start 64)
    (multiple-value-bind (end/64 end%64) (floor end 64)
      (declare (optimize (safety 0)))
      (if (= start/64 end/64)
          (setf (sb-kernel:%vector-raw-bits sb-vector start/64)
                (u64-dpb (ldb (byte (- end%64 start%64) start%64)
                              (logxor +most-positive-word+ (sb-kernel:%vector-raw-bits sb-vector start/64)))
                         (byte (- end%64 start%64) start%64)
                         (sb-kernel:%vector-raw-bits sb-vector start/64)))
          (progn
            (setf (sb-kernel:%vector-raw-bits sb-vector start/64)
                  (dpb (sb-kernel:%vector-raw-bits sb-vector start/64)
                       (byte start%64 0)
                       (logxor +most-positive-word+ (sb-kernel:%vector-raw-bits sb-vector start/64))))
            (loop for i from (+ 1 start/64) below end/64
                  do (setf (sb-kernel:%vector-raw-bits sb-vector i)
                           (logxor +most-positive-word+ (sb-kernel:%vector-raw-bits sb-vector i))))
            (unless (zerop end%64)
              (setf (sb-kernel:%vector-raw-bits sb-vector end/64)
                    (dpb (logxor +most-positive-word+ (sb-kernel:%vector-raw-bits sb-vector end/64))
                         (byte end%64 0)
                         (sb-kernel:%vector-raw-bits sb-vector end/64))))))))
  sb-vector)

(declaim (ftype (function * (values simple-bit-vector &optional)) bit-fill!))
(defun bit-fill! (sb-vector bit &optional (start 0) end)
  "Destructively sets the bits in the range [START, END) to BIT."
  (declare (optimize (speed 3))
           (simple-bit-vector sb-vector)
           (bit bit)
           ((mod #.array-total-size-limit) start)
           ((or null (mod #.array-total-size-limit)) end))
  (setq end (or end (length sb-vector)))
  (assert (<= start end (length sb-vector)))
  (let ((mask (if (zerop bit) 0 +most-positive-word+)))
    (multiple-value-bind (start/64 start%64) (floor start 64)
      (multiple-value-bind (end/64 end%64) (floor end 64)
        (if (= start/64 end/64)
            (setf (sb-kernel:%vector-raw-bits sb-vector start/64)
                  (u64-dpb (ldb (byte (- end%64 start%64) 0) mask)
                           (byte (- end%64 start%64) start%64)
                           (sb-kernel:%vector-raw-bits sb-vector start/64)))
            (progn
              (setf (sb-kernel:%vector-raw-bits sb-vector start/64)
                    (u64-dpb (sb-kernel:%vector-raw-bits sb-vector start/64)
                             (byte start%64 0)
                             mask))
              (loop for i from (+ 1 start/64) below end/64
                    do (setf (sb-kernel:%vector-raw-bits sb-vector i) mask))
              (unless (zerop end%64)
                (setf (sb-kernel:%vector-raw-bits sb-vector end/64)
                      (dpb mask
                           (byte end%64 0)
                           (sb-kernel:%vector-raw-bits sb-vector end/64)))))))))
  sb-vector)

;; (count 1 simple-bit-vector) is sufficiently fast on SBCL when handling whole
;; vector. If START or END are specified, however, it is slow as the
;; deftransform for COUNT doesn't work. See
;; https://github.com/sbcl/sbcl/blob/cd7af0d5b15e98e21ace8ef164e0f39019e5ed4b/src/compiler/generic/vm-tran.lisp#L484-L527
(defun bit-count (sb-vector &optional (start 0) end)
  "Counts 1's in the range [START, END)."
  (declare (optimize (speed 3))
           (simple-bit-vector sb-vector)
           ((mod #.array-total-size-limit) start)
           ((or null (mod #.array-total-size-limit)) end))
  (setq end (or end (length sb-vector)))
  (assert (<= start end (length sb-vector)))
  (multiple-value-bind (start/64 start%64) (floor start 64)
    (multiple-value-bind (end/64 end%64) (floor end 64)
      (declare (optimize (safety 0)))
      (if (= start/64 end/64)
          (logcount (ldb (byte (- end%64 start%64) start%64)
                         (sb-kernel:%vector-raw-bits sb-vector start/64)))
          (let ((result 0))
            (declare ((mod #.array-total-size-limit) result))
            (incf result (logcount (ldb (byte (- 64 start%64) start%64)
                                        (sb-kernel:%vector-raw-bits sb-vector start/64))))
            (loop for i from (+ 1 start/64) below end/64
                  do (incf result (logcount (sb-kernel:%vector-raw-bits sb-vector i))))
            (unless (zerop end%64)
              (incf result (logcount (ldb (byte end%64 0)
                                          (sb-kernel:%vector-raw-bits sb-vector end/64)))))
            result)))))

(declaim (ftype (function * (values simple-bit-vector &optional)) bit-lshift))
(defun bit-lshift (bit-vector delta &optional result-vector end)
  "Left-shifts BIT-VECTOR by DELTA bits and fills the new bits with zero.
The behaviour is the same as the bit-wise operations in ANSI CL: The result is
copied to RESULT-VECTOR; if it is T, BIT-VECTOR is destructively modified; if it
is NIL, a new bit-vector of the same length is created. If END is specified,
this function shifts only the range [0, END) of BIT-VECTOR and copies it to the
range [0, END+DELTA) of RESULT-VECTOR.

Note that here `left' means the direction from a smaller index to a larger one
and is contrary to the `visual' direction: i.e. (bit-lshift #*1011000 2) |->
#*0010110."
  (declare (optimize (speed 3))
           (simple-bit-vector bit-vector)
           ((or null (eql t) simple-bit-vector) result-vector)
           ((mod #.array-total-size-limit) delta)
           ((or null (mod #.array-total-size-limit)) end))
  (setq result-vector
        (etypecase result-vector
          (null (make-array (length bit-vector) :element-type 'bit :initial-element 0))
          ((eql t) bit-vector)
          (simple-bit-vector result-vector)))
  (setq end (or end (length bit-vector)))
  (assert (<= end (length bit-vector)))
  (replace result-vector bit-vector :start1 (min (length result-vector) delta)
                                    :start2 0 :end2 end)
  (bit-fill! result-vector 0 0 (min delta (length result-vector))))

(declaim (ftype (function * (values simple-bit-vector &optional)) bit-rshift))
(defun bit-rshift (bit-vector delta &optional result-vector)
  "Right-shifts BIT-VECTOR by DELTA bits and fills the new bits with zero.
The behaviour is the same as the bit-wise operations in ANSI CL: The result is
copied to RESULT-VECTOR; if it is T, BIT-VECTOR is destructively modified; if it
is NIL, a new bit-vector of the same length is created.

Note that here `right' means the direction from a larger index to a smaller one
and is contrary to the `visual' direction: i.e. (bit-rshift #*1011000 2) |->
#*1100000."
  (declare (optimize (speed 3))
           (simple-bit-vector bit-vector)
           ((or null (eql t) simple-bit-vector) result-vector)
           ((mod #.array-total-size-limit) delta))
  (setq result-vector
        (etypecase result-vector
          (null (make-array (length bit-vector) :element-type 'bit :initial-element 0))
          ((eql t) bit-vector)
          (simple-bit-vector result-vector)))
  (replace result-vector bit-vector :start2 (min delta (length bit-vector)))
  (bit-fill! result-vector 0
             (min (max 0 (- (length bit-vector) delta)) (length result-vector))))

;; not tested
(declaim (ftype (function * (values simple-bit-vector &optional)) bit-shift))
(defun bit-shift (bit-vector delta &optional result-vector)
  (declare (optimize (speed 3))
           (simple-bit-vector bit-vector)
           ((or null (eql t) simple-bit-vector) result-vector)
           ((integer #.(- array-total-size-limit) #.array-total-size-limit) delta))
  (if (>= delta 0)
      (bit-lshift bit-vector delta result-vector)
      (bit-rshift bit-vector (- delta) result-vector)))

;; (defun bit-rotate (bit-vector delta &optional result-vector)
;;   (declare (optimize (speed 3))
;;            ((mod #.array-total-size-limit) delta)
;;            (simple-bit-vector bit-vector)
;;            ((or null simple-bit-vector) result-vector))
;;   (assert (not (eql bit-vector result-vector)))
;;   (let* ((end (length bit-vector))
;;          (result-vector (or result-vector (make-array end :element-type 'bit)))
;;          (delta (mod delta end)))
;;     :unfinished))
