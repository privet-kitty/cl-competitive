;;
;; Complement to the bitwise operations in CLHS
;;

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

(defconstant +most-positive-word+ #.(- (ash 1 64) 1))

(defun bit-not! (sb-vector &optional (start 0) end)
  "Destructively flips the bits in the range [START, END)."
  (declare (optimize (speed 3))
           (simple-bit-vector sb-vector)
           ((integer 0 #.most-positive-fixnum) start)
           ((or null (integer 0 #.most-positive-fixnum)) end))
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

;; (count 1 simple-bit-vector) is sufficiently fast on SBCL when handling whole
;; vector. If START or END are specified, however, it is slow as the transformer
;; for COUNT doesn't work. See
;; https://github.com/sbcl/sbcl/blob/cd7af0d5b15e98e21ace8ef164e0f39019e5ed4b/src/compiler/generic/vm-tran.lisp#L484-L527
(defun bit-count (sb-vector &optional (start 0) end)
  "Counts 1's in the range [START, END)."
  (declare (optimize (speed 3))
           (simple-bit-vector sb-vector)
           ((integer 0 #.most-positive-fixnum) start)
           ((or null (integer 0 #.most-positive-fixnum)) end))
  (setq end (or end (length sb-vector)))
  (assert (<= start end (length sb-vector)))
  (multiple-value-bind (start/64 start%64) (floor start 64)
    (multiple-value-bind (end/64 end%64) (floor end 64)
      (declare (optimize (safety 0)))
      (if (= start/64 end/64)
          (logcount (ldb (byte (- end%64 start%64) start%64)
                         (sb-kernel:%vector-raw-bits sb-vector start/64)))
          (let ((result 0))
            (declare ((integer 0 #.most-positive-fixnum) result))
            (incf result (logcount (ldb (byte (- 64 start%64) start%64)
                                        (sb-kernel:%vector-raw-bits sb-vector start/64))))
            (loop for i from (+ 1 start/64) below end/64
                  do (incf result (logcount (sb-kernel:%vector-raw-bits sb-vector i))))
            (unless (zerop end%64)
              (incf result (logcount (ldb (byte end%64 0)
                                          (sb-kernel:%vector-raw-bits sb-vector end/64)))))
            result)))))

;; unfinished
;; (defun bit-shift (bit-vector delta &optional result-vector)
;;   "Shifts BIT-VECTOR by DELTA bits and fills the new bits with zero. Positive
;; DELTA means left-shifting and negative DELTA means right-shifting.

;; The behaviour is the same as the bit-wise operations in CLHS: The result is
;; copied to RESULT-VECTOR; if it is T, BIT-VECTOR is destructively modified; if it
;; is NIL, a new bit-vector of the same length is created."
;;   (declare (simple-bit-vector bit-vector)
;;            ((or null (eql t) simple-bit-vector) result-vector)
;;            (fixnum delta))
;;   (setq result-vector
;;         (etypecase result-vector
;;           (null (make-array (length bit-vector) :element-type 'bit :initial-element 0))
;;           ((eql t) bit-vector)
;;           (simple-bit-vector result-vector)))
;;   (when (>= delta 0)
;;     (return-from bit-shift (bit-lshift bit-vector delta result-vector)))
;;   (let* ((delta (- delta))
;;          (end (length bit-vector)))
;;     (unless (zerop end)
;;       (multiple-value-bind (d/64 d%64) (floor delta 64)
;;         (multiple-value-bind (end/64 end%64) (floor end 64)
;;           ;; process the initial word separately
;;           (when (and (> d%64 0) (< d/64 (ceiling end 64)))
;;             (setf (ldb (byte (- 64 d%64) 0)
;;                        (sb-kernel:%vector-raw-bits result-vector 0))
;;                   (ldb (byte (- 64 d%64) d%64)
;;                        (sb-kernel:%vector-raw-bits bit-vector d/64))))
;;           (do ((i (ceiling delta 64) (+ i 1)))
;;               ((>= i end/64))
;;             (setf (ldb (byte d%64 (- 64 d%64))
;;                        (sb-kernel:%vector-raw-bits result-vector (- i d/64 1)))
;;                   (ldb (byte d%64 0)
;;                        (sb-kernel:%vector-raw-bits bit-vector i)))
;;             (setf (ldb (byte (- 64 d%64) 0)
;;                        (sb-kernel:%vector-raw-bits result-vector (- i d/64)))
;;                   (ldb (byte (- 64 d%64) d%64)
;;                        (sb-kernel:%vector-raw-bits bit-vector i))))
;;           ;; process the last word separately
;;           (unless (zerop end%64)
;;             (setf (ldb (byte d%64 (- 64 d%64))
;;                        (sb-kernel:%vector-raw-bits result-vector (- end/64 d/64 1)))
;;                   (ldb (byte (min d%64 end%64) 0)
;;                        (sb-kernel:%vector-raw-bits bit-vector end/64)))
;;             (setf (ldb (byte (- 64 d%64) 0)
;;                        (sb-kernel:%vector-raw-bits result-vector (- end/64 d/64)))
;;                   (ldb (byte (max 0 (- end%64 d%64)) d%64)
;;                        (sb-kernel:%vector-raw-bits bit-vector end/64)))))))
;;     result-vector))

;; TODO: benchmark
;; TODO: right shift
(defun bit-lshift (bit-vector delta &optional result-vector end)
  "Left-shifts BIT-VECTOR by DELTA bits and fills the new bits with zero.

The behaviour is the same as the bit-wise operations in CLHS: The result is
copied to RESULT-VECTOR; if it is T, BIT-VECTOR is destructively modified; if it
is NIL, a new bit-vector of the same length is created. If END is specified,
this function shifts only the range [0, END) of BIT-VECTOR and copies it to the
range [0, END+DELTA) of RESULT-VECTOR."
  (declare (simple-bit-vector bit-vector)
           ((or null (eql t) simple-bit-vector) result-vector)
           ((integer 0 #.most-positive-fixnum) delta)
           ((or null (integer 0 #.most-positive-fixnum)) end))
  (setq result-vector
        (etypecase result-vector
          (null (make-array (length bit-vector) :element-type 'bit :initial-element 0))
          ((eql t) bit-vector)
          (simple-bit-vector result-vector)))
  (setq end (or end (length bit-vector)))
  (assert (<= end (length bit-vector)))
  (setq end (min end (max 0 (- (length result-vector) delta))))
  (multiple-value-bind (d/64 d%64) (floor delta 64)
    (declare (optimize (speed 3) (safety 0))
             (simple-bit-vector result-vector))
    (multiple-value-bind (end/64 end%64) (floor end 64)
      ;; process the last word separately
      (unless (zerop end%64)
        (let ((word (sb-kernel:%vector-raw-bits bit-vector end/64)))
          (setf (sb-kernel:%vector-raw-bits result-vector (+ end/64 d/64))
                (u64-dpb (ldb (byte (min end%64 (- 64 d%64)) 0) word)
                         (byte (min end%64 (- 64 d%64)) d%64)
                         (sb-kernel:%vector-raw-bits result-vector (+ end/64 d/64))))
          (when (> end%64 (- 64 d%64))
            (setf (ldb (byte (- end%64 (- 64 d%64)) 0)
                       (sb-kernel:%vector-raw-bits result-vector (+ 1 end/64 d/64)))
                  (ldb (byte (- end%64 (- 64 d%64)) (- 64 d%64)) word)))))
      (do ((i (- end/64 1) (- i 1)))
          ((< i 0))
        (let ((word (sb-kernel:%vector-raw-bits bit-vector i)))
          (declare ((unsigned-byte 64) word))
          (setf (sb-kernel:%vector-raw-bits result-vector (+ i d/64))
                (u64-dpb (ldb (byte (- 64 d%64) 0) word)
                         (byte (- 64 d%64) d%64)
                         (sb-kernel:%vector-raw-bits result-vector (+ i d/64))))
          (setf (ldb (byte d%64 0)
                     (sb-kernel:%vector-raw-bits result-vector (+ 1 i d/64)))
                (ldb (byte d%64 (- 64 d%64)) word))))
      ;; zero padding
      (when (< d/64 (ceiling (length result-vector) 64))
        (setf (ldb (byte d%64 0) (sb-kernel:%vector-raw-bits result-vector d/64)) 0))
      ;; REVIEW: May we set the last word of a bit vector to zero beyond the
      ;; actual bound?
      (dotimes (i (min d/64 (ceiling (length result-vector) 64)))
        (setf (sb-kernel:%vector-raw-bits result-vector i) 0))
      result-vector)))

(defun bench (size sample)
  (declare ((unsigned-byte 32) size sample))
  (let ((seq (make-array size :element-type 'bit))
        (state (sb-ext:seed-random-state 0)))
    (time (loop repeat sample
                sum (aref (bit-lshift seq (random 128 state)) 0) of-type bit))))
