;;
;; Complement to the bitwise operations in CLHS
;;

;; TODO: We could make it faster by preparing MASK in advance.
(defmacro u64-dpb (new spec int)
  (destructuring-bind (byte s p) spec
    (assert (eql 'byte byte))
    (let ((size (gensym)) (posn (gensym)) (mask (gensym)))
      `(let* ((,size ,s)
              (,posn ,p)
              (,mask (ldb (byte ,size 0) -1)))
         (logior (the (unsigned-byte 64) (ash (logand ,new ,mask) ,posn))
                 (the (unsigned-byte 64) (logand ,int (lognot (ash ,mask ,posn)))))))))

;; TODO: benchmark
;; TODO: right shift
(declaim (inline bit-shift))
(defun bit-lshift (bit-vector delta &optional dest-vector end)
  "Left-shifts BIT-VECTOR by DELTA bits and fills the lowert bits with zero.

The result is copied to DEST-VECTOR. (If it is NIL, BIT-VECTOR is destructively
modified.) If END is specified, this function shifts only the range [0, END) of
BIT-VECTOR and copies it to the range [0, END+DELTA) of DEST-VECTOR."
  (declare (simple-bit-vector bit-vector)
           ((or null simple-bit-vector) dest-vector)
           ((integer 0 #.most-positive-fixnum) delta)
           ((or null (integer 0 #.most-positive-fixnum)) end))
  (setf dest-vector (or dest-vector bit-vector))
  (setf end (or end (length bit-vector)))
  (assert (<= end (length bit-vector)))
  (setf end (min end (max 0 (- (length dest-vector) delta))))
  (multiple-value-bind (d/64 d%64) (floor delta 64)
    (multiple-value-bind (end/64 end%64) (floor end 64)
      (declare (optimize (speed 3) (safety 0)))
      ;; process the last bits separately
      (unless (zerop end%64)
        (let ((word (sb-kernel:%vector-raw-bits bit-vector end/64)))
          (setf (sb-kernel:%vector-raw-bits dest-vector (+ end/64 d/64))
                (u64-dpb (ldb (byte (min end%64 (- 64 d%64)) 0) word)
                         (byte (min end%64 (- 64 d%64)) d%64)
                         (sb-kernel:%vector-raw-bits dest-vector (+ end/64 d/64))))
          (when (> end%64 (- 64 d%64))
            (setf (ldb (byte (- end%64 (- 64 d%64)) 0)
                       (sb-kernel:%vector-raw-bits dest-vector (+ 1 end/64 d/64)))
                  (ldb (byte (- end%64 (- 64 d%64)) (- 64 d%64)) word)))))
      (loop for i from (- end/64 1) downto 0
            do (let ((word (sb-kernel:%vector-raw-bits bit-vector i)))
                 (declare ((unsigned-byte 64) word))
                 (setf (sb-kernel:%vector-raw-bits dest-vector (+ i d/64))
                       (u64-dpb (ldb (byte (- 64 d%64) 0) word)
                                (byte (- 64 d%64) d%64)
                                (sb-kernel:%vector-raw-bits dest-vector (+ i d/64))))
                 (setf (ldb (byte d%64 0)
                            (sb-kernel:%vector-raw-bits dest-vector (+ 1 i d/64)))
                       (ldb (byte d%64 (- 64 d%64)) word))))
      ;; zero padding
      (when (< d/64 (ceiling (length dest-vector) 64))
        (setf (ldb (byte d%64 0) (sb-kernel:%vector-raw-bits dest-vector d/64)) 0))
      ;; REVIEW: May we set the last word of a bit vector to zero beyond the
      ;; actual bound?
      (dotimes (i (min d/64 (ceiling (length dest-vector) 64)))
        (setf (sb-kernel:%vector-raw-bits dest-vector i) 0))
      dest-vector)))
