;;;
;;; Fast operations on GF(2)
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (assert (= sb-vm:n-word-bits 64)))

;; Reference: https://www.pvk.ca/Blog/2014/08/16/how-to-define-new-intrinsics-in-sbcl/
;; We need to define POPCNT by ourselves as the version of SBCL is 1.1.14 on AtCoder.
(sb-c:defknown popcnt ((unsigned-byte 64)) (integer 0 64)
    (sb-c:foldable sb-c:flushable sb-c:movable)
  :overwrite-fndb-silently t)

(sb-c:defknown popcnt ((unsigned-byte 64)) (integer 0 64)
    (sb-c:foldable sb-c:flushable sb-c:movable)
  :overwrite-fndb-silently t)

(sb-vm::define-vop (popcnt)
  (:policy :fast-safe)
  (:translate popcnt)
  (:args (x :scs (sb-vm::unsigned-reg) :target r))
  (:arg-types sb-vm::unsigned-num)
  (:results (r :scs (sb-vm::unsigned-reg)))
  (:result-types sb-vm::unsigned-num)
  (:generator 3
              (unless (sb-vm::location= r x)
                (sb-vm::inst xor r r))
              (sb-vm::inst popcnt r x)))

(sb-vm::define-vop (popcnt/fx)
  (:policy :fast-safe)
  (:translate popcnt)
  (:args (x :scs (sb-vm::unsigned-reg) :target r))
  (:arg-types sb-vm::positive-fixnum)
  (:results (r :scs (sb-vm::unsigned-reg)))
  (:result-types sb-vm::unsigned-num)
  (:generator 2
              (unless (sb-vm::location= r x)
                (sb-vm::inst xor r r))
              (sb-vm::inst popcnt r x)))

(defun popcnt (x)
  (popcnt x))

(defun f2-gemm (a b)
  "Calculates A*B on GF(2). The width of A (and the height of B) must be
multiple of 64."
  (declare (optimize (speed 3))
           ((simple-array bit (* *)) a b))
  (multiple-value-bind (length/64 rem) (floor (array-dimension a 1) 64)
    (declare (optimize (safety 0)))
    (assert (zerop rem))
    (assert (= (array-dimension a 1) (array-dimension b 0)))
    (let* ((tb (make-array (list (array-dimension b 1) (array-dimension b 0))
                           :element-type 'bit)) ; transposed B
           (c (make-array (list (array-dimension a 0) (array-dimension b 1))
                          :element-type 'bit))
           (a-storage (array-storage-vector a))
           (tb-storage (array-storage-vector tb)))
      (dotimes (row (array-dimension b 0))
        (dotimes (col (array-dimension b 1))
          (setf (aref tb row col) (aref b col row))))
      (dotimes (row (array-dimension a 0))
        (dotimes (col (array-dimension b 1))
          (let ((res 0)
                (a-index (floor (array-row-major-index a row 0) 64))
                (tb-index (floor (array-row-major-index tb col 0) 64)))
            (declare (bit res))
            (dotimes (k length/64)
              (setf res
                    (logxor res
                            (ldb (byte 1 0)
                                 (popcnt (logand (sb-kernel:%vector-raw-bits a-storage (+ k a-index))
                                                 (sb-kernel:%vector-raw-bits tb-storage (+ k tb-index))))))))
            (setf (aref c row col) res))))
      c)))

(defun f2-gemv (a v)
  "Calculates A*v on GF(2). The width of A (and the length of v) must be
multiple of 64."
  (declare (optimize (speed 3))
           ((simple-array bit (* *)) a)
           ((simple-array bit (*)) v))
  (multiple-value-bind (length/64 rem) (floor (length v) 64)
    (declare (optimize (safety 0)))
    (assert (zerop rem))
    (assert (= (array-dimension a 1) (length v)))
    (let* ((res (make-array (array-dimension a 0) :element-type 'bit))
           (a-storage (array-storage-vector a))
           (v-storage (array-storage-vector v)))
      (dotimes (row (array-dimension a 0))
        (let ((value 0)
              (a-index (floor (array-row-major-index a row 0) 64)))
          (declare (bit value))
          (dotimes (k length/64)
            (setf value
                  (logxor value
                          (ldb (byte 1 0)
                               (popcnt (logand (sb-kernel:%vector-raw-bits a-storage (+ k a-index))
                                               (sb-kernel:%vector-raw-bits v-storage k)))))))
          (setf (aref res row) value)))
      res)))

(defun f2-echelon! (matrix &optional extended)
  "Returns the row echelon form of MATRIX by gaussian elimination on GF(2). If
EXTENDED is true, the last column is regarded as the right hand side of the
linear equations and is not eliminated. This function destructively modifies
MATRIX.

The width of MATRIX must be multiple of 64."
  (declare (optimize (speed 3))
           ((simple-array bit (* *)) matrix))
  (destructuring-bind (m n) (array-dimensions matrix)
    (declare (optimize (safety 0))
             ((integer 0 #.most-positive-fixnum) m n))
    (multiple-value-bind (n/64 rem) (floor n 64)
      (assert (zerop rem))
      (let* ((storage (array-storage-vector matrix))
             (rank 0))
        (declare (fixnum rank))
        (dotimes (target-col (if extended (- n 1) n))
          (let* ((pivot-row (do ((i rank (+ 1 i)))
                                ((= i m) -1)
                              (unless (zerop (aref matrix i target-col))
                                (return i)))))
            (when (>= pivot-row 0)
              (let ((pivot-row/64 (floor (array-row-major-index matrix pivot-row 0) 64))
                    (rank-row/64 (floor (array-row-major-index matrix rank 0) 64)))
                ;; swap rows
                (loop
                  for k from 0 below n/64
                  do (rotatef (sb-kernel:%vector-raw-bits storage (+ rank-row/64 k))
                              (sb-kernel:%vector-raw-bits storage (+ pivot-row/64 k))))
                ;; eliminate the column
                (dotimes (i m)
                  (unless (or (= i rank) (zerop (aref matrix i target-col)))
                    (loop
                      with base/64 = (floor (array-row-major-index matrix i 0) 64)
                      for k below n/64
                      do (setf (sb-kernel:%vector-raw-bits storage (+ base/64 k))
                               (logxor (sb-kernel:%vector-raw-bits storage (+ base/64 k))
                                       (sb-kernel:%vector-raw-bits storage (+ rank-row/64 k))))))))
              (incf rank))))
        (values matrix rank)))))

(defun f2-solve-linear-system! (matrix vector)
  "Solves Ax = b on GF(2) and returns a root if it exists. Otherwise it returns
NIL. In addition, this function returns the rank of A as the second value. This
function destructively modifies MATRIX and VECTOR.

The width of A must be multiple of 64."
  (declare (optimize (speed 3))
           ((simple-array bit (* *)) matrix)
           (simple-bit-vector vector))
  (destructuring-bind (m n) (array-dimensions matrix)
    (declare (optimize (safety 0))
             ((integer 0 #.most-positive-fixnum) m n))
    (multiple-value-bind (n/64 rem) (floor n 64)
      (assert (zerop rem))
      (let* ((storage (array-storage-vector matrix))
             (rank 0))
        (declare (fixnum rank))
        (dotimes (target-col n)
          (let* ((pivot-row (do ((i rank (+ 1 i)))
                                ((= i m) -1)
                              (unless (zerop (aref matrix i target-col))
                                (return i)))))
            (when (>= pivot-row 0)
              (let ((pivot-row/64 (floor (array-row-major-index matrix pivot-row 0) 64))
                    (rank-row/64 (floor (array-row-major-index matrix rank 0) 64)))
                ;; swap rows
                (rotatef (aref vector rank) (aref vector pivot-row))
                (loop
                  for k from 0 below n/64
                  do (rotatef (sb-kernel:%vector-raw-bits storage (+ rank-row/64 k))
                              (sb-kernel:%vector-raw-bits storage (+ pivot-row/64 k))))
                ;; eliminate the column
                (dotimes (i m)
                  (unless (or (= i rank) (zerop (aref matrix i target-col)))
                    (setf (aref vector i) (logxor (aref vector i) (aref vector rank)))
                    (loop
                      with base/64 = (floor (array-row-major-index matrix i 0) 64)
                      for k below n/64
                      do (setf (sb-kernel:%vector-raw-bits storage (+ base/64 k))
                               (logxor (sb-kernel:%vector-raw-bits storage (+ base/64 k))
                                       (sb-kernel:%vector-raw-bits storage (+ rank-row/64 k))))))))
              (incf rank))))
        (if (loop for i from rank below m
                  always (zerop (aref vector i)))
            (values vector rank)
            (values nil rank))))))
