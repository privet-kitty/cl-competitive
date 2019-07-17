(declaim (ftype (function * (values simple-bit-vector &optional)) make-prime-table))
(defun make-prime-table (sup)
  "Returns a simple-bit-vector of length SUP, whose (0-based) i-th bit is 1 if i
is prime and 0 otherwise.

Example: (make-prime-table 10) => #*0011010100"
  (declare (optimize (speed 3) (safety 0)))
  (check-type sup (integer 2 (#.array-total-size-limit)))
  (let ((table (make-array sup :element-type 'bit :initial-element 0))
        (sup/64 (ceiling sup 64)))
    ;; special treatment for p = 2
    (dotimes (i sup/64)
      (setf (sb-kernel:%vector-raw-bits table i) #xAAAAAAAAAAAAAAAA))
    (setf (sbit table 1) 0
          (sbit table 2) 1)
    ;; p >= 3
    (loop for p from 3 to (+ 1 (isqrt (- sup 1))) by 2
          when (= 1 (sbit table p))
          do (loop for composite from (* p p) below sup by p
                   do (setf (sbit table composite) 0)))
    table))

;; FIXME: Currently the element type of the resultant vector is (UNSIGNED-BYTE 62).
(defun make-prime-sequence (sup)
  "Returns the ascending sequence of primes smaller than SUP."
  (declare (optimize (speed 3) (safety 0)))
  (check-type sup (integer 2 (#.array-total-size-limit)))
  (let ((table (make-prime-table sup)))
    (let* ((length (count 1 table))
           (result (make-array length :element-type '(integer 0 #.most-positive-fixnum)))
           (index 0))
      (declare ((integer 0 #.most-positive-fixnum) length))
      (loop for x below sup
            when (= 1 (sbit table x))
            do (setf (aref result index) x)
               (incf index))
      result)))

;; TODO: enable to take a list as PRIME-SEQ
(declaim (inline factorize)
         (ftype (function * (values list &optional)) factorize))
(defun factorize (x prime-seq)
  "Returns the associative list of prime factors of X, which is composed
of (<prime> . <exponent>). E.g. (factorize 100 <prime-table>) => '((2 . 2) (5
. 5)).

PRIME-SEQ := vector (composed only of not duplicated primes)
Note that the returned list is NOT guaranteed to be in ascending order."
  (declare (integer x)
           (vector prime-seq))
  (setq x (abs x))
  (when (<= x 1)
    (return-from factorize nil))
  (let (result)
    (loop for prime of-type unsigned-byte across prime-seq
          do (when (= x 1)
               (return-from factorize result))
             (loop for exponent of-type (integer 0 #.most-positive-fixnum) from 0
                   do (multiple-value-bind (quot rem) (floor x prime)
                        (if (zerop rem)
                            (setf x quot)
                            (progn
                              (when (> exponent 0)
                                (push (cons prime exponent) result))
                              (loop-finish))))))
    (if (= x 1)
        result
        (cons (cons x 1) result))))

(defun make-omega-table (sup prime-seq)
  "Returns the table of prime omega function on {0, 1, ..., SUP-1}."
  (declare ((integer 0 #.most-positive-fixnum) sup))
  ;; (assert (>= (expt (aref prime-seq (- (length prime-seq) 1)) 2) (- sup 1)))
  (let ((table (make-array sup :element-type '(unsigned-byte 32)))
        (res (make-array sup :element-type '(unsigned-byte 8))))
    (dotimes (i (length table))
      (setf (aref table i) i))
    (loop for p of-type (integer 0 #.most-positive-fixnum) across prime-seq
          do (loop for i from p below sup by p
                   do (loop
                        (multiple-value-bind (quot rem) (floor (aref table i) p)
                          (if (zerop rem)
                              (progn (incf (aref res i))
                                     (setf (aref table i) quot))
                              (return))))))
    (loop for i below sup
          unless (= 1 (aref table i))
          do (incf (aref res i)))
    res))
