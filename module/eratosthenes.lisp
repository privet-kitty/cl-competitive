(defpackage :cp/eratosthenes
  (:use :cl)
  (:export #:make-prime-table #:make-prime-sequence #:prime-data #:make-prime-data
           #:prime-data-seq #:prime-data-table #:prime-data-p
           #:factorize #:make-omega-table))
(in-package :cp/eratosthenes)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (assert (= sb-vm:n-word-bits 64)))

(declaim (ftype (function * (values simple-bit-vector &optional)) make-prime-table))
(defun make-prime-table (length)
  "Returns a simple-bit-vector of LENGTH, whose (0-based) i-th bit is 1 if i is
prime and 0 otherwise.

Example: (make-prime-table 10) => #*0011010100"
  (declare (optimize (speed 3) (safety 0)))
  (check-type length (mod #.array-dimension-limit))
  (when (<= length 1)
    (return-from make-prime-table
      (make-array length :element-type 'bit :initial-element 0)))
  (let ((table (make-array length :element-type 'bit :initial-element 0)))
    ;; special treatment for p = 2
    (when (> length 2)
      (dotimes (i (ceiling length 64))
        (setf (sb-kernel:%vector-raw-bits table i) #xAAAAAAAAAAAAAAAA)))
    (setf (sbit table 1) 0
          (sbit table 2) 1)
    ;; p >= 3
    (loop for p from 3 to (+ 1 (isqrt (- length 1))) by 2
          when (= 1 (sbit table p))
          do (loop for composite from (* p p) below length by p
                   do (setf (sbit table composite) 0)))
    table))

(declaim (ftype (function * (values (simple-array (integer 0 #.most-positive-fixnum) (*))
                                    simple-bit-vector
                                    &optional))
                make-prime-sequence))
(defun make-prime-sequence (length)
  "Returns the ascending sequence of primes smaller than LENGTH. Internally
calls MAKE-PRIME-TABLE and returns its result as the second value."
  (declare (optimize (speed 3) (safety 0)))
  (check-type length (mod #.array-dimension-limit))
  (let* ((table (make-prime-table length))
         (pnumber (count 1 table))
         (result (make-array pnumber :element-type '(integer 0 #.most-positive-fixnum)))
         (index 0))
    (loop for x below length
          when (= 1 (sbit table x))
          do (setf (aref result index) x)
             (incf index))
    (values result table)))

(defstruct (prime-data (:constructor %make-prime-data (seq table))
                       (:copier nil))
  (seq nil :type (simple-array (integer 0 #.most-positive-fixnum) (*)))
  (table nil :type simple-bit-vector))

(defun make-prime-data (length)
  (multiple-value-call #'%make-prime-data (make-prime-sequence length)))

(declaim (inline factorize)
         (ftype (function * (values list list &optional)) factorize))
(defun factorize (x prime-data)
  "Returns the associative list of prime factors of X, which is composed
of (<prime> . <exponent>). E.g. (factorize 40 <prime-table>) => '((2 . 3) (5
. 1)).

- Any numbers beyond the range of PRIME-DATA are regarded as prime.
- The returned list is in ascending order w.r.t. prime factors."
  (declare (integer x))
  (let* ((x (abs x))
         (prime-seq (prime-data-seq prime-data))
         (result (load-time-value (list :root)))
         (tail result))
    (labels ((add (x)
               (setf (cdr tail) (list x)
                     tail (cdr tail))))
      (loop for prime of-type unsigned-byte across prime-seq
            until (= x 1)
            do (loop for exponent of-type (integer 0 #.most-positive-fixnum) from 0
                     do (multiple-value-bind (quot rem) (floor x prime)
                          (unless (zerop rem)
                            (when (> exponent 0)
                              (add (cons prime exponent)))
                            (return))
                          (setq x quot)))
            finally (unless (= x 1)
                      (add (cons x 1))))
      (multiple-value-prog1 (values (cdr result)
                                    (if (eq result tail) nil tail))
        (setf (cdr result) nil)))))

(defun make-omega-table (length prime-data)
  "Returns the table of prime omega function on {0, 1, ..., LENGTH-1}."
  (declare ((unsigned-byte 31) length))
  ;; (assert (>= (expt (aref prime-seq (- (length prime-seq) 1)) 2) (- length 1)))
  (let ((prime-seq (prime-data-seq prime-data))
        (table (make-array length :element-type '(unsigned-byte 31)))
        (res (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (i (length table))
      (setf (aref table i) i))
    (loop for p of-type (integer 0 #.most-positive-fixnum) across prime-seq
          do (loop for i from p below length by p
                   do (loop
                        (multiple-value-bind (quot rem) (floor (aref table i) p)
                          (unless (zerop rem)
                            (return))
                          (incf (aref res i))
                          (setf (aref table i) quot)))))
    (loop for i below length
          unless (= 1 (aref table i))
          do (incf (aref res i)))
    res))
