(defpackage :cp/linear-sieve
  (:use :cl)
  (:export #:make-minfactor-table #:factorize #:euler-phi)
  (:documentation
   "Provides linear sieve and prime factorization.

build: O(N)
query: O(log(n))

Reference:
https://cp-algorithms.com/algebra/prime-sieve-linear.html
https://37zigen.com/linear-sieve/ (Japanese)"))
(in-package :cp/linear-sieve)

;; limit the range of integers for efficiency
(deftype uint () '(unsigned-byte 31))

(declaim (ftype (function * (values uint &optional))
                %calc-pi-upper-bound))
(defun %calc-pi-upper-bound (x)
  "Computes an upper bound of prime-counting function.

Reference:
Pierre Dusart. Estimates of some functions over primes without R.H."
  (declare (uint x))
  (let* ((x (float x 1d0))
         (log (max 1d0 (log x))))
    (nth-value 0 (ceiling (* (/ x log) (+ 1d0 (/ 1.2762d0 log)))))))

(declaim (ftype (function * (values (simple-array uint (*))
                                    (simple-array uint (*))
                                    &optional))
                make-minfactor-table))
(defun make-minfactor-table (sup)
  "Returns a vector of length SUP, whose (0-based) i-th value is the smallest
prime factor of i. (Corner case: 0th value is 0 and 1st value is 1.) This
function returns an ascending vector of primes (less than SUP) as the second
value."
  (declare (optimize (speed 3) (safety 0)))
  (check-type sup (and uint (integer 2)))
  (let* ((table (make-array sup :element-type 'uint :initial-element 0))
         (ub (%calc-pi-upper-bound (- sup 1)))
         (primes (make-array ub :element-type 'uint))
         (end 0))
    (declare (uint end))
    (setf (aref table 1) 1)
    (loop for x from 2 below sup
          when (zerop (aref table x))
          do (setf (aref table x) x)
             (setf (aref primes end) x)
             (incf end)
          do (dotimes (i end)
               (let ((p (aref primes i)))
                 (when (or (>= (* p x) sup)
                           (> p (aref table x)))
                   (return))
                 (setf (aref table (* p x)) p))))
    (values table (adjust-array primes end))))

(declaim (inline factorize)
         (ftype (function * (values list &optional)) factorize))
(defun factorize (x minfactor-table)
  "Returns the associative list of prime factors of X, which is composed
of (<prime> . <exponent>). E.g. (factorize 100 <minfactor-table>) => '((2
. 2) (5 . 5)). The returned list is in ascending order
w.r.t. prime. Corner case: (factorize 0 table) => NIL.

MINFACTOR-TABLE := vector (MINFACTOR-TABLE[k] is the minimal prime factor of k)"
  (declare (fixnum x)
           (vector minfactor-table))
  (setq x (abs x))
  (when (<= x 1)
    (return-from factorize nil))
  (assert (< x (length minfactor-table)))
  (loop until (= x 1)
        for prime of-type (integer 0 #.most-positive-fixnum) = (aref minfactor-table x)
        collect (loop for exponent of-type (integer 0 #.most-positive-fixnum) from 0
                      do (multiple-value-bind (quot rem) (floor x prime)
                           (if (zerop rem)
                               (setf x quot)
                               (loop-finish)))
                      finally (return (cons prime exponent)))))

(declaim (inline euler-phi))
(defun euler-phi (x minfactor-table)
  (declare (fixnum x)
           (vector minfactor-table))
  (setq x (abs x))
  (assert (< x (length minfactor-table)))
  (let ((res x))
    (declare ((integer 0 #.most-positive-fixnum) res))
    (loop until (= x 1)
          for prime of-type (integer 0 #.most-positive-fixnum) = (aref minfactor-table x)
          do (decf res (floor res prime))
             (loop (multiple-value-bind (quot rem) (floor x prime)
                     (if (zerop rem)
                         (setq x quot)
                         (return)))))
    res))
