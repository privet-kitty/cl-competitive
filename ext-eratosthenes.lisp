;;;
;;; Extended Eratosthenes' sieve (osa_k's method)
;;;
;;; build: O(n)
;;; With this sieve each prime factorization can be executed in O(log(n)), which
;;; is faster than the well known one in O(N/log(n) + log(n)).
;;; Reference: http://www.osak.jp/diary/diary_201310.html#20131017
;;;

(defun make-minfactor-table (sup)
  "Returns a vector of length SUP, whose (0-based) i-th value is the minimal
prime factor of i. (Corner case: 0th value is 0 and 1st value is 1.)"
  (declare (optimize (speed 3) (safety 0)))
  (check-type sup (integer 2 (#.array-total-size-limit)))
  (let ((table (make-array sup :element-type '(integer 0 #.most-positive-fixnum))))
    ;; initialize
    (dotimes (i sup) (setf (aref table i) i))
    ;; p = 2
    (loop for even-num from 4 below sup by 2
          do (setf (aref table even-num) 2))
    ;; p >= 3
    (loop for p from 3 to (+ 1 (isqrt (- sup 1))) by 2
          when (= p (aref table p))
          do (loop for composite from (* p p) below sup by p
                   when (= (aref table composite) composite)
                   do (setf (aref table composite) p)))
    table))

(declaim (inline factorize))
(defun factorize (x minfactor-table)
  "Returns the associative list of prime factors of X, which is composed
of (<prime> . <exponent>). E.g. (factorize 100 <minfactor-table>) => '((2 . 2) (5
. 5)).

MINFACTOR-TABLE := vector (MINFACTOR-TABLE[k] is the minimal prime factor of k)

Note that the returned list is NOT guaranteed to be in ascending order."
  (declare (fixnum x)
           (vector minfactor-table))
  (setq x (abs x))
  (when (<= x 1) (return-from factorize nil))
  (assert (< x (length minfactor-table)))
  (let (result)
    (loop until (= x 1)
          for prime of-type (integer 0 #.most-positive-fixnum) = (aref minfactor-table x)
          do (loop for exponent of-type (integer 0 #.most-positive-fixnum) from 0
                   do (multiple-value-bind (quot rem) (floor x prime)
                        (if (zerop rem)
                            (setf x quot)
                            (loop-finish)))
                   finally (push (cons prime exponent) result)))
    result))
