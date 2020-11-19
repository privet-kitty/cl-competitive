(defpackage :cp/enum-quotients
  (:use :cl)
  (:export #:enum-quotients))
(in-package :cp/enum-quotients)

(declaim (ftype (function * (values (array (integer 0 #.most-positive-fixnum) (*)) &optional))
                enum-quotients))
(defun enum-quotients (n)
  "Given a positive integer N, floor(N/k) takes at most O(sqrt(N)) values for k
in {1, ..., N}. ENUM-QUOTIENTS returns the ascending vector of minimal integers
k that take `new' values compared to floor(N/(k-1)). Note that this vector
contains 1 and N+1.

Tips: Let A be the returned vector of length M+1 and an index i in {0, ..., M}
be given; then A[i]A[j] <= N iff j < M-i."
  (declare ((integer 0 #.most-positive-fixnum) n))
  (let ((k 1)
        (result (make-array (* 2 (isqrt n))
                            :element-type '(integer 0 #.most-positive-fixnum)
                            :adjustable t
                            :fill-pointer 0)))
    (loop (vector-push-extend k result)
          (when (> k n)
            (return result))
          (setq k (+ 1 (floor n (floor n k)))))))
