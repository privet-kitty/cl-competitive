(defpackage :cp/enum-quotients
  (:use :cl)
  (:export #:enum-quotients #:map-quotients))
(in-package :cp/enum-quotients)

(declaim (inline %calc-length))
(defun %calc-length (x)
  (let ((sqrt (isqrt x)))
    (if (= (floor x sqrt) sqrt)
        (* 2 sqrt)
        (+ 1 (* 2 sqrt)))))

(declaim (ftype (function * (values (simple-array (integer 0 #.most-positive-fixnum) (*))
                                    &optional))
                enum-quotients))
(defun enum-quotients (n)
  "Given a positive integer N, floor(N/k) takes at most O(sqrt(N)) values for k
in {1, ..., N}. ENUM-QUOTIENTS returns the ascending vector of minimal integers
k that take `new' values compared to floor(N/(k-1)). Note that this vector
contains 1 and N+1.

Tips: Let A be the returned vector of length M+1 and an index i in {0, ..., M}
be given; then A[i]A[j] <= N iff j < M-i."
  (declare (optimize (speed 3))
           ((integer 1 #.most-positive-fixnum) n))
  (let ((k 1)
        (result (make-array (%calc-length n)
                            :element-type '(integer 0 #.most-positive-fixnum)))
        (end 0))
    (declare ((mod #.array-total-size-limit) end))
    (loop (setf (aref result end) k)
          (incf end)
          (when (> k n)
            (return result))
          (setq k (+ 1 (floor n (floor n k)))))))

(declaim (inline map-quotients))
(defun map-quotients (function n)
  (declare ((integer 0 #.most-positive-fixnum) n))
  (let ((k 1))
    (loop (funcall function k)
          (when (> k n)
            (return))
          (setq k (+ 1 (floor n (floor n k)))))))

