(defpackage :cp/enum-quotients
  (:use :cl)
  (:export #:enum-quotients #:map-quotients))
(in-package :cp/enum-quotients)

(declaim (inline %calc-length))
(defun %calc-length (x)
  (let ((sqrt (isqrt x)))
    (if (and (> x 0) (= (floor x sqrt) sqrt))
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

Returns #(1) for convenience when N == 0.

Tips: Let A be the returned vector of length M+1 and an index i in {0, ..., M}
be given; then A[i]A[j] <= N iff j < M-i."
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) n))
  (let ((k 1)
        (result (make-array (%calc-length n)
                            :element-type '(integer 0 #.most-positive-fixnum)))
        (end 0))
    (declare ((mod #.array-dimension-limit) end))
    (loop (setf (aref result end) k)
          (when (> k n)
            (return result))
          (incf end)
          (setq k (+ 1 (floor n (floor n k)))))))

;; Another implementation:
;; (defun enum-quotients2 (n)
;;   (let ((sqrt (isqrt n))
;;         (res (make-array 0 :element-type 'fixnum :fill-pointer 0)))
;;     (loop for i from 1 below sqrt
;;           do (vector-push-extend i res))
;;     (vector-push-extend sqrt res)
;;     (unless (= sqrt (floor n sqrt))
;;       (vector-push-extend (+ 1 (floor n (+ sqrt 1))) res))
;;     (loop for i from (- sqrt 1) downto 1
;;           do (vector-push-extend (+ 1 (floor n (+ i 1))) res))
;;     res))

(declaim (inline map-quotients))
(defun map-quotients (function n)
  (declare ((integer 0 #.most-positive-fixnum) n))
  (let ((k 1))
    (loop (funcall function k)
          (when (> k n)
            (return))
          (setq k (+ 1 (floor n (floor n k)))))))
