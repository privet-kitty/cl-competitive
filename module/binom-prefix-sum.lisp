(defpackage :cp/binom-prefix-sum
  (:use :cl :cp/binom-mod-prime :cp/static-mod)
  (:export #:make-binom-prefix-sums))
(in-package :cp/binom-prefix-sum)

(declaim (ftype (function * (values mint-vector &optional))
                make-binom-prefix-sums))
(defun make-binom-prefix-sums (length k)
  "Returns the sequence of T(0, k), T(1, k), ..., T(length-1, k), where T(n, k)
= binom(n, 0) + binom(n, 1) + ... + binom(n, k)."
  (declare (optimize (speed 3))
           (mint length k))
  (let ((res (make-array length :element-type 'mint :initial-element 0)))
    (when (> length 0)
      (setf (aref res 0) (mod 1 +mod+)))
    (loop for i from 1 below length
          do (setf (aref res i) (- (mod (* 2 (aref res (- i 1))) +mod+)
                                   (binom (- i 1) k))))
    res))
