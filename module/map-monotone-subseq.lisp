(defpackage :cp/map-monotone-subseq
  (:use :cl)
  (:export #:map-monotone-subseq #:map-alternating-monotone-subseq))
(in-package :cp/map-monotone-subseq)

;; not tested

;; TODO: integrate these two functions
(declaim (inline map-monotone-subseq))
(defun map-monotone-subseq (function vector order)
  "Applies FUNCTION to each maximal monotone subarray of VECTOR. FUNCTION must
take two arguments, L and R, such that the interval [L, R) corresponds to a
monotone subarray on VECTOR.

Definition: VECTOR[l], .., VECTOR[r-1] is monotone iff (FUNCALL ORDER VECTOR[i]
VECTOR[i+1]) always returns true."
  (declare (vector vector))
  (unless (zerop (length vector))
    (let ((prev 0))
      (declare ((mod #.array-dimension-limit) prev))
      (loop for i from 1 below (length vector)
            unless (funcall order (aref vector (- i 1)) (aref vector i))
            do (funcall function prev i)
               (setq prev i)
            finally (funcall function prev (length vector))))))

(declaim (inline map-alternating-monotone-subseq))
(defun map-alternating-monotone-subseq (function vector order)
  "Alternately applies FUNCTION to each increasing subarray and decreasing
one (if ORDER is #'<, for example). FUNCTION receives two arguments, L and R,
such that the **closed** interval [L, R] corresponds to a monotone subarray on
VECTOR.

`Decreasing and then increasing' can also be realized by passing a descending
order (e.g. #'>) to ORDER. Non-decreasing (#'<=) and non-increasing (#'>=) are
also available."
  (declare (vector vector))
  (let ((prev 0)
        (up t))
    (declare ((mod #.array-dimension-limit) prev))
    (unless (zerop (length vector))
      (loop for i from 1 below (length vector)
            do (if up
                   (unless (funcall order (aref vector (- i 1)) (aref vector i))
                     (funcall function prev (- i 1))
                     (setq prev (- i 1)
                           up nil))
                   (unless (funcall order (aref vector i) (aref vector (- i 1)))
                     (funcall function prev (- i 1))
                     (setq prev (- i 1)
                           up t)))
            finally (funcall function prev (- (length vector) 1))))))
