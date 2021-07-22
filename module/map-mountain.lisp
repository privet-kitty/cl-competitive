(defpackage :cp/map-mountain
  (:use :cl)
  (:export #:map-mountain))
(in-package :cp/map-mountain)

;; not tested

;; NOTE: Use MAP-ALTERNATING-MONOTONE-SUBSEQ instead to deal with non-decreasing
;; or non-increasing subarray.
(declaim (inline map-mountain))
(defun map-mountain (function vector &key (order #'<))
  "Applies FUNCTION to each `increasing and then decreasing' subarray of
VECTOR. FUNCTION must take three arguments: index of left end, index of summit,
index of right end.

`Decreasing and then increasing' can also be realized by passing a descending
order (e.g. #'>) to ORDER. "
  (declare (vector vector))
  (let ((base-bottom 0)
        (base-peak 0)
        (up t)
        (n (length vector)))
    (unless (zerop n)
      (dotimes (i (- n 1))
        (if up
            (unless (funcall order (aref vector i) (aref vector (+ i 1)))
              (setq base-peak i)
              (setq up nil))
            (when (funcall order (aref vector i) (aref vector (+ i 1)))
              (funcall function base-bottom base-peak i)
              (setq base-bottom i)
              (setq up t))))
      (unless (< base-bottom base-peak)
        (setq base-peak (- n 1)))
      (funcall function base-bottom base-peak (- n 1)))))
