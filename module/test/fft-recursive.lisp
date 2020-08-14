(defpackage :cp/test/fft-recursive
  (:use :cl :fiveam :cp/fft-recursive :cp/test/nearly-equal)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/fft-recursive)
(in-suite base-suite)

(defun to-fft-array (f)
  (let ((res (make-array (length f) :element-type '(complex fft-float))))
    (dotimes (i (length f))
      (setf (aref res i) (coerce (aref f i) '(complex double-float))))
    res))

(defun fft-array= (arr1 arr2)
  (every #'identity (map 'list (lambda (x y) (nearly= 1d-8 x y)) arr1 arr2)))

(test fft-recursive
  (is (fft-array=
       (convolute! (to-fft-array #(1 2 3 4 0 0 0 0))
                   (to-fft-array #(-1 -1 -1 -1 0 0 0 0)))
       #(-1 -3 -6 -10 -9 -7 -4 0))))
