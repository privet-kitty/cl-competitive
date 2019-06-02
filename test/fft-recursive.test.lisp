(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../fft-recursive.lisp")
  (load "../nearly-equal.lisp"))

(use-package :test-util)

(defun to-fft-array (f)
  (let ((res (make-array (length f) :element-type '(complex fft-float))))
    (dotimes (i (length f))
      (setf (aref res i) (coerce (aref f i) '(complex double-float))))
    res))

(defun fft-array= (arr1 arr2)
  (every #'identity (map 'list (lambda (x y) (nearly= 1d-8 x y)) arr1 arr2)))

(with-test (:name fft-recursive)
  (assert (fft-array=
           (poly-multiply! (to-fft-array #(1 2 3 4 0 0 0 0))
		           (to-fft-array #(-1 -1 -1 -1 0 0 0 0)))
           #(-1 -3 -6 -10 -9 -7 -4 0))))
