(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../fft-real.lisp")
  (load "../nearly-equal.lisp"))

(use-package :test-util)

(defun to-fft-array (f)
  (let ((res (make-array (length f) :element-type 'fft-float)))
    (dotimes (i (length f))
      (setf (aref res i) (coerce (aref f i) 'fft-float)))
    res))

(defun fft-array= (arr1 arr2)
  (every #'identity (map 'list (lambda (x y) (nearly= 1d-8 x y)) arr1 arr2)))

(with-test (:name fft-real)
  (assert (fft-array=
           (convolute! (to-fft-array #(1 2 3 4 0 0 0 0))
		       (to-fft-array #(-1 -1 -1 -1 0 0 0 0)))
           #(-1 -3 -6 -10 -9 -7 -4 0)))

  ;; use RESULT-VECTOR argument
  (let ((res (make-array 8 :element-type 'fft-float)))
    (assert (fft-array=
             (convolute! (to-fft-array #(1 2 3 4 0 0 0 0))
		         (to-fft-array #(-1 -1 -1 -1 0 0 0 0))
                         res)
             #(-1 -3 -6 -10 -9 -7 -4 0)))
    (assert (eql res (convolute! (to-fft-array #(1 2 3 4 0 0 0 0))
		                 (to-fft-array #(-1 -1 -1 -1 0 0 0 0))
                                 res))))

  (assert (with-fixed-base 8
            (fft-array=
             (convolute! (to-fft-array #(1 2 3 4 0 0 0 0))
        	         (to-fft-array #(-1 -1 -1 -1 0 0 0 0)))
             #(-1 -3 -6 -10 -9 -7 -4 0))))

  (assert (with-fixed-base 16
            (fft-array=
             (convolute! (to-fft-array #(1 2 3 4 0 0 0 0 0 0 0 0 0 0 0 0))
        	         (to-fft-array #(-1 -1 -1 -1 0 0 0 0 0 0 0 0 0 0 0 0)))
             #(-1 -3 -6 -10 -9 -7 -4 0 0 0 0 0 0 0 0 0))))

  ;; base table doesn't have right length
  (signals simple-error
    (with-fixed-base 16 (dft! (to-fft-array #(0 0 0 0 0 0 0 0)))))

  ;; not power of two
  (signals simple-error (dft! (to-fft-array #(1 2 3 4 0 0 0))))
  (signals simple-error (inverse-dft! (to-fft-array #(1 2 3 4 0 0 0))))
  (signals simple-error (convolute! (to-fft-array #(1 2 3 4 0 0 0)) (to-fft-array #(1 2 3 4 0 0 0))))

  ;; boundary case
  (let ((zero (make-array 0 :element-type 'fft-float)))
    (assert (equalp #() (convolute! zero zero)))
    (assert (equalp #() (dft! zero)))
    (assert (equalp #() (inverse-dft! zero)))))

