(defpackage :cp/test/fft-real
  (:use :cl :fiveam :cp/fft-real :cp/test/nearly-equal)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/fft-real)
(in-suite base-suite)

(defun to-fft-array (f)
  (let ((res (make-array (length f) :element-type 'fft-float)))
    (dotimes (i (length f))
      (setf (aref res i) (coerce (aref f i) 'fft-float)))
    res))

(defun fft-array= (arr1 arr2)
  (every #'identity (map 'list (lambda (x y) (nearly= 1d-8 x y)) arr1 arr2)))

(test fft-real
  (is (fft-array= #(1) (convolve (to-fft-array #(1)) (to-fft-array #(1)))))
  (is (fft-array= #(1) (convolve! (to-fft-array #(1)) (to-fft-array #(1)))))
  (is (fft-array=
       (convolve! (to-fft-array #(1 2 3 4 0 0 0 0))
                  (to-fft-array #(-1 -1 -1 -1 0 0 0 0)))
       #(-1 -3 -6 -10 -9 -7 -4 0)))

  (is (fft-array= (convolve (to-fft-array #(1 2 3)) (to-fft-array #(3 1 -1 3)))
                  #(3 7 10 4 3 9)))

  ;; use RESULT-VECTOR argument
  (let ((res (make-array 8 :element-type 'fft-float)))
    (is (fft-array=
         (convolve! (to-fft-array #(1 2 3 4 0 0 0 0))
                    (to-fft-array #(-1 -1 -1 -1 0 0 0 0))
                    res)
         #(-1 -3 -6 -10 -9 -7 -4 0)))
    (is (eql res (convolve! (to-fft-array #(1 2 3 4 0 0 0 0))
                            (to-fft-array #(-1 -1 -1 -1 0 0 0 0))
                            res))))

  (with-fixed-length-fft 8
    (is 
     (fft-array=
      (convolve! (to-fft-array #(1 2 3 4 0 0 0 0))
                 (to-fft-array #(-1 -1 -1 -1 0 0 0 0)))
      #(-1 -3 -6 -10 -9 -7 -4 0))))

  (with-fixed-length-fft 16
    (is 
     (fft-array=
      (convolve! (to-fft-array #(1 2 3 4 0 0 0 0 0 0 0 0 0 0 0 0))
                 (to-fft-array #(-1 -1 -1 -1 0 0 0 0 0 0 0 0 0 0 0 0)))
      #(-1 -3 -6 -10 -9 -7 -4 0 0 0 0 0 0 0 0 0))))

  (with-fixed-length-fft 8
    (is 
     (fft-array=
      (convolve (to-fft-array #(1 2 3 4)) (to-fft-array #(-1 -1 -1 -1)))
      #(-1 -3 -6 -10 -9 -7 -4))))

  ;; base table doesn't have right length
  (signals simple-error
    (with-fixed-length-fft 16 (dft! (to-fft-array #(0 0 0 0 0 0 0 0)))))

  ;; not power of two
  (signals simple-error (dft! (to-fft-array #(1 2 3 4 0 0 0))))
  (signals simple-error (inverse-dft! (to-fft-array #(1 2 3 4 0 0 0))))
  (signals simple-error (convolve! (to-fft-array #(1 2 3 4 0 0 0))
                                   (to-fft-array #(1 2 3 4 0 0 0))))

  ;; boundary case
  (let ((zero (make-array 0 :element-type 'fft-float)))
    (is (equalp #() (convolve! zero zero)))
    (is (equalp #() (convolve zero zero)))
    (is (equalp #() (dft! zero)))
    (is (equalp #() (inverse-dft! zero)))))

