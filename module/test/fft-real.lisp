(defpackage :cp/test/fft-real
  (:use :cl :fiveam :cp/fft-real :cp/test/nearly-equal)
  (:import-from :cp/convolution)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/fft-real)
(in-suite base-suite)

(defun to-fft-array (f)
  (let ((res (make-array (length f) :element-type 'fft-float)))
    (dotimes (i (length f))
      (setf (aref res i) (coerce (aref f i) 'fft-float)))
    res))

(defun fft-array= (arr1 arr2 &optional (eps 1d-8))
  (every #'identity (map 'list (lambda (x y) (nearly= eps x y)) arr1 arr2)))

(test fft-real/hand
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

  (is 
   (convolve! (to-fft-array #(1 2 3 4 0 0 0 0))
              (to-fft-array #(-1 -1 -1 -1 0 0 0 0)))
   #(-1 -3 -6 -10 -9 -7 -4 0))

  (is 
   (fft-array=
    (convolve! (to-fft-array #(1 2 3 4 0 0 0 0 0 0 0 0 0 0 0 0))
               (to-fft-array #(-1 -1 -1 -1 0 0 0 0 0 0 0 0 0 0 0 0)))
    #(-1 -3 -6 -10 -9 -7 -4 0 0 0 0 0 0 0 0 0)))

  (is 
   (fft-array=
    (convolve (to-fft-array #(1 2 3 4)) (to-fft-array #(-1 -1 -1 -1)))
    #(-1 -3 -6 -10 -9 -7 -4)))

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

(test fft-real/random
  (let ((*test-dribble* nil)
        (*random-state* (sb-ext:seed-random-state 0)))
    (dotimes (len 200)
      (dotimes (_ 10)
        (let ((v1 (make-array (random (+ 1 len)) :element-type 'double-float))
              (v2 (make-array (random (+ 1 len)) :element-type 'double-float)))
          (dotimes (i (length v1))
            (setf (aref v1 i) (- (random 1000d0) 500d0)))
          (dotimes (i (length v2))
            (setf (aref v2 i) (- (random 1000d0) 500d0)))
          (is (fft-array= (convolve v1 v2)
                          (cp/convolution:convolve v1 v2 :identity+ 0d0)
                          1d-6)))))))
