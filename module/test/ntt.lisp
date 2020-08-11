(defpackage :cp/test/ntt
  (:use :cl :fiveam :cp/ntt :cp/polynomial)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/ntt)
(in-suite base-suite)

(test ntt/manual
  (is (equalp #() (ntt-convolute #() #())))
  (is (equalp #(15) (ntt-convolute #(3) #(5))))
  (is (equalp #(998244308 17 2 998244348 1)
              (ntt-convolute #(5 998244350 1) #(998244344 998244351 1)))))

(defun make-random-polynomial (degree)
  (let ((res (make-array degree :element-type 'ntt-int :initial-element 0)))
    (dotimes (i degree res)
      (setf (aref res i) (random +ntt-mod+)))
    (let ((end (+ 1 (or (position 0 res :from-end t :test-not #'eql) -1))))
      (adjust-array res end))))

(test ntt/random
  (finishes
    (dotimes (_ 1000)
      (let* ((len1 (random 10))
             (len2 (random 10))
             (poly1 (make-random-polynomial len1))
             (poly2 (make-random-polynomial len2)))
        (assert (equalp (poly-mult poly1 poly2 +ntt-mod+)
                        (ntt-convolute poly1 poly2)))))))
