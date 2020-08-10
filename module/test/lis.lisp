(defpackage :cp/test/lis
  (:use :cl :fiveam :cp/lis)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/lis)
(in-suite base-suite)

(defun verify (original-vec lis lis-len order)
  (assert (= (length lis) lis-len))
  (let ((prev-pos -1))
    (loop for x across lis
          for next-pos = (position x original-vec :start (+ prev-pos 1))
          do (assert next-pos)
             (setq prev-pos next-pos))
    (loop for i from (+ 1 prev-pos) below (length original-vec)
          do (assert (not (funcall order
                                   (aref original-vec prev-pos)
                                   (aref original-vec i)))))
    (loop for i from 1 below lis-len
          do (assert (funcall order (aref lis (- i 1)) (aref lis i))))))

(test lis
  (declare (notinline calc-lis))
  ;; from https://www.geeksforgeeks.org/longest-increasing-subsequence-dp-3/
  (let ((vec #(10 22 9 33 21 50 41 60 80)))
    (multiple-value-bind (len lis) (calc-lis vec #'< most-positive-fixnum t)
      (is (= len 6))
      (verify vec lis len #'<))
    (multiple-value-bind (len lis) (calc-lis vec #'< most-positive-fixnum)
      (is (= len 6))
      (is (null lis))))
  ;; empty case
  (multiple-value-bind (len lis) (calc-lis #() #'> 1 t)
    (is (zerop len))
    (is (equalp #() lis)))
  ;; same elements
  (multiple-value-bind (len lis) (calc-lis #(5 5 5 5 5) #'< 1000 t)
    (is (= len 1))
    (is (equalp lis #(5))))
  (multiple-value-bind (len lis) (calc-lis #(5 5 5 5 5) #'<= 1000 t)
    (is (= len 5))
    (is (equalp lis #(5 5 5 5 5))))
  ;; random case
  (dotimes (i 200)
    (let ((vec (coerce (loop repeat 20 collect (- (random 10) 5)) 'vector)))
      (multiple-value-bind (len lis) (calc-lis vec #'< 100 t)
        (verify vec lis len #'<))
      (multiple-value-bind (len lis) (calc-lis vec #'<= 100 t)
        (verify vec lis len #'<=))
      (multiple-value-bind (len lis) (calc-lis vec #'> 100 t)
        (verify vec lis len #'>))
      (multiple-value-bind (len lis) (calc-lis vec #'>= 100 t)
        (verify vec lis len #'>=)))))
