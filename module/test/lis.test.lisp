(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../lis.lisp"))

(use-package :test-util)

(declaim (notinline calc-lis))
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

(with-test (:name lis)
  ;; from https://www.geeksforgeeks.org/longest-increasing-subsequence-dp-3/
  (let ((vec #(10 22 9 33 21 50 41 60 80)))
    (multiple-value-bind (len lis) (calc-lis vec #'< most-positive-fixnum t)
      (assert (= len 6))
      (verify vec lis len #'<))
    (multiple-value-bind (len lis) (calc-lis vec #'< most-positive-fixnum)
      (assert (= len 6))
      (assert (null lis))))
  ;; empty case
  (multiple-value-bind (len lis) (calc-lis #() #'> 1 t)
    (assert (zerop len))
    (assert (equalp #() lis)))
  ;; same elements
  (multiple-value-bind (len lis) (calc-lis #(5 5 5 5 5) #'< 1000 t)
    (assert (= len 1))
    (assert (equalp lis #(5))))
  (multiple-value-bind (len lis) (calc-lis #(5 5 5 5 5) #'<= 1000 t)
    (assert (= len 5))
    (assert (equalp lis #(5 5 5 5 5))))
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
