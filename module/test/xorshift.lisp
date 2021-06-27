(defpackage :cp/test/xorshift
  (:use :cl :fiveam :cp/xorshift)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/xorshift)
(in-suite base-suite)

(test xorshift
  (let ((*test-dribble* nil))
    (is (= (progn (xorshift-seed! 123) (xorshift (ash 1 64)))
           (progn (xorshift-seed! 123) (xorshift (ash 1 64)))))
    (let ((counter (make-array 20 :element-type 'fixnum :initial-element 0)))
      (dotimes (_ 1000)
        (let ((x (randi 10 20)))
          (incf (aref counter x))))
      (loop for x below 10
            do (is (zerop (aref counter x))))
      (loop for x from 10 below 20
            do (is (<= (abs (- 100 (aref counter x))) 50))))
    (dotimes (_ 1000)
      (let ((x (randprob)))
        (is (<= 0 x))
        (is (< x 1))))))
