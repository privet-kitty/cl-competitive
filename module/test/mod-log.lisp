(defpackage :cp/test/mod-log
  (:use :cl :fiveam :cp/mod-log :cp/mod-power :cp/ext-gcd)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/mod-log)
(in-suite base-suite)

(defun naive-mod-log (x y modulus &key from-zero)
  (loop for k from (if from-zero 0 1) to modulus
        when (= (mod-power x k modulus) (mod y modulus))
        do (return k)
        finally (return nil)))

(test mod-log
  (let ((state (sb-ext:seed-random-state 0)))
    (dotimes (i 100)
      (let ((a (- (random 20 state) 10))
            (b (- (random 20 state) 10)))
        (multiple-value-bind (x y) (ext-gcd a b)
          (assert (= (+ (* a x) (* b y)) (gcd a b)))))))
  (assert (= 8 (mod-log 6 4 44)))
  (assert (= 8 (mod-log -38 -40 44)))
  (assert (null (mod-log 6 2 44)))
  (assert (= 2 (mod-log 8 4 12)))
  (assert (= 4 (mod-log 3 13 17)))
  (assert (= 1 (mod-log 12 0 4)))
  (assert (= 2 (mod-log 12 0 8)))
  (assert (null (mod-log 12 1 8)))
  (assert (= 1 (mod-log 0 0 100)))
  (loop for x to 30
        do (loop for y to 30
                 do (loop for modulus from 1 to 30
                          do (assert (eql (mod-log x y modulus)
                                          (naive-mod-log x y modulus)))
                             (assert (eql (mod-log x y modulus :from-zero t)
                                          (naive-mod-log x y modulus :from-zero t))))))
  (let ((state (sb-ext:seed-random-state 0)))
    (dotimes (_ 200)
      (let ((x (random 1000 state))
            (y (random 1000 state))
            (modulus (+ 1 (random 1000 state))))
        (assert (eql (mod-log x y modulus) (naive-mod-log x y modulus)))
        (assert (eql (mod-log x y modulus :from-zero t)
                     (naive-mod-log x y modulus :from-zero t)))))))
