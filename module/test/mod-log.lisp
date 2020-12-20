(defpackage :cp/test/mod-log
  (:use :cl :fiveam :cp/mod-log :cp/mod-power :cp/ext-gcd)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/mod-log)
(in-suite base-suite)

(defun naive-mod-log (x y modulus &key from-zero)
  (declare (optimize (speed 3))
           ((unsigned-byte 31) x y modulus))
  (loop for k from (if from-zero 0 1) to modulus
        when (= (mod-power x k modulus) (mod y modulus))
        do (return k)
        finally (return nil)))

(test mod-log/hand
  (is (= 8 (mod-log 6 4 44)))
  (is (= 8 (mod-log -38 -40 44)))
  (is (null (mod-log 6 2 44)))
  (is (= 2 (mod-log 8 4 12)))
  (is (= 4 (mod-log 3 13 17)))
  (is (= 1 (mod-log 12 0 4)))
  (is (= 2 (mod-log 12 0 8)))
  (is (null (mod-log 12 1 8)))
  (is (= 1 (mod-log 0 0 100)))
  (finishes
    (loop for x to 30
          do (loop for y to 30
                   do (loop for modulus from 1 to 30
                            do (assert (eql (mod-log x y modulus)
                                            (naive-mod-log x y modulus)))
                               (assert (eql (mod-log x y modulus :from-zero t)
                                            (naive-mod-log x y modulus :from-zero t))))))))

(test mod-log/random
  (let ((*num-trials* 100))
    (for-all ((a (gen-integer :min -10 :max 10))
              (b (gen-integer :min -10 :max 10)))
      (multiple-value-bind (x y) (ext-gcd a b)
        (is (= (+ (* a x) (* b y)) (gcd a b))))))
  
  (let ((*num-trials* 200))
    (for-all ((x (gen-integer :min 0 :max 1000))
              (y (gen-integer :min 0 :max 1000))
              (modulus (gen-integer :min 1 :max 1000)))
      (is (eql (mod-log x y modulus) (naive-mod-log x y modulus)))
      (is (eql (mod-log x y modulus :from-zero t)
               (naive-mod-log x y modulus :from-zero t))))))
