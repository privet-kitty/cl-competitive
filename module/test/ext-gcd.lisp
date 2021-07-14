(defpackage :cp/test/ext-gcd
  (:use :cl :fiveam :cp/ext-gcd)
  (:import-from :sb-kernel #:%simple-fun-type)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/ext-gcd)
(in-suite base-suite)

(defun test1 (a b)
  (declare ((integer -3 5) a)
           ((integer -6 4) b))
  (ext-gcd a b))

(defun test2 (a b)
  (declare ((integer -3 5) a)
           ((integer * 4) b))
  (ext-gcd a b))

(test ext-gcd/type
  (is (equal (%simple-fun-type #'test1)
             '(function ((integer -3 5) (integer -6 4))
               (values (integer -5 5) (integer -5 5) &optional))))
  (is (equal (%simple-fun-type #'test2)
             '(function ((integer -3 5) (integer * 4))
               (values integer integer &optional)))))

(test ext-gcd/hand
  (is (equalp '(-9 47) (multiple-value-list (ext-gcd 240 46))))
  (is (equalp '(9 47) (multiple-value-list (ext-gcd -240 46))))
  (is (equalp '(-9 -47) (multiple-value-list (ext-gcd 240 -46))))
  (is (equalp '(9 -47) (multiple-value-list (ext-gcd -240 -46)))))

(test ext-gcd/random
  (let ((*test-dribble* nil)
        (*random-state* (sb-ext:seed-random-state 0)))
    (dotimes (_ 10000)
      (let ((a (- (random 1000) 500))
            (b (- (random 1000) 500)))
        (multiple-value-bind (x y) (ext-gcd a b)
          (is (= (+ (* a x) (* b y)) (gcd a b))))))
    (dotimes (_ 1000)
      (let ((a (- (random (expt 10 100))
                  (random (* 5 (expt 10 99)))))
            (b (- (random (expt 10 100))
                  (random (* 5 (expt 10 99))))))
        (multiple-value-bind (x y) (ext-gcd a b)
          (is (= (+ (* a x) (* b y)) (gcd a b))))))))
