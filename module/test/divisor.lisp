(defpackage :cp/test/divisor
  (:use :cl :fiveam :cp/divisor :cp/set-equal)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/divisor)
(in-suite base-suite)

(test enum-ascending-divisors
  (let ((*test-dribble* nil))
    (loop for x from 1 to 1000
          do (is (equalp (loop for y from 1 to x
                               when (zerop (mod x y))
                               collect y)
                         (enum-ascending-divisors x))))))

(test divisor
  (let ((*test-dribble* nil))
    (loop for x from 1 to 1000
          for set1 = (enum-ascending-divisors x)
          for set2 = (enum-divisors x)
          for set3 = nil
          do (map-divisors x
                           (lambda (d) (push d set3)))
          do (is (set-equal set1 set2))
             (is (set-equal set2 set3))
             (is (= (length set1) (length set2) (length set3))))))
