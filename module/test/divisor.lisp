(defpackage :cp/test/divisor
  (:use :cl :fiveam :cp/divisor :cp/test/set-equal)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/divisor)
(in-suite base-suite)

(test make-divisors-table
  (is (equalp (make-divisors-table 13)
              #(() (1) (1 2) (1 3) (1 2 4) (1 5) (1 2 3 6) (1 7) (1 2 4 8) (1 3 9)
                (1 2 5 10) (1 11) (1 2 3 4 6 12))))
  (is (equalp (make-divisors-table 0) #())))

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
