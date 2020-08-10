(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../divisor.lisp")
  (load "set-equal"))

(use-package :test-util)

(with-test (:name make-divisors-table)
  (assert (equalp (make-divisors-table 13)
                  #(() (1) (1 2) (1 3) (1 2 4) (1 5) (1 2 3 6) (1 7) (1 2 4 8) (1 3 9)
                    (1 2 5 10) (1 11) (1 2 3 4 6 12))))
  (assert (equalp (make-divisors-table 0) #())))

(with-test (:name enum-ascending-divisors)
  (loop for x from 1 to 1000
        do (assert (equalp (loop for y from 1 to x
                                 when (zerop (mod x y))
                                 collect y)
                           (enum-ascending-divisors x)))))

(with-test (:name enum-divisors)
  (loop for x from 1 to 1000
        do (assert (set-equal (enum-ascending-divisors x)
                              (enum-divisors x)))))
