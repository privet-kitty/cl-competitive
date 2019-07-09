(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../binomial-coefficient-mod.lisp"))

(use-package :test-util)

(with-test (:name binom)
  (assert (= 0 (binom -1 1)))
  (assert (= 0 (binom 1 -2)))
  (assert (= 0 (binom 1 3)))
  (assert (= 1 (binom 0 0)))
  (assert (= 10 (binom 5 2)))
  (assert (= 962556519 (binom 1000 400))))
