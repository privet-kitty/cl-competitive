(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../power-mod.lisp"))

(use-package :test-util)

(with-test (:name power-mod)
  (dotimes (i 100)
    (assert (= (mod (expt -2 i) 998244353)
               (power-mod -2 i 998244353)))))
