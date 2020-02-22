(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../mod-power.lisp"))

(use-package :test-util)

(with-test (:name mod-power)
  (dotimes (i 100)
    (assert (= (mod (expt -3 i) 998244353)
               (mod-power -3 i 998244353)))))
